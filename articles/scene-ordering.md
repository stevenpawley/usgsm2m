# How scene ordering works

The main vignette shows the pipeline as one chain. This one explains
what is actually happening underneath: why getting a scene takes several
steps, where scene lists fit, and what the download queue is doing while
you wait.

``` r

library(usgsm2m)
```

## Why it takes more than one call

There is no single “download this scene” endpoint. Four separate things
have to be established before the API will hand over a file:

1.  **Which scenes** you mean — found by searching a dataset.
2.  **A server-side name for that set** — a *scene list*, because the
    endpoints in step 3 will not accept scene ids inline.
3.  **Which products** of those scenes you want — a scene is not one
    file but a bundle, its individual bands, and browse images.
4.  **A prepared download** — the distribution system may have to stage
    a product before there is a URL to fetch.

Each step has its own endpoint, and each produces exactly the one thing
the next step needs:

      scene-search        which scenes match
           │
           │  entityIds
           ▼
      scene-list-add      register that set server-side
           │
           │  listId
           ▼
      download-options    what is downloadable for those scenes
           │
           │  entityId + productId pairs
           ▼
      download-request    place the order
           │
           │  label
           ▼
      download-retrieve   collect prepared URLs
           │
           │  urls
           ▼
      HTTP GET            the bytes

Follow the labels down the middle and the whole thing is a chain of
handoffs: a search gives you scene ids, registering them gives you a
`listId`, that gives you product ids, ordering those gives you a
`label`, and the label eventually gives you URLs.

Each endpoint maps onto one method:

| API endpoint | Package | Needs | Gives you |
|----|----|----|----|
| `scene-search` | `$search()` | filters | `M2MSceneSearch` — `$scenes`, `$total_hits` |
| `scene-list-add` | internal to `$products()` | scenes, dataset | a temporary server-side `listId` |
| `download-options` | `$products()` | `listId`, dataset | `M2MDownloadOptions` — `$bands()`, `$scene_products()` |
| `download-request` | `$request(label)` | entityId/productId pairs | `M2MDownloadQueue` — `$ready()`, `$pending()` |
| `download-retrieve` | `$refresh()` | `label` | updates the queue in place |
| (plain HTTP GET) | `$retrieve(dir)` | URLs | tibble of `path` and `status` per file |

Two of these you will rarely call yourself. `$products()` performs
`scene-list-add` as well as `download-options`, which is why the scene
list is usually invisible, and `$request()` already returns whatever was
available immediately, so `$refresh()` only matters when something
needed staging.

## Scene lists

### What registering actually does

A scene list is not something the package holds in memory. It is a
**named set of scene ids stored on the USGS server**, created by posting
to `scene-list-add`:

    {
      "listId":      "usgsm2m_20260821150915_vbbem6",   <- a name you invent
      "idField":     "entityId",
      "entityIds":   ["LC80430322020199LGN00", ...],
      "datasetName": "landsat_ot_c2_l2"
    }

You choose the identifier, the server stores the set under it, and the
call reports how many scenes the list now holds. From then on that id
stands in for the whole set. It is closer to uploading a temporary table
than to creating an object locally.

`download-options` and `scene-metadata-list` will not accept scene ids
inline — only a `listId`. For a search returning thousands of scenes
that avoids re-sending every id on each call, but it means even a
single-scene download has to go through the same ceremony.

### What the package does for you

Written out by hand, the registration step looks like this: invent a
unique id, add the scenes under it, then pass the same id *and the same
dataset name* to the next call.

``` r

list_id <- "my_unique_id_12345"
api_scene_list_add(session, "landsat_ot_c2_l2", scenes, list_id)
products <- api_scene_products(session, "landsat_ot_c2_l2", list_id)
```

Three things have to stay consistent there, and each fails in its own
way: a reused identifier silently appends to an existing set, a
mismatched `datasetName` fails obscurely, and the id has to be threaded
between the calls.

`$products()` collapses all of that into one line, because the search
object already knows both the scenes and the dataset:

``` r

found$products()
```

Internally it is still the same two calls: `$products()` privately
registers the scenes, then requests download options against the
resulting id. The identifier is generated automatically, and the dataset
name is carried on the search object rather than retyped.

### One registration per search

Repeated calls reuse the same list rather than registering the scenes
again:

``` r

found$products()
found$products()
```

A narrowed search is a different set of scenes, so its `$products()`
call gets its own registration:

``` r

found$filter(cloudCover < 10)$products()
```

The registration is deliberately private because it is API plumbing
rather than a workflow choice. Temporary lists persist until the server
expires them.

### Reattaching to an existing list

`M2MSceneList` remains available for an existing server-side list whose
id you already know. This is an advanced recovery/interoperability path;
searches do not expose the temporary lists they create:

``` r

scenes <- sess$scene_list(
  "an_existing_list_id",
  dataset_name = "landsat_ot_c2_l2"
)
scenes
#> <M2MSceneList>
#>   List id: an_existing_list_id
#>   Dataset: landsat_ot_c2_l2
#>   Next:    $products()  |  $metadata()  |  $summary()
```

An attached list can expose bulk metadata and describe its server-side
set.

**Bulk metadata.** `$metadata()` fetches the full metadata for every
scene in the list in a single call. Done scene by scene that would be
one request each, so this is by far the most common reason to want the
list:

``` r

scenes$metadata()
```

**Describing the set.** `$summary()` reports the combined spatial and
temporal extent of everything in the list, and `$scenes()` returns the
entity ids it currently holds:

``` r

scenes$summary()
scenes$scenes()
```

To gather scenes from several searches, use `$combine()` before
`$products()`; the package will register the combined set privately.

### Combining several searches into one set

Sometimes one query cannot express what you want — two separate date
ranges, say, or two areas — but you still want a single `$products()`
call and a single download order to cover all of it. `$combine()` merges
searches:

``` r

ds <- sess$dataset("landsat_ot_c2_l2")

july <- ds$search(temporal = filter_temporal("2020-07-01", "2020-07-05"))
sept <- ds$search(temporal = filter_temporal("2020-09-01", "2020-09-05"))

summer <- july$combine(sept)
summer
#> <M2MSceneSearch>
#>   Dataset: landsat_ot_c2_l2
#>   Scenes:  4 of 4 total hits
#>   Next:    $products()  |  $filter(...)

summer$products()
```

Like `$filter()`, it returns a new search and leaves the originals
untouched. It takes several at once and chains, so
`july$combine(sept, oct)` and `july$combine(sept)$combine(oct)` are the
same thing.

A scene found by more than one search is kept once, so overlapping
searches do not double-count. All the searches have to be of the same
dataset, since products are discovered per dataset — combining across
datasets raises an error rather than producing a set whose `$products()`
would be wrong.

Note that `$total_hits` on a combined search is simply how many scenes
it holds. The API’s per-query hit counts describe the individual queries
and cannot be added up without double-counting, and a combined set is
not something the API can page through.

### Lifetime

Scene lists are temporary. They expire server-side after a period of
inactivity, and `$summary()` reports a `listTimeout` for lists that
carry one. Nothing breaks when a list expires — it simply stops being
found, and `$scenes()` returns zero rows rather than raising an error.
The same is true after you delete one with `$remove()`.

This matters when resuming work later: **a scene list will usually be
gone by the next session, but the download order it produced will not.**
Reconnect to orders by label, not to scene lists by id.

### Reattaching to a list

If you reattach to a list by id, the package does not initially know
which dataset it holds — the API only reveals that through the list’s
summary — so `$dataset()` resolves it on demand:

``` r

again <- sess$scene_list("usgsm2m_20260821150915_vbbem6")
again
#>   Dataset: <unresolved>
again$dataset()
#> [1] "landsat_ot_c2_l2"
```

Pass `dataset_name` to `$scene_list()` to skip that lookup, or to choose
when a list spans more than one dataset.

## Choosing products

A scene is not a file. `download-options` reports several *products* per
scene, at two different granularities:

- **Whole products** — a Level-1 Product Bundle (one `.tar` containing
  the entire scene), or a full-resolution browse image. Listed by
  `$scene_products()`.
- **Individual files** — the separate bands and metadata files nested
  inside a bundle. Listed by `$bands()`.

For one Landsat Collection 2 scene that is roughly ten whole products
and nineteen individual files. Browse images have no constituent files,
so they appear only in `$scene_products()`.

Pick a granularity with the matching selector, then narrow with
`$filter()`:

``` r

opts <- found$products()

opts$select_bands(c("B4", "B5"))$filter(bulkAvailable)   # 6 separate .TIFs
opts$select_products("Landsat Collection 2 Level-2 Product Bundle")$
  filter(available)                                      # 1 .tar per scene
```

`$selected()` always shows what `$request()` will queue. Neither
selector filters on availability, because the API lists superseded
products with `available = FALSE` and silently dropping them would hide
that they exist.

## The download queue

`$request()` places an order under a label. A file is ready when it has
a URL, and how it gets one depends on how USGS serves that product:

- **Served by M2M itself** — appears in the API’s available bucket with
  a URL.
- **Proxied** — appears in the API’s requested bucket, but *also* with a
  URL. These are hosted elsewhere (`landsatlook.usgs.gov`, for instance)
  on a signed URL, so M2M hands you the link and steps out of the way.
  They are immediately downloadable.
- **Still staging** — appears in the requested bucket with no URL yet.

Because the API’s two buckets do not line up with ready-versus-pending,
the queue keeps them private and exposes `$ready()` and `$pending()`
instead. `$is_ready()` is true when nothing is left staging, and
`$refresh()` re-polls:

``` r

queue <- opts$select_bands(c("B4", "B5"))$request(label = "ndvi_july_2020")

while (!queue$is_ready()) {
  Sys.sleep(30)
  queue$refresh()
}

queue$retrieve("data/landsat")
```

`$refresh()` accumulates rather than replaces, because the API drops
entries once it has handed them over — a plain replacement would lose
URLs collected on an earlier poll.

Some orders are staged rather than queued, and need `$prepare()` to
start processing. Along with `$cancel()`, it is one of the two methods
here that change server-side state.

## Coming back later

Orders outlive both your R session and the scene list that produced
them, so a large download does not have to finish in one sitting:

``` r

sess <- m2m_session()

sess$download_labels()      # which orders exist
queue <- sess$download_queue("ndvi_july_2020")
queue$refresh()
queue$retrieve("data/landsat")
```

`sess$downloads()` lists every individual download across all labels.

`$retrieve()` leaves alone any file already sitting in the output
directory under the name the server would give it, so re-running it
after an interrupted session only fetches what is missing; those files
come back with a status of `"skipped"`. Pass `overwrite = TRUE` to fetch
them again anyway.

## What lives where

| Thing | Identified by | Survives your session? | Created by |
|----|----|----|----|
| Session | API token | No — expires when idle | [`m2m_session()`](https://stevenpawley.github.io/usgsm2m/reference/m2m_session.md) |
| Scene list | `listId` | Usually not — expires when idle | privately by `$products()` |
| Download order | `label` | Yes | `$request()` |
| Downloaded files | path on disk | Yes | `$retrieve()` |

The practical consequence: give orders labels you will recognise later,
and do not expect a scene list to still be there tomorrow.
