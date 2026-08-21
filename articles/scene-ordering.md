# How scene ordering works

The main vignette shows the pipeline as one chain. This one explains
what is actually happening underneath: why getting a scene takes several
steps, where scene lists fit, and what the download queue is doing while
you wait.

``` r

library(USGSm2m)
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

Each step has its own endpoint, and each depends on the one before:

       scene-search          which scenes exist
            │
            ▼
       scene-list-add        name that set server-side  ──►  listId
            │
            ▼
       download-options      what is downloadable       (requires listId)
            │
            ▼
       download-request      queue it                   ──►  label
            │
            ▼
       download-retrieve     collect URLs               (by label)
            │
            ▼
       HTTP GET              the actual bytes

In this package steps 1–3 are `$search()`, `$scene_list()` and
`$products()`, and steps 4–5 are `$request()` and `$retrieve()`.
`$products()` performs step 2 for you, which is why the scene list is
usually invisible.

## Scene lists

### What registering actually does

A scene list is not something the package holds in memory. It is a
**named set of scene ids stored on the USGS server**, created by posting
to `scene-list-add`:

    {
      "listId":      "USGSm2m_20260821150915_vbbem6",   <- a name you invent
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
ers_scene_list_add(session, "landsat_ot_c2_l2", scenes, list_id)
products <- ers_scene_products(session, "landsat_ot_c2_l2", list_id)
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

Internally it is still the same two calls — `$products()` asks
`$scene_list()` to register, then requests download options against the
resulting id. The identifier is generated (a timestamp plus six random
characters) so it cannot collide, and the dataset name is carried on the
object rather than retyped, so it cannot disagree.

### One registration per search

Repeated calls reuse the same list rather than registering the scenes
again:

``` r

found$products()
found$products()
found$scene_list()$list_id   # all three refer to one registration
```

A narrowed search is a different set of scenes, so it gets its own:

``` r

found$filter(cloudCover < 10)$scene_list()   # separate list
```

Passing an explicit `list_id` always registers under that name and
leaves the reused one alone, and a list you have deleted with
`$remove()` is registered afresh next time rather than being handed back
dead.

Note that nothing deletes these lists automatically — they persist until
they expire, or until you call `$remove()`.

### Working with the list directly

`M2MSceneList` is what you get back when scenes are registered.
`$products()` creates one and uses it without showing you, so most of
the time you never need it. `$scene_list()` gives you the same object to
hold onto:

``` r

found <- sess$dataset("landsat_ot_c2_l2")$search(
  temporal = filter_temporal("2020-07-01", "2020-07-31"),
  max_results = 4
)

scenes <- found$scene_list()
scenes
#> <M2MSceneList>
#>   List id: USGSm2m_20260821150915_vbbem6
#>   Dataset: landsat_ot_c2_l2
#>   Next:    $products()  |  $metadata()  |  $summary()
```

There are three reasons to reach for it.

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

**Naming a set to come back to.** The identifier is generated for you,
but you can supply your own and reattach to it later in the same
session:

``` r

found$scene_list(list_id = "july_2020_candidates")
sess$scene_list("july_2020_candidates")
```

Bear in mind that lists expire (see below), so this is useful within a
working session rather than across days.

`$remove()` deletes the list from the server when you are finished with
it.

### Combining several searches into one set

One search normally means one scene list, so you will not usually think
about what happens when scenes are added to a list that already exists.
The exception is when you want scenes from *more than one* search
treated as a single set — two date ranges, say, or two areas — so that
one `$products()` call and one download order covers all of them.

Give the searches the same `list_id` and they accumulate:

``` r

ds <- sess$dataset("landsat_ot_c2_l2")

july <- ds$search(temporal = filter_temporal("2020-07-01", "2020-07-05"), max_results = 2)
sept <- ds$search(temporal = filter_temporal("2020-09-01", "2020-09-05"), max_results = 2)

july$scene_list(list_id = "summer_2020")
scenes <- sept$scene_list(list_id = "summer_2020")

scenes$scenes()      # all four scenes
scenes$products()    # products for the whole set
```

Adding **appends** rather than replacing, which is what makes this work,
and re-adding a scene already in the list is a **no-op**, so searches
that overlap will not double-count.

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

again <- sess$scene_list("USGSm2m_20260821150915_vbbem6")
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
opts$select_products("Product Bundle")$filter(available) # 1 .tar per scene
```

`$selected()` always shows what `$request()` will queue. Neither
selector filters on availability, because the API lists superseded
products with `available = FALSE` and silently dropping them would hide
that they exist.

## The download queue

`$request()` places an order under a label. What comes back depends on
whether the distribution system can serve the products immediately:

- Products already staged return **with URLs**, in `$available`.
- Anything needing preparation is accepted and reported in `$requested`,
  with no URL yet.

`$is_ready()` tells you which situation you are in. `$refresh()`
re-polls and adds newly ready files to `$available`:

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

`sess$downloads()` lists every individual download across all labels,
and `queue$summary()` breaks one order down by dataset.

## What lives where

| Thing | Identified by | Survives your session? | Created by |
|----|----|----|----|
| Session | API token | No — expires when idle | [`m2m_session()`](https://stevenpawley.github.io/USGSm2m/reference/m2m_session.md) |
| Scene list | `listId` | Usually not — expires when idle | `$scene_list()`, or implicitly by `$products()` |
| Download order | `label` | Yes | `$request()` |
| Downloaded files | path on disk | Yes | `$retrieve()` |

The practical consequence: give orders labels you will recognise later,
and do not expect a scene list to still be there tomorrow.
