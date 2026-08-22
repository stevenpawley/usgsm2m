# USGSm2m (development version)

## New

* `M2MSession$remove_items()` and `M2MDownloadQueue$remove_items()` remove
  individual downloads from the queue, where `M2MDownloadQueue$cancel()` only
  drops a whole order. Ids come from the `downloadId` column of
  `$downloads()`, `$ready()` or `$pending()`.

  The API removes one item per request and answers the same way for an id
  that does not exist, so a successful call is not evidence that anything was
  removed - check `$downloads()` afterwards.

## Bug fixes

* Proxied downloads could not be retrieved at all. USGS serves some products
  from another host (`landsatlook.usgs.gov`, for instance) on a signed URL,
  and lists those under `$requested` even though they carry a working URL.
  The queue treated everything in `$requested` as still being prepared, so
  `$is_ready()` never became `TRUE` and `$retrieve()` refused with "No
  downloads are available yet". Readiness is now a matter of having a URL,
  via the new `$ready()` and `$pending()` methods.
* The M2M session token was sent to whichever host a download URL pointed at.
  Proxied URLs are self-authenticating and ignore it, so this handed the token
  to hosts that neither needed nor asked for it. It is now sent only to the
  M2M API's own host.
* Proxied downloads are now reported back through `download-complete-proxied`
  once fetched. The API does not serve them itself and so cannot observe the
  transfer; without this they stay in the download queue indefinitely.
* An expired signed URL is reported as `status = "expired"` rather than a bare
  failure, since the fix is to `$refresh()` and retry.

# USGSm2m 0.2.0

The package is now organised around R6 objects rather than a flat set of
functions. **This is a breaking change**: every exported `ers_*` function has
been removed. See "Migrating from 0.1.0" below.

## The pipeline

Getting data out of the M2M API takes several steps that have to happen in a
particular order. That order is now expressed by the objects themselves, each
stage returning an object whose methods are the valid next steps:

```
M2MSession → M2MDataset → M2MSceneSearch → M2MDownloadOptions → M2MDownloadQueue
```

Tab-completing `$` shows only what you can legitimately do from where you are,
and every object prints a `Next:` hint. A whole download now reads as one
chain:

```r
sess <- m2m_session()

queue <- sess$
  dataset("landsat_ot_c2_l2")$
  search(
    spatial  = filter_spatial(ll_lon = -120, ll_lat = 40, ur_lon = -119.5, ur_lat = 40.5),
    temporal = filter_temporal("2020-07-01", "2020-07-31"),
    cloud    = filter_cloud(0, 50)
  )$
  products()$
  select_bands(c("B4", "B5"))$
  filter(bulkAvailable)$
  request(label = "ndvi_july_2020")

queue$retrieve("data/")
```

Scene lists are now created for you. The M2M API requires one server-side
before download options can be listed; `$products()` handles that, where
previously you had to wire `ers_scene_list_add()` and `ers_scene_products()`
together by hand and pass a `list_id` between them.

## Migrating from 0.1.0

| 0.1.0 | 0.2.0 |
|---|---|
| `ers_session()` | `m2m_session()` |
| `ers_logout(s)` | `s$logout()` |
| `ers_dataset(s, dataset_name = x)` | `s$dataset(x)` |
| `ers_dataset_search(s, x)` | `s$find_datasets(x)` |
| `ers_dataset_filters(s, x)` | `s$dataset(x)$filters()` |
| `ers_scene_search(s, x, ...)` | `s$dataset(x)$search(...)` |
| `ers_scene_list_add(s, x, scenes, id)` | `search$scene_list()` |
| `ers_scene_list_get(s, id)` | `list$scenes()` |
| `ers_scene_list_summary(s, id)` | `list$summary()` |
| `ers_scene_list_remove(s, id)` | `list$remove()` |
| `ers_scene_products(s, x, id, ...)` | `search$products()` |
| `ers_download_request(s, p, label = l)` | `products$request(label = l)` |
| `ers_download_queue(s, label = l)` | `s$download_queue(l)` |
| `ers_download_search(s)` | `s$downloads()` |
| `ers_download_retrieve(s, r, l, dir)` | `queue$retrieve(dir)` |
| `ers_download_eula(s, code)` | `s$eula(code)` |
| `ers_download_remove_order(s, l)` | `queue$cancel()` |
| `ers_download_remove_items(s, ids)` | (no direct equivalent; use `queue$cancel()`) |

`filter_spatial()`, `filter_temporal()` and `filter_cloud()` are unchanged.

The `filter()` S3 method for `scene_products` objects has been removed;
`M2MDownloadOptions$filter()` replaces it.

## New

* `filter_metadata_value()`, `filter_metadata_between()`,
  `filter_metadata_and()` and `filter_metadata_or()` build metadata filters
  from the field ids that `M2MDataset$filters()` reports. Those ids could be
  listed in 0.1.0 but there was nothing to turn them into a filter with.
* `M2MDownloadOptions$scene_products()` and `$select_products()` expose whole
  products — a Level-1 Product Bundle (one `.tar` per scene) or a
  full-resolution browse image — alongside the individual band files that
  `$bands()` lists. Browse products have no constituent files and so were
  previously unreachable entirely.
* `M2MDataset$related_scenes()` wraps `scene-search-secondary`. Note that few
  datasets define a secondary relationship; the rest raise `DATASET_ERROR`.
* `M2MSession$download_labels()` lists your orders, so an order can be found
  again without remembering its label.
* `M2MDownloadQueue$summary()` breaks an order down by dataset, and
  `$prepare()` moves a staged order into the queue for processing.
* `M2MSceneList$dataset()` reports which dataset a scene list holds, looking
  it up from the list's summary when reattaching to a list by id and caching
  the answer. `M2MSession$scene_list()` also takes an optional `dataset_name`
  to skip that lookup, or to choose one when a list spans several datasets.
* A usage vignette, `vignette("USGSm2m")`.

## Errors

API failures now raise conditions of class `m2m_error`, with
`m2m_http_error`, `m2m_api_error` and `m2m_not_found` subclasses, instead of
printing a message and returning `NULL`. A failed step stops the chain where
it failed rather than surfacing later as an unrelated error:

```r
tryCatch(
  sess$dataset("not_a_real_dataset"),
  m2m_not_found = function(e) message("No such dataset")
)
```

## Bug fixes

Seven data-coercion bugs were found and fixed by testing against the live API
across 20+ datasets. Several produced plausible but wrong data rather than
failing, so results obtained with 0.1.0 are worth re-checking:

* Scene metadata was reduced to a single row of deparsed R code
  (`"c(\"5e83d150...\", ...)"`) whenever a search spanned scenes with
  differing metadata field counts — the common case in any multi-scene search.
  Uniform searches took a different code path and worked, which hid this.
* Every file was queued four times. The API repeats a scene's
  `secondaryDownloads` under each of its product entries, and flattening them
  did not de-duplicate.
* `download-options` failed outright on any dataset mixing products that have
  bands with products that do not, affecting `landsat_tm_c2_l1`,
  `landsat_mss_c2_l1` and `eo1_ali_pub` among others.
* Dataset searches repeated a dataset once per catalog it belongs to. The
  same unnesting also mishandled null fields, which could drop a dataset's row
  or fail outright when it had nothing else to expand against.
* `dataset-filters` repeated a Select field once per permitted value, giving
  22 rows for 12 fields.
* `scene-list-add` returned HTTP 500 for any single-scene list: JSON
  serialisation turned a length-1 `entityIds` vector into a bare string
  instead of an array. The same fault is fixed for `eulaCodes`.
* `scene-list-summary` returned a column set that varied with the data —
  `listTimeout` vanished when null, `invalidSceneCount` was never returned,
  and `invalidScenes` changed type depending on whether any existed. An
  unknown or expired list also raised a confusing "Column `coordinates`
  doesn't exist" from dplyr, rather than summarizing as empty.

# USGSm2m 0.1.0

* Initial version.
