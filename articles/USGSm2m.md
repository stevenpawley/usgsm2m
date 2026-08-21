# Searching for and downloading USGS/EROS data

The USGS/EROS Machine-to-Machine (M2M) API provides programmatic access
to the same catalogue that Earth Explorer serves, covering several
hundred datasets. This vignette walks through the full path from logging
in to having files on disk.

``` r

library(USGSm2m)
```

## How the package is organised

Getting data out of the M2M API takes several steps, and they have to
happen in a particular order: you cannot list a scene’s products before
you have searched for the scene, and you cannot download anything before
you have requested it.

Rather than leaving that order for you to remember, the package models
it as a chain of objects. Each step returns an object whose methods are
the steps that are valid next:

    M2MSession → M2MDataset → M2MSceneSearch → M2MDownloadOptions → M2MDownloadQueue

Two practical consequences: pressing Tab after `$` shows only what you
can legitimately do from where you are, and every object prints a
`Next:` line reminding you where to go.

## Authenticating

Authentication needs your ERS username and an M2M **application token**.
The token is not your Earth Explorer password; generate one from your
profile at <https://ers.cr.usgs.gov/>. Access to the M2M API itself must
be requested and granted before any of this will work.

Keeping credentials out of your scripts is worth the small effort —
store them in `~/.Renviron` via `usethis::edit_r_environ()`:

    M2M_USERNAME=your_username
    M2M_TOKEN=your_application_token

[`m2m_session()`](https://stevenpawley.github.io/USGSm2m/reference/m2m_session.md)
reads those two variables by default:

``` r

sess <- m2m_session()
sess
#> <M2MSession>
#>   User:    your_username
#>   Status:  connected
#>   Next:    $dataset(name)  |  $find_datasets(pattern)
```

Sessions expire after a period of inactivity. Call `sess$logout()` when
you are finished to release it immediately.

## Finding a dataset

Datasets are identified by an alias such as `landsat_ot_c2_l2`. If you
do not know the alias, search the catalogue for a name pattern —
wildcards are applied automatically:

``` r

sess$find_datasets("landsat_ot")
#> # A tibble: 2 × 21
#>   datasetAlias     collectionName             sceneCount ...
#>   <chr>            <chr>                           <int>
#> 1 landsat_ot_c2_l1 Landsat 8-9 OLI/TIRS C2 L1    4791194
#> 2 landsat_ot_c2_l2 Landsat 8-9 OLI/TIRS C2 L2    4132193
```

Once you know which one you want, pick it up by alias:

``` r

ds <- sess$dataset("landsat_ot_c2_l2")
ds
#> <M2MDataset>
#>   Alias:   landsat_ot_c2_l2
#>   Name:    Landsat 8-9 OLI/TIRS C2 L2
#>   Scenes:  4,132,193
#>   Next:    $search(...)  |  $filters()
```

The full metadata record is in `ds$info`, a one-row tibble.

## Searching for scenes

`$search()` takes any combination of a spatial, temporal, cloud cover,
and metadata filter. The `filter_*()` helpers build them:

``` r

found <- ds$search(
  spatial  = filter_spatial(ll_lon = -120, ll_lat = 40, ur_lon = -119.5, ur_lat = 40.5),
  temporal = filter_temporal("2020-07-01", "2020-07-31"),
  cloud    = filter_cloud(min = 0, max = 50)
)
found
#> <M2MSceneSearch>
#>   Dataset: landsat_ot_c2_l2
#>   Scenes:  3 of 3 total hits
#>   Next:    $products()  |  $filter(...)  |  $scene_list()
```

Results are in the `$scenes` tibble. Searches are paged through
exhaustively by default; pass `max_results` to stop early. `$total_hits`
always reports what the API matched overall, so comparing it against
`nrow(found$scenes)` tells you whether you are looking at a complete
result set:

``` r

found$scenes
found$total_hits
```

`$filter()` narrows the scenes locally, using
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
semantics against the `$scenes` tibble. No further API call is made:

``` r

recent <- found$filter(cloudCover < 10)
recent$scenes
```

### Dataset-specific filters

Beyond the common filters, each dataset exposes its own searchable
metadata fields. `$filters()` lists them, one row per field:

``` r

ds$filters()
#> # A tibble: 12 × 7
#>    id               fieldLabel                    valueList  ...
#>    <chr>            <chr>                         <list>
#>  1 5e83d14f567d0086 Landsat Product Identifier L2 <NULL>
#>  4 5e83d14fb9436d88 WRS Path                      <NULL>
#>  5 5e83d14ff1eda1b8 WRS Row                       <NULL>
#>  6 61af9273566bb9a8 Satellite                     <list [3]>
```

The `id` column gives the `filterId` that the `filter_metadata_*()`
builders take. For fields of type `Select`, `valueList` holds the
permitted values.

Searching a specific Landsat path/row, for instance:

``` r

ds$search(
  temporal = filter_temporal("2020-07-01", "2020-07-31"),
  metadata = filter_metadata_and(
    filter_metadata_value("5e83d14fb9436d88", "043"),  # WRS Path
    filter_metadata_value("5e83d14ff1eda1b8", "032")   # WRS Row
  )
)
```

[`filter_metadata_between()`](https://stevenpawley.github.io/USGSm2m/reference/filter_metadata_between.md)
covers ranges, and
[`filter_metadata_or()`](https://stevenpawley.github.io/USGSm2m/reference/filter_metadata_and.md)
combines alternatives.

## Choosing what to download

`$products()` asks the API what is actually downloadable for the scenes
you found:

``` r

opts <- found$products()
opts
#> <M2MDownloadOptions>
#>   Scenes:  12
#>   Files:   66 selected
#>   Next:    $request(label)  |  $select_bands(...)  |  $bands()
```

Behind the scenes this registers a *scene list* on the M2M server, which
the API requires before it will list download options. The package
handles that for you; `$scene_list()` exposes it directly if you want it
for its own sake, for example to pull bulk metadata with `$metadata()`.

`$bands()` returns one row per downloadable file:

``` r

opts$bands()
#> # A tibble: 66 × 11
#>    entityId                            displayId          bulkAvailable filesize
#>    <chr>                               <chr>              <lgl>            <int>
#>  1 L2SR_LC08_L2SP_043032_20200717_..._B4_TIF LC08_..._SR_B4 TRUE          62914560
#>  2 L2SR_LC08_L2SP_043032_20200717_..._B5_TIF LC08_..._SR_B5 TRUE          62914560
```

A Landsat scene has dozens of associated files, so you will usually want
a subset. `$select_bands()` matches against `displayId`, and `$filter()`
accepts Remselection intact:

``` r

selected <- opts$
  select_bands(c("B4", "B5"))$
  filter(bulkAvailable)

nrow(selected$selected())
```

### Whole products versus individual files

The API offers downloads at two granularities, and `$bands()` only shows
one of them. Alongside the individual band files, each scene has
whole-product entries: a Level-1 Product Bundle, which is a single
`.tar` holding the entire scene, and full-resolution browse images.
`$scene_products()` lists these:

``` r

opts$scene_products()
#> # A tibble: 10 × 12
#>    productName                                    available id
#>    <chr>                                          <lgl>     <chr>
#>  1 Landsat Collection 2 Level-1 Product Bundle    TRUE      5e83d0a0f94d7d8d
#>  2 Full-Resolution Browse (Natural Color) GeoTIFF TRUE      5e9eb4f5ebb15d46
#>  3 Full-Resolution Browse (Thermal) GeoTIFF       TRUE      5e9eb5318675b02d
#>  8 Landsat Collection 2 Level-1 Product Bundle    FALSE     63231219fdd8c4e5
```

Browse images have no constituent files, so they never appear in
`$bands()` at all. Use `$select_products()` to queue whole products
instead of individual files — for one `.tar` per scene rather than 19
separate downloads:

``` r

bundles <- opts$
  select_products("Product Bundle")$
  filter(available)

bundles$request(label = "bundles_2020")
```

Note the `available` filter: the API lists superseded products with
`available = FALSE`, and requesting one will fail. As with
`$select_bands()`, neither selector filters on availability for you.

`$selected()` always shows exactly what `$request()` will queue,
whichever granularity you chose.

## Requesting and retrieving

`$request()` places the order. The label is how you find the order again
later, so give it something meaningful:

``` r

queue <- selected$request(label = "ndvi_july_2020")
queue
#> <M2MDownloadQueue>
#>   Label:     ndvi_july_2020
#>   Available: 6 file(s) ready
#>   Preparing: 0
#>   Next:      $retrieve(out_dir)
```

Files the distribution system can serve immediately come back with URLs
attached. Larger or archived products have to be staged first, and those
show up under `$requested` instead. `$is_ready()` tells you which
situation you are in, and `$refresh()` re-polls, adding newly ready
files to `$available`:

``` r

while (!queue$is_ready()) {
  Sys.sleep(30)
  queue$refresh()
}
```

`$refresh()` is the one method in the package that modifies its object
in place, because it reflects live server-side state. Everything else
returns a new object.

Finally, write the files to disk. The directory is created if it does
not exist:

``` r

results <- queue$retrieve("data/landsat")
results
#> # A tibble: 6 × 4
#>   entityId                      url                     path                 status
#>   <chr>                         <chr>                   <chr>                <chr>
#> 1 L2SR_LC08_..._SR_B4_TIF       https://landsatlook...  data/landsat/L2SR... downloaded
```

Check `results$status` for any `"failed"` rows rather than assuming
everything arrived.

## Resuming a large order

Orders live on the server, so a download does not have to finish in one
sitting. Reconnect by label from a fresh session:

``` r

sess <- m2m_session()
queue <- sess$download_queue("ndvi_july_2020")

queue$refresh()
queue$retrieve("data/landsat")
```

If you no longer remember the label, `sess$download_labels()` lists your
orders one row each:

``` r

sess$download_labels()
#> # A tibble: 1 × 5
#>   label          totalComplete downloadCount downloadSize   dateEntered
#>   <chr>                  <int>         <int> <chr>                <dbl>
#> 1 ndvi_july_2020             0             6 549580314    1787113928127
```

`sess$downloads()` lists every individual download across all labels,
and `queue$summary()` breaks one order down by dataset. `queue$cancel()`
abandons an order you no longer want.

Orders whose scenes are staged rather than queued need `queue$prepare()`
to start them processing — it is the one queue method besides
`$cancel()` that changes server-side state:

``` r

queue$prepare()
queue$refresh()
```

## Related scenes

Some datasets define a relationship between scenes — a mosaic and its
source frames, for instance. `$related_scenes()` follows it:

``` r

ds <- sess$dataset("aerial_combin")
related <- ds$related_scenes("AR1MSH06B001001", max_results = 5)
related$scenes
```

Most datasets define no such relationship and raise an `m2m_error` with
code `DATASET_ERROR`. Because related scenes usually belong to a
*different* dataset, the returned search is bound to that dataset, so
`$products()` on the result acts on the right one.

## End user license agreements

A few datasets require you to accept a EULA before downloads will
succeed. Acceptance happens once, through the Earth Explorer website,
but you can read the text from R:

``` r

sess$eula(code = "EULA_CODE")
```

The relevant code appears on queue entries for datasets that need one.
