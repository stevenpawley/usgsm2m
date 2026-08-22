# USGSm2m

<!-- badges: start -->
[![R-CMD-check](https://github.com/stevenpawley/USGSm2m/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stevenpawley/USGSm2m/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

R interface to the USGS/EROS Machine-to-Machine (M2M) API.

This package is not affiliated with the USGS/EROS. It is a community-developed
interface to the M2M API for use in R.

Documentation is at <https://stevenpawley.github.io/USGSm2m/>.

## Installation

The package is not on CRAN. Install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("stevenpawley/USGSm2m")
```

You also need access to the M2M API itself, which is requested separately and
has to be granted before any of this will work. See
<https://m2m.cr.usgs.gov/> for details.

## Design

The package is organised around a chain of objects, where each stage returns
an object whose methods are the valid next steps. Tab-completing `$` in the
console shows you what you can do next, and every object prints a `Next:` hint,
so the order of operations is discoverable without reading the API docs:

```
M2MSession  →  M2MDataset  →  M2MSceneSearch  →  M2MDownloadOptions  →  M2MDownloadQueue
```

## Getting started

Authentication uses an ERS username and an M2M **application token** (not your
Earth Explorer password). Generate a token at <https://ers.cr.usgs.gov/>.
`m2m_session()` reads `M2M_USERNAME` and `M2M_TOKEN` from the environment by
default:

```r
library(USGSm2m)

sess <- m2m_session()
#> <M2MSession>
#>   User:    your_username
#>   Status:  connected
#>   Next:    $dataset(name)  |  $find_datasets(pattern)
```

## Finding data

If you don't know a dataset's alias, search the catalog:

```r
sess$find_datasets("landsat")
```

Then pick one up by alias:

```r
ds <- sess$dataset("landsat_ot_c2_l2")
#> <M2MDataset>
#>   Alias:   landsat_ot_c2_l2
#>   Name:    Landsat 8-9 OLI/TIRS C2 L2
#>   Scenes:  4,132,193
#>   Next:    $search(...)  |  $filters()
```

## Searching for scenes

```r
found <- ds$search(
  spatial  = filter_spatial(ll_lon = -120, ll_lat = 40, ur_lon = -119.5, ur_lat = 40.5),
  temporal = filter_temporal("2020-07-01", "2020-07-31"),
  cloud    = filter_cloud(0, 50)
)
#> <M2MSceneSearch>
#>   Dataset: landsat_ot_c2_l2
#>   Scenes:  3 of 3 total hits
#>   Next:    $products()  |  $filter(...)  |  $scene_list()
```

All results are retrieved by paging automatically; pass `max_results` to cap
this. `$total_hits` reports what the API matched, so you can tell a capped
result set from a complete one.

Dataset-specific fields can be filtered too. `$filters()` lists the available
fields and their ids, which feed into the `filter_metadata_*()` builders:

```r
ds$filters()

ds$search(metadata = filter_metadata_and(
  filter_metadata_value("5e83d14fb9436d88", "045"),  # WRS Path
  filter_metadata_value("5e83d14ff1eda1b8", "032")   # WRS Row
))
```

## Downloading

`$products()` lists what is downloadable. It registers a scene list on the M2M
server behind the scenes, which the API requires before download options can be
listed:

Downloads come at two granularities. `$bands()` lists the individual band
files, while `$scene_products()` lists whole products — a Level-1 Product
Bundle (one `.tar` per scene) or a full-resolution browse image. Browse
products have no constituent files and so appear only in the latter.

```r
queue <- found$
  products()$
  select_bands(c("B4", "B5"))$
  filter(bulkAvailable)$
  request(label = "my_order")
#> <M2MDownloadQueue>
#>   Label:     my_order
#>   Available: 12 file(s) ready
#>   Preparing: 0
#>   Next:      $retrieve(out_dir)

queue$retrieve("data/")
```

To take whole products instead — one `.tar` per scene rather than many
separate files — swap the selector. The names to match come from
`$scene_products()`, and differ between datasets, so look before you pick:

```r
opts <- found$products()

unique(opts$scene_products()$productName)
#> [1] "Landsat Collection 2 Level-2 Product Bundle"
#> [2] "Landsat Collection 2 Level-2 Band File"

queue <- opts$
  select_products("Landsat Collection 2 Level-2 Product Bundle")$
  filter(available)$
  request(label = "bundles")
```

Products are matched **exactly**, against `productName` or `productCode`, so a
value copied out of that listing selects what you copied. For looser matching
chain `$filter()`, which takes any expression:
`$select_products()$filter(grepl("Bundle", productName))`.

`$selected()` shows exactly what `$request()` will queue, either way. A value
that matches nothing warns and lists what was available, rather than quietly
selecting an empty set.

Files the distribution system cannot serve immediately are prepared in the
background. When `$is_ready()` is `FALSE`, poll with `$refresh()` — it adds
newly ready files to `$available` in place:

```r
while (!queue$is_ready()) {
  Sys.sleep(30)
  queue$refresh()
}

queue$retrieve("data/")
```

An order can be picked up again in a later R session by its label, so a large
download does not have to complete in one sitting:

```r
queue <- sess$download_queue("my_order")
```

## Errors

API failures raise errors of class `m2m_error` (with `m2m_http_error`,
`m2m_api_error`, `m2m_not_found`, or `m2m_no_access` subclasses) rather than
returning `NULL`,
so a broken step stops the chain rather than surfacing later as a confusing
error:

```r
tryCatch(
  sess$dataset("not_a_real_dataset"),
  m2m_not_found = function(e) message("no such dataset")
)
```

## Mutation semantics

Stage transitions (`$dataset()`, `$search()`, `$products()`, `$request()`) and
the narrowing methods (`$filter()`, `$select_bands()`) all return **new**
objects, so intermediate results stay usable. The one exception is
`M2MDownloadQueue$refresh()`, which updates the queue in place because it
represents live server-side state.
