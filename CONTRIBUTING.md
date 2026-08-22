# Contributing to usgsm2m

## Code architecture

The package separates the user workflow from the USGS M2M HTTP API:

```text
M2MSession → M2MDataset → M2MSceneSearch → M2MSceneList
                                             ↓
                              M2MDownloadOptions → M2MDownloadQueue

class-* method → api_* endpoint function → m2m_request()
                         ↓
                    coerce_*()
```

Files under `R/` follow the same layers:

- `class-*.R` contains the public R6 workflow objects and their state.
- `api-*.R` contains internal functions corresponding to M2M endpoints.
- `api-request.R` contains shared authenticated HTTP and response-error logic.
- `coerce-*.R` normalizes API responses into stable R values.
- `filters.R` contains public request-payload constructors.
- `utils.R` contains small helpers shared across layers.

## Layer responsibilities

R6 methods decide what the next workflow operation is and which objects to
return. They do not construct HTTP requests or reshape raw API responses.

Functions prefixed `api_` each represent an endpoint or one closely related
transport operation. Their usual structure is:

```r
api_example <- function(session, value) {
  payload <- list(value = value)

  resp <- m2m_request(session, "example", payload)

  result <- m2m_response_data(resp, "Example request failed")

  coerce_example(result)
}
```

Not every endpoint needs all four stages. An endpoint with no body omits
`payload`; one returning a scalar may not need a `coerce_*` function.

The API layer accepts the small `ers_session` value returned by
`M2MSession$ers_session()`. Keeping that conversion explicit at class call
sites makes the boundary between workflow state and transport credentials
visible and keeps endpoint functions easy to test with a plain fixture.

Functions prefixed `coerce_` contain response-shape knowledge, including API
inconsistencies and nested-column handling. They should not perform requests or
create R6 objects.

## Endpoint inventory

| API function | M2M endpoint | Public entry point |
|---|---|---|
| `api_dataset_search()` | `dataset-search` | `M2MSession$find_datasets()` |
| `api_dataset()` | `dataset` | `M2MSession$dataset()` |
| `api_dataset_filters()` | `dataset-filters` | `M2MDataset$filters()` |
| `api_scene_search()` | `scene-search` | `M2MDataset$search()` |
| `api_scene_search_secondary()` | `scene-search-secondary` | `M2MDataset$related_scenes()` |
| `api_scene_list_add()` | `scene-list-add` | `M2MSceneSearch$scene_list()` |
| `api_scene_list_get()` | `scene-list-get` | `M2MSceneList$scenes()` |
| `api_scene_list_summary()` | `scene-list-summary` | `M2MSceneList$summary()` |
| `api_scene_list_remove()` | `scene-list-remove` | `M2MSceneList$remove()` |
| `api_scene_metadata_list()` | `scene-metadata-list` | `M2MSceneList$metadata()` |
| `api_scene_products()` | `download-options` | `M2MSceneList$products()` |
| `api_download_request()` | `download-request` | `M2MDownloadOptions$request()` |
| `api_download_search()` | `download-search` | `M2MSession$downloads()` |
| `api_download_queue()` | `download-retrieve` | `M2MDownloadQueue$refresh()` |
| `api_download_complete_proxied()` | `download-complete-proxied` | `M2MDownloadQueue$retrieve()` |
| `api_download_labels()` | `download-labels` | `M2MSession$download_labels()` |
| `api_download_summary()` | `download-summary` | `M2MDownloadQueue$summary()` |
| `api_download_order_load()` | `download-order-load` | `M2MDownloadQueue$prepare()` |
| `api_download_eula()` | `download-eula` | `M2MSession$eula()` |
| `api_download_remove_order()` | `download-order-remove` | `M2MDownloadQueue$cancel()` |
| `api_download_remove_items()` | `download-remove` | `M2MSession$remove_items()` / `M2MDownloadQueue$remove_items()` |

`api_download_files()` is intentionally absent: it transfers URLs obtained
from the API rather than representing an M2M JSON endpoint.

## M2M behavior worth knowing

- Scene products and bulk metadata require a server-side scene list. A search
  creates one lazily through `$scene_list()`, and repeated `$products()` calls
  reuse it.
- `download-options` can repeat the same secondary file under several product
  entries, so `coerce_products()` retains nested files and the selection layer
  de-duplicates them before requesting downloads.
- Scene-search results are paginated. `m2m_paginate_scene_search()` owns the
  shared pagination protocol for normal and secondary searches.
- Proxied download URLs are served by another USGS host and must not receive
  the M2M token. Successful proxied transfers are reported with
  `download-complete-proxied`.
- Several M2M failures use HTTP 200 plus `errorCode`; always use
  `m2m_response_data()` or `m2m_check_response()` rather than treating a 200
  response as success.

## Adding an endpoint

1. Add an `api_*` function to the appropriate `api-*.R` file.
2. Use `m2m_request()` for authenticated M2M requests.
3. Put non-trivial response normalization in the corresponding `coerce-*.R`.
4. Expose the operation from the R6 class representing that workflow state.
5. Add tests according to the endpoint checklist below.

## Endpoint test checklist

Tests run without M2M credentials. Use `mock_session()`, `mock_response()`,
and `with_captured_requests()` from `tests/testthat/helper-mock-http.R`.

For every new JSON endpoint, add:

1. A request test that asserts the path, authentication header, and JSON body
   where there is one.
2. A success test that checks the public result or the endpoint's parsed value.
3. An error test when the endpoint has a distinct error case, such as null data
   with HTTP 200, a required argument, or an API `errorCode`.

Add a unit test in `test-coercion-unit.R` for every non-trivial response shape:
empty arrays, optional fields, nested data, or fields that vary between a
scalar, list, and data frame. Keep live M2M tests focused on end-to-end
coverage and guarded by `skip_if_no_m2m()`.
