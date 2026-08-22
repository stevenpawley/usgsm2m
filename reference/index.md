# Package index

## Connecting

Authenticate against the M2M API. The session is the entry point for
everything else.

- [`m2m_session()`](https://stevenpawley.github.io/usgsm2m/reference/m2m_session.md)
  : Connect to the USGS/EROS M2M API
- [`M2MSession`](https://stevenpawley.github.io/usgsm2m/reference/M2MSession.md)
  : M2M API session

## The pipeline

Each stage returns an object whose methods are the valid next steps,
from a dataset through to files on disk.

- [`M2MDataset`](https://stevenpawley.github.io/usgsm2m/reference/M2MDataset.md)
  : A dataset in the M2M catalog
- [`M2MSceneSearch`](https://stevenpawley.github.io/usgsm2m/reference/M2MSceneSearch.md)
  : Results of a scene search
- [`M2MSceneList`](https://stevenpawley.github.io/usgsm2m/reference/M2MSceneList.md)
  : A scene list registered on the M2M server
- [`M2MDownloadOptions`](https://stevenpawley.github.io/usgsm2m/reference/M2MDownloadOptions.md)
  : Products available for download
- [`M2MDownloadQueue`](https://stevenpawley.github.io/usgsm2m/reference/M2MDownloadQueue.md)
  : A download order in the M2M queue

## Search filters

Constructors for the filters accepted by `M2MDataset$search()`. The
metadata filters take field ids reported by `M2MDataset$filters()`.

- [`filter_spatial()`](https://stevenpawley.github.io/usgsm2m/reference/filter_spatial.md)
  : Create a spatial filter for use in a scene search
- [`filter_temporal()`](https://stevenpawley.github.io/usgsm2m/reference/filter_temporal.md)
  : Create a temporal filter for use in a scene search
- [`filter_cloud()`](https://stevenpawley.github.io/usgsm2m/reference/filter_cloud.md)
  : Create a cloud cover filter for use in a scene search
- [`filter_metadata_value()`](https://stevenpawley.github.io/usgsm2m/reference/filter_metadata_value.md)
  : Create a metadata filter matching a single value
- [`filter_metadata_between()`](https://stevenpawley.github.io/usgsm2m/reference/filter_metadata_between.md)
  : Create a metadata filter matching a range of values
- [`filter_metadata_and()`](https://stevenpawley.github.io/usgsm2m/reference/filter_metadata_and.md)
  [`filter_metadata_or()`](https://stevenpawley.github.io/usgsm2m/reference/filter_metadata_and.md)
  : Combine metadata filters with a logical operator

## Package

- [`usgsm2m`](https://stevenpawley.github.io/usgsm2m/reference/usgsm2m-package.md)
  [`usgsm2m-package`](https://stevenpawley.github.io/usgsm2m/reference/usgsm2m-package.md)
  : usgsm2m: R Interface to the USGS/EROS Machine-to-Machine (M2M)
  Application Programming Interface (API)
- [`` `%>%` ``](https://stevenpawley.github.io/usgsm2m/reference/pipe.md)
  : Pipe operator
