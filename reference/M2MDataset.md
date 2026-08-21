# A dataset in the M2M catalog

Returned by \`M2MSession\$dataset()\`. Holds a dataset's metadata and is
the starting point for a scene search. Not created directly.

## Public fields

- `info`:

  A one-row tibble of the dataset's metadata.

## Methods

### Public methods

- [`M2MDataset$new()`](#method-M2MDataset-initialize)

- [`M2MDataset$alias()`](#method-M2MDataset-alias)

- [`M2MDataset$id()`](#method-M2MDataset-id)

- [`M2MDataset$filters()`](#method-M2MDataset-filters)

- [`M2MDataset$search()`](#method-M2MDataset-search)

- [`M2MDataset$related_scenes()`](#method-M2MDataset-related_scenes)

- [`M2MDataset$print()`](#method-M2MDataset-print)

- [`M2MDataset$clone()`](#method-M2MDataset-clone)

------------------------------------------------------------------------

### `M2MDataset$new()`

Wrap dataset metadata. Use \`M2MSession\$dataset()\` instead.

#### Usage

    M2MDataset$new(session, info)

#### Arguments

- `session`:

  The parent \[M2MSession\].

- `info`:

  A one-row tibble of dataset metadata.

------------------------------------------------------------------------

### `M2MDataset$alias()`

The dataset's system-friendly alias, e.g. \`"landsat_ot_c2_l2"\`.

#### Usage

    M2MDataset$alias()

#### Returns

A string.

------------------------------------------------------------------------

### `M2MDataset$id()`

The dataset's identifier.

#### Usage

    M2MDataset$id()

#### Returns

A string.

------------------------------------------------------------------------

### `M2MDataset$filters()`

Metadata filter fields available for this dataset. Each row's \`id\` is
the \`filterId\` used when building a metadata filter to pass to
\`\$search(metadata = )\`.

#### Usage

    M2MDataset$filters()

#### Returns

A tibble of filter fields.

------------------------------------------------------------------------

### `M2MDataset$search()`

Search this dataset for scenes. All filters are optional; with none
supplied every scene in the dataset matches.

#### Usage

    M2MDataset$search(
      spatial = NULL,
      temporal = NULL,
      cloud = NULL,
      metadata = NULL,
      max_results = NULL
    )

#### Arguments

- `spatial`:

  A spatial filter, see \[filter_spatial()\].

- `temporal`:

  A temporal filter, see \[filter_temporal()\].

- `cloud`:

  A cloud cover filter, see \[filter_cloud()\].

- `metadata`:

  A metadata filter built from \`\$filters()\` field ids.

- `max_results`:

  Maximum number of scenes to return. If \`NULL\` (the default) all
  matching scenes are retrieved by paging through results.

#### Returns

An \[M2MSceneSearch\] object.

------------------------------------------------------------------------

### `M2MDataset$related_scenes()`

Find the scenes related to a given scene, via the
\`scene-search-secondary\` endpoint.

Only datasets that define a secondary relationship support this; others
raise an \`m2m_error\` with code \`DATASET_ERROR\`. The related scenes
belong to a different dataset, so the returned search is bound to that
dataset rather than this one - meaning \`\$products()\` on the result
acts on the correct dataset.

#### Usage

    M2MDataset$related_scenes(entity_id, max_results = NULL)

#### Arguments

- `entity_id`:

  The \`entityId\` of the scene to find relatives of.

- `max_results`:

  Maximum number of scenes to return. If \`NULL\` (the default) all are
  retrieved by paging through results.

#### Returns

An \[M2MSceneSearch\] over the secondary dataset.

------------------------------------------------------------------------

### `M2MDataset$print()`

Print a summary of the dataset.

#### Usage

    M2MDataset$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `M2MDataset$clone()`

The objects of this class are cloneable with this method.

#### Usage

    M2MDataset$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
