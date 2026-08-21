# Results of a scene search

Returned by \`M2MDataset\$search()\`. Holds the matching scenes and is
the bridge to discovering downloadable products. Not created directly.

## Public fields

- `scenes`:

  A tibble of the matching scenes.

- `total_hits`:

  The total number of scenes the API reported as matching, which exceeds
  \`nrow(scenes)\` when \`max_results\` was set.

## Methods

### Public methods

- [`M2MSceneSearch$new()`](#method-M2MSceneSearch-initialize)

- [`M2MSceneSearch$dataset()`](#method-M2MSceneSearch-dataset)

- [`M2MSceneSearch$filter()`](#method-M2MSceneSearch-filter)

- [`M2MSceneSearch$scene_list()`](#method-M2MSceneSearch-scene_list)

- [`M2MSceneSearch$products()`](#method-M2MSceneSearch-products)

- [`M2MSceneSearch$print()`](#method-M2MSceneSearch-print)

- [`M2MSceneSearch$clone()`](#method-M2MSceneSearch-clone)

------------------------------------------------------------------------

### `M2MSceneSearch$new()`

Wrap search results. Use \`M2MDataset\$search()\` instead.

#### Usage

    M2MSceneSearch$new(session, dataset, results, total_hits)

#### Arguments

- `session`:

  The parent \[M2MSession\].

- `dataset`:

  The \[M2MDataset\] that was searched.

- `results`:

  A tibble of matching scenes.

- `total_hits`:

  Total matches reported by the API.

------------------------------------------------------------------------

### `M2MSceneSearch$dataset()`

The dataset these scenes came from.

#### Usage

    M2MSceneSearch$dataset()

#### Returns

An \[M2MDataset\] object.

------------------------------------------------------------------------

### `M2MSceneSearch$filter()`

Subset the scenes before requesting products, using \`dplyr::filter()\`
semantics against the \`scenes\` tibble.

#### Usage

    M2MSceneSearch$filter(...)

#### Arguments

- `...`:

  Expressions passed to \`dplyr::filter()\`.

#### Returns

A new \[M2MSceneSearch\] with the subset applied.

------------------------------------------------------------------------

### `M2MSceneSearch$scene_list()`

Register these scenes as a scene list on the M2M server.
\`\$products()\` does this for you, so you only need it directly if you
want the list for its own sake (e.g. bulk metadata).

The registration is reused: calling this again, or calling
\`\$products()\` more than once, returns the same list rather than
registering the scenes a second time. Passing an explicit \`list_id\`
always registers under that id, and does not disturb the reused one.

#### Usage

    M2MSceneSearch$scene_list(list_id = NULL)

#### Arguments

- `list_id`:

  Optional identifier. A unique one is generated if omitted.

#### Returns

An \[M2MSceneList\] object.

------------------------------------------------------------------------

### `M2MSceneSearch$products()`

Discover the products (bands and related files) available for these
scenes. This registers a scene list server-side first, which the M2M API
requires before download options can be listed.

#### Usage

    M2MSceneSearch$products(band_group = TRUE)

#### Arguments

- `band_group`:

  Whether to include secondary file groups, i.e. the individual bands.
  \`TRUE\` by default.

#### Returns

An \[M2MDownloadOptions\] object.

------------------------------------------------------------------------

### `M2MSceneSearch$print()`

Print a summary of the search results.

#### Usage

    M2MSceneSearch$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `M2MSceneSearch$clone()`

The objects of this class are cloneable with this method.

#### Usage

    M2MSceneSearch$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
