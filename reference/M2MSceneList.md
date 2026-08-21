# A scene list registered on the M2M server

Scene lists are the M2M API's way of naming a set of scenes so that
other endpoints can act on them in bulk: \`download-options\` and
\`scene-metadata-list\` will not take scene ids inline, only a
\`listId\`.

\`M2MSceneSearch\$products()\` creates and uses one for you, so the
usual pipeline never mentions scene lists. Reach for this class when the
list itself is what you want - \`\$metadata()\` fetches metadata for
every scene in one call rather than one call per scene, and
\`\$summary()\` reports the set's combined extent.

Get one with \`M2MSceneSearch\$scene_list()\`, or reattach to an
existing list with \`M2MSession\$scene_list()\`. Not created directly.

## Public fields

- `list_id`:

  The scene list's identifier.

- `dataset_name`:

  The dataset alias the list belongs to, or \`NA\` if not yet known.
  Reattaching to a list by id leaves this \`NA\` until \`\$dataset()\`
  resolves it, since the API does not report a list's dataset except
  through its summary.

## Methods

### Public methods

- [`M2MSceneList$new()`](#method-M2MSceneList-initialize)

- [`M2MSceneList$dataset()`](#method-M2MSceneList-dataset)

- [`M2MSceneList$scenes()`](#method-M2MSceneList-scenes)

- [`M2MSceneList$metadata()`](#method-M2MSceneList-metadata)

- [`M2MSceneList$summary()`](#method-M2MSceneList-summary)

- [`M2MSceneList$products()`](#method-M2MSceneList-products)

- [`M2MSceneList$remove()`](#method-M2MSceneList-remove)

- [`M2MSceneList$print()`](#method-M2MSceneList-print)

- [`M2MSceneList$clone()`](#method-M2MSceneList-clone)

------------------------------------------------------------------------

### `M2MSceneList$new()`

Attach to a scene list. Use \`M2MSceneSearch\$scene_list()\` or
\`M2MSession\$scene_list()\` instead.

#### Usage

    M2MSceneList$new(session, list_id, dataset_name = NA_character_)

#### Arguments

- `session`:

  The parent \[M2MSession\].

- `list_id`:

  The scene list identifier.

- `dataset_name`:

  The dataset alias the list belongs to, or \`NA\` if unknown.

------------------------------------------------------------------------

### `M2MSceneList$dataset()`

The dataset alias this list belongs to, looked up from the list's
summary if it is not already known and cached thereafter.

A scene list can span several datasets, in which case there is no single
answer and this errors - pass \`dataset_name\` to
\`M2MSession\$scene_list()\` to say which one you mean.

#### Usage

    M2MSceneList$dataset()

#### Returns

The dataset alias, as a string.

------------------------------------------------------------------------

### `M2MSceneList$scenes()`

The entity ids currently in the list.

#### Usage

    M2MSceneList$scenes()

#### Returns

A tibble of scene entity ids.

------------------------------------------------------------------------

### `M2MSceneList$metadata()`

Full metadata for every scene in the list.

#### Usage

    M2MSceneList$metadata()

#### Returns

A tibble of scene metadata.

------------------------------------------------------------------------

### `M2MSceneList$summary()`

Summarize the list's spatial and temporal extent, and the datasets it
spans.

#### Usage

    M2MSceneList$summary()

#### Returns

A tibble, with overall bounds in the \`spatialBounds\` attribute.

------------------------------------------------------------------------

### `M2MSceneList$products()`

Discover the products available for the scenes in this list.

#### Usage

    M2MSceneList$products(band_group = TRUE)

#### Arguments

- `band_group`:

  Whether to include secondary file groups (bands).

#### Returns

An \[M2MDownloadOptions\] object.

------------------------------------------------------------------------

### `M2MSceneList$remove()`

Delete the scene list from the M2M server. Lists also expire on their
own after a period of inactivity.

#### Usage

    M2MSceneList$remove()

#### Returns

The scene list, invisibly.

------------------------------------------------------------------------

### `M2MSceneList$print()`

Print a summary of the scene list.

#### Usage

    M2MSceneList$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `M2MSceneList$clone()`

The objects of this class are cloneable with this method.

#### Usage

    M2MSceneList$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
