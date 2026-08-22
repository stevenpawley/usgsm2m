# Products available for download

Returned by \`M2MSceneSearch\$products()\`. Holds what the M2M API will
let you download for a set of scenes, and lets you narrow it before
placing a request. Not created directly.

The API offers downloads at two granularities, and this object exposes
both:

\* \*\*Individual files\*\* - the bands and per-file products nested
inside a product's \`secondaryDownloads\`, listed by \`\$bands()\`. This
is the default selection. \* \*\*Whole products\*\* - the scene-level
entries the API lists, such as a Level-1 Product Bundle (a single
\`.tar\` for the scene) or a full-resolution browse image, listed by
\`\$scene_products()\`. Some of these have no constituent files at all
and so never appear in \`\$bands()\`.

Both are requestable. Use \`\$select_bands()\` to queue individual
files, or \`\$select_products()\` to queue whole products;
\`\$selected()\` always shows what \`\$request()\` will actually queue.

## Public fields

- `products`:

  A scene-level tibble of products, with the individual bands nested in
  a \`secondaryDownloads\` list-column.

## Methods

### Public methods

- [`M2MDownloadOptions$new()`](#method-M2MDownloadOptions-initialize)

- [`M2MDownloadOptions$selected()`](#method-M2MDownloadOptions-selected)

- [`M2MDownloadOptions$bands()`](#method-M2MDownloadOptions-bands)

- [`M2MDownloadOptions$scene_products()`](#method-M2MDownloadOptions-scene_products)

- [`M2MDownloadOptions$select_bands()`](#method-M2MDownloadOptions-select_bands)

- [`M2MDownloadOptions$select_products()`](#method-M2MDownloadOptions-select_products)

- [`M2MDownloadOptions$filter()`](#method-M2MDownloadOptions-filter)

- [`M2MDownloadOptions$request()`](#method-M2MDownloadOptions-request)

- [`M2MDownloadOptions$print()`](#method-M2MDownloadOptions-print)

- [`M2MDownloadOptions$clone()`](#method-M2MDownloadOptions-clone)

------------------------------------------------------------------------

### `M2MDownloadOptions$new()`

Wrap download options. Use \`M2MSceneSearch\$products()\` instead.

#### Usage

    M2MDownloadOptions$new(session, products, scene_list, selected = NULL)

#### Arguments

- `session`:

  The parent \[M2MSession\].

- `products`:

  A tibble of products from the download-options endpoint.

- `scene_list`:

  The \[M2MSceneList\] the products belong to.

- `selected`:

  An optional pre-narrowed band tibble.

------------------------------------------------------------------------

### `M2MDownloadOptions$selected()`

The rows \`\$request()\` will queue. Individual files by default, or
whole products after \`\$select_products()\`.

#### Usage

    M2MDownloadOptions$selected()

#### Returns

A tibble of downloadable items.

------------------------------------------------------------------------

### `M2MDownloadOptions$bands()`

The currently selected downloads. An alias for \`\$selected()\`, kept
because the selection is individual bands in the common case.

#### Usage

    M2MDownloadOptions$bands()

#### Returns

A tibble of downloadable items.

------------------------------------------------------------------------

### `M2MDownloadOptions$scene_products()`

The whole-product entries the API lists for these scenes, one row each,
such as a Level-1 Product Bundle or a full-resolution browse image.

These are requestable in their own right and are a different unit from
\`\$bands()\`: a bundle is a single \`.tar\` holding the whole scene,
where \`\$bands()\` would list its contents as separate files. Products
with no constituent files, such as browse imagery, appear only here.

#### Usage

    M2MDownloadOptions$scene_products()

#### Returns

A tibble of products, without the \`secondaryDownloads\` column.

------------------------------------------------------------------------

### `M2MDownloadOptions$select_bands()`

Select the individual files whose \`displayId\` matches any of the given
patterns.

Each value is matched as a literal substring of \`displayId\`, which is
how \`"B4"\` picks out \`...\_SR_B4\` without you writing the whole
name. Nothing is treated as pattern syntax, so a value containing
punctuation matches itself. See \`\$bands()\` for the values. Matching
nothing warns and lists what was available.

Like \`\$select_products()\`, this selects afresh from everything
available at that granularity rather than narrowing an existing
selection - use \`\$filter()\` to narrow. It also does not filter on
availability; chain \`\$filter(bulkAvailable)\` if you want only
bulk-downloadable files.

#### Usage

    M2MDownloadOptions$select_bands(patterns)

#### Arguments

- `patterns`:

  A character vector of patterns, e.g. \`c("B4", "B5")\`.

#### Returns

A new \[M2MDownloadOptions\] with those files selected.

------------------------------------------------------------------------

### `M2MDownloadOptions$select_products()`

Switch the selection to whole products rather than the individual files
inside them, optionally keeping only those whose \`productName\` matches
one of the given patterns.

Values are matched \*\*exactly\*\*, against either \`productName\` or
\`productCode\`, so a value copied out of \`\$scene_products()\` selects
what you copied. Names contain characters such as parentheses that would
otherwise be read as pattern syntax, and they differ between datasets -
the bundle is "Landsat Collection 2 Level-2 Product Bundle" for
\`landsat_ot_c2_l2\` but "Standard Format" for \`corona2\` - so read
them off \`\$scene_products()\` rather than assuming. Matching nothing
warns and lists what was available.

For anything looser, chain \`\$filter()\`, which takes arbitrary
expressions: \`\$select_products()\$filter(grepl("Browse",
productName))\`.

As with \`\$select_bands()\` this does not filter on availability; chain
\`\$filter(available)\` to drop products the API has marked unavailable.

#### Usage

    M2MDownloadOptions$select_products(products = NULL)

#### Arguments

- `products`:

  An optional character vector of \`productName\` or \`productCode\`
  values, matched exactly.

#### Returns

A new \[M2MDownloadOptions\] with the products selected.

------------------------------------------------------------------------

### `M2MDownloadOptions$filter()`

Narrow the current selection using \`dplyr::filter()\` semantics against
the tibble returned by \`\$selected()\`.

#### Usage

    M2MDownloadOptions$filter(...)

#### Arguments

- `...`:

  Expressions passed to \`dplyr::filter()\`.

#### Returns

A new \[M2MDownloadOptions\] with the narrowed selection.

------------------------------------------------------------------------

### `M2MDownloadOptions$request()`

Place a download request for the current selection. Files the
distribution system can serve immediately come back with URLs; anything
needing preparation is collected later via
\`M2MDownloadQueue\$refresh()\`.

#### Usage

    M2MDownloadOptions$request(label)

#### Arguments

- `label`:

  A label identifying this order in the download queue. Required, and
  used to find the order again later.

#### Returns

An \[M2MDownloadQueue\] object.

------------------------------------------------------------------------

### `M2MDownloadOptions$print()`

Print a summary of the available and selected downloads.

#### Usage

    M2MDownloadOptions$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `M2MDownloadOptions$clone()`

The objects of this class are cloneable with this method.

#### Usage

    M2MDownloadOptions$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
