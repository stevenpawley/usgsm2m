# M2M API session

Represents an authenticated connection to the USGS/EROS M2M API. Create
one with \[m2m_session()\] rather than calling \`M2MSession\$new()\`
directly.

## Methods

### Public methods

- [`M2MSession$new()`](#method-M2MSession-initialize)

- [`M2MSession$dataset()`](#method-M2MSession-dataset)

- [`M2MSession$find_datasets()`](#method-M2MSession-find_datasets)

- [`M2MSession$download_queue()`](#method-M2MSession-download_queue)

- [`M2MSession$downloads()`](#method-M2MSession-downloads)

- [`M2MSession$download_labels()`](#method-M2MSession-download_labels)

- [`M2MSession$eula()`](#method-M2MSession-eula)

- [`M2MSession$scene_list()`](#method-M2MSession-scene_list)

- [`M2MSession$logout()`](#method-M2MSession-logout)

- [`M2MSession$print()`](#method-M2MSession-print)

- [`M2MSession$clone()`](#method-M2MSession-clone)

------------------------------------------------------------------------

### `M2MSession$new()`

Authenticate and open a session.

#### Usage

    M2MSession$new(
      username = Sys.getenv("M2M_USERNAME"),
      token = Sys.getenv("M2M_TOKEN")
    )

#### Arguments

- `username`:

  ERS username.

- `token`:

  Earth Explorer M2M application token.

------------------------------------------------------------------------

### `M2MSession$dataset()`

Look up a dataset by alias or id. This is the usual starting point for a
search.

#### Usage

    M2MSession$dataset(name = NULL, id = NULL)

#### Arguments

- `name`:

  The system-friendly dataset alias, e.g. \`"landsat_ot_c2_l2"\`. Use
  this or \`id\`, not both.

- `id`:

  The dataset identifier. Use this or \`name\`, not both.

#### Returns

An \[M2MDataset\] object.

------------------------------------------------------------------------

### `M2MSession$find_datasets()`

Search the catalog for datasets matching a name pattern. Use this when
you don't yet know a dataset's alias; wildcards are applied
automatically around \`pattern\`.

#### Usage

    M2MSession$find_datasets(pattern, spatial = NULL, temporal = NULL)

#### Arguments

- `pattern`:

  Search pattern for the dataset name.

- `spatial`:

  Optional spatial filter, see \[filter_spatial()\].

- `temporal`:

  Optional temporal filter, see \[filter_temporal()\].

#### Returns

A tibble of matching datasets. Pass a \`datasetAlias\` value from the
result to \`\$dataset()\` to continue.

------------------------------------------------------------------------

### `M2MSession$download_queue()`

Reconnect to an existing download order by its label, for example to
resume collecting a large order in a later R session.

#### Usage

    M2MSession$download_queue(label)

#### Arguments

- `label`:

  The label the downloads were requested under.

#### Returns

An \[M2MDownloadQueue\] object.

------------------------------------------------------------------------

### `M2MSession$downloads()`

List every download in the queue, regardless of status or label.

#### Usage

    M2MSession$downloads()

#### Returns

A tibble of queued downloads.

------------------------------------------------------------------------

### `M2MSession$download_labels()`

List the distinct order labels in the download queue, one row each. Use
this to find an order to reconnect to with \`\$download_queue()\` when
you no longer remember its label.

#### Usage

    M2MSession$download_labels(download_application = NULL)

#### Arguments

- `download_application`:

  Optional application name to scope the listing to.

#### Returns

A tibble with \`label\`, \`downloadCount\`, \`totalComplete\`,
\`downloadSize\` and \`dateEntered\` (epoch milliseconds).

------------------------------------------------------------------------

### `M2MSession$eula()`

Retrieve the text of one or more End User License Agreements. Some
datasets require a EULA to be accepted (once, through the EarthExplorer
website) before downloads will succeed.

#### Usage

    M2MSession$eula(code = NULL, codes = NULL)

#### Arguments

- `code`:

  A single EULA code. Use this or \`codes\`, not both.

- `codes`:

  A character vector of EULA codes. Use this or \`code\`.

#### Returns

A tibble with \`eulaCode\` and \`agreementContent\`.

------------------------------------------------------------------------

### `M2MSession$scene_list()`

Attach to a scene list that already exists server-side.

#### Usage

    M2MSession$scene_list(list_id, dataset_name = NULL)

#### Arguments

- `list_id`:

  The scene list identifier.

- `dataset_name`:

  The dataset alias the list belongs to. Optional - it is looked up from
  the list's summary when needed. Supply it to save that lookup, or to
  pick one when the list spans several datasets.

#### Returns

An \[M2MSceneList\] object.

------------------------------------------------------------------------

### `M2MSession$logout()`

End the session, invalidating its API key. M2M sessions also expire on
their own after a period of inactivity.

#### Usage

    M2MSession$logout()

#### Returns

The session, invisibly.

------------------------------------------------------------------------

### `M2MSession$print()`

Print a summary of the session.

#### Usage

    M2MSession$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `M2MSession$clone()`

The objects of this class are cloneable with this method.

#### Usage

    M2MSession$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
