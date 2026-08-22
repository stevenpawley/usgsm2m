# A download order in the M2M queue

Returned by \`M2MDownloadOptions\$request()\`, or reconnected to with
\`M2MSession\$download_queue(label)\`. Tracks which files are ready to
download and which the distribution system is still preparing.

## Public fields

- `label`:

  The label identifying this order.

- `available`:

  A tibble of files ready to download, with URLs.

- `requested`:

  A tibble of files still being prepared.

- `queue_size`:

  Number of items the API still has queued.

## Methods

### Public methods

- [`M2MDownloadQueue$new()`](#method-M2MDownloadQueue-initialize)

- [`M2MDownloadQueue$refresh()`](#method-M2MDownloadQueue-refresh)

- [`M2MDownloadQueue$ready()`](#method-M2MDownloadQueue-ready)

- [`M2MDownloadQueue$pending()`](#method-M2MDownloadQueue-pending)

- [`M2MDownloadQueue$is_ready()`](#method-M2MDownloadQueue-is_ready)

- [`M2MDownloadQueue$retrieve()`](#method-M2MDownloadQueue-retrieve)

- [`M2MDownloadQueue$summary()`](#method-M2MDownloadQueue-summary)

- [`M2MDownloadQueue$prepare()`](#method-M2MDownloadQueue-prepare)

- [`M2MDownloadQueue$remove_items()`](#method-M2MDownloadQueue-remove_items)

- [`M2MDownloadQueue$cancel()`](#method-M2MDownloadQueue-cancel)

- [`M2MDownloadQueue$print()`](#method-M2MDownloadQueue-print)

- [`M2MDownloadQueue$clone()`](#method-M2MDownloadQueue-clone)

------------------------------------------------------------------------

### `M2MDownloadQueue$new()`

Attach to a download order. Use \`M2MDownloadOptions\$request()\` or
\`M2MSession\$download_queue()\`.

#### Usage

    M2MDownloadQueue$new(session, label, available = NULL, n_preparing = NULL)

#### Arguments

- `session`:

  The parent \[M2MSession\].

- `label`:

  The order's label.

- `available`:

  Optionally seed the ready-to-download files.

- `n_preparing`:

  Number of records the API accepted for preparation.

------------------------------------------------------------------------

### `M2MDownloadQueue$refresh()`

Re-poll the M2M queue for this label, picking up files that have
finished preparing since the last check.

Unlike the other methods here, this updates the object in place rather
than returning a new one - newly ready files are added to
\`\$available\` and \`\$requested\`/\`\$queue_size\` are replaced.

#### Usage

    M2MDownloadQueue$refresh()

#### Returns

The queue, invisibly.

------------------------------------------------------------------------

### `M2MDownloadQueue$ready()`

The files that can be downloaded now, from either \`\$available\` or
\`\$requested\`.

Readiness is a matter of having a URL, not of which bucket the API put a
row in: proxied downloads are listed under \`\$requested\` but carry a
working URL, because they are served by another USGS host rather than
staged by the distribution system.

#### Usage

    M2MDownloadQueue$ready()

#### Returns

A tibble of downloadable files.

------------------------------------------------------------------------

### `M2MDownloadQueue$pending()`

The files still being prepared, which have no URL yet.

#### Usage

    M2MDownloadQueue$pending()

#### Returns

A tibble of pending files.

------------------------------------------------------------------------

### `M2MDownloadQueue$is_ready()`

Whether every file in the order can be downloaded now.

#### Usage

    M2MDownloadQueue$is_ready()

#### Returns

\`TRUE\` if nothing is still being prepared.

------------------------------------------------------------------------

### `M2MDownloadQueue$retrieve()`

Download every file that has a URL to disk. Call \`\$refresh()\` first
if \`\$is_ready()\` is \`FALSE\`.

Files are named as the server names them - the \`.tar\` of a product
bundle, the \`.TIF\` of a band - rather than after the \`entityId\` the
queue lists them under, which for a scene-level product is the scene id
and carries no extension.

Proxied downloads are reported back to the API afterwards, since it does
not serve them itself and would otherwise leave them in the queue
indefinitely.

#### Usage

    M2MDownloadQueue$retrieve(out_dir, report_proxied = TRUE)

#### Arguments

- `out_dir`:

  Directory to write files into. Created if missing.

- `report_proxied`:

  Whether to mark proxied downloads complete.

#### Returns

A tibble with one row per file: \`entityId\`, \`downloadId\`, \`url\`,
\`path\`, \`size\` and \`status\`. A \`status\` of \`"expired"\` means
the signed URL is no longer valid - \`\$refresh()\` and retry.

------------------------------------------------------------------------

### `M2MDownloadQueue$summary()`

Summarize this order by dataset, via the \`download-summary\` endpoint.

#### Usage

    M2MDownloadQueue$summary(download_application = "M2M", send_email = FALSE)

#### Arguments

- `download_application`:

  The application the downloads were requested under. Required by the
  API; the counts come back as zero if it does not match the one used at
  request time.

- `send_email`:

  Whether the API should also email the summary.

#### Returns

A list with \`label\`, \`download_count\`, \`scene_count\`,
\`total_estimated_size\` and a \`collections\` tibble.

------------------------------------------------------------------------

### `M2MDownloadQueue$prepare()`

Move this order's scenes into the queue for processing, via the
\`download-order-load\` endpoint.

Unlike the other methods here this changes server-side state: it is what
starts a staged order being prepared. Follow it with \`\$refresh()\` to
pick up URLs as they become ready.

#### Usage

    M2MDownloadQueue$prepare(download_application = NULL)

#### Arguments

- `download_application`:

  Optional application name to scope the order to.

#### Returns

The queue, invisibly.

------------------------------------------------------------------------

### `M2MDownloadQueue$remove_items()`

Remove individual files from this order, leaving the rest of it in
place. \`\$cancel()\` drops the whole order instead.

Ids come from the \`downloadId\` column of \`\$ready()\` or
\`\$pending()\`.

#### Usage

    M2MDownloadQueue$remove_items(download_id, quiet = FALSE)

#### Arguments

- `download_id`:

  A vector of download ids.

- `quiet`:

  Suppress the message shown before a large batch.

#### Returns

The queue, invisibly.

------------------------------------------------------------------------

### `M2MDownloadQueue$cancel()`

Cancel this order, removing it from the M2M queue.

#### Usage

    M2MDownloadQueue$cancel()

#### Returns

The queue, invisibly.

------------------------------------------------------------------------

### `M2MDownloadQueue$print()`

Print a summary of the order.

#### Usage

    M2MDownloadQueue$print(...)

#### Arguments

- `...`:

  Ignored.

------------------------------------------------------------------------

### `M2MDownloadQueue$clone()`

The objects of this class are cloneable with this method.

#### Usage

    M2MDownloadQueue$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
