# Connect to the USGS/EROS M2M API

Authenticates against the M2M API and returns a session object, which is
the entry point for everything else in this package. From a session you
reach a dataset, from a dataset a scene search, and so on - each step
returns an object whose methods are the valid next steps, so the
download pipeline is discoverable by tab-completing \`\$\` in the
console.

## Usage

``` r
m2m_session(
  username = Sys.getenv("M2M_USERNAME"),
  token = Sys.getenv("M2M_TOKEN")
)
```

## Arguments

- username:

  ERS username. This is the same username that is used to log into Earth
  Explorer. Defaults to the \`M2M_USERNAME\` environment variable.

- token:

  Earth Explorer M2M application token. To generate a token, go to
  https://ers.cr.usgs.gov/. The token is different from the password
  used to log into Earth Explorer. Defaults to the \`M2M_TOKEN\`
  environment variable.

## Value

An \[M2MSession\] object.

## Details

The usual pipeline is:

“\` sess \<- m2m_session() queue \<- sess\$
dataset("landsat_ot_c2_l2")\$ search(spatial = filter_spatial(-120, 40,
-119, 41), temporal = filter_temporal("2020-07-01", "2020-07-31"))\$
products()\$ select_bands(c("B4", "B5"))\$ request(label = "my_order")

queue\$retrieve("data/") “\`

## Examples

``` r
if (FALSE) { # \dontrun{
# Login using environment variables
sess <- m2m_session()

# Login using arguments
sess <- m2m_session(username = "your_username", token = "your_token")
} # }
```
