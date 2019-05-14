[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/github/KWB-R/kwb.flusshygiene.app?branch=master&svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-flusshygiene-app/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.flusshygiene.app.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.flusshygiene.app)
[![codecov](https://codecov.io/github/KWB-R/kwb.flusshygiene.app/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.flusshygiene.app)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.flusshygiene.app)]()

# kwb.flusshygiene.app

R Package Implementing the Flusshygiene
Application. This package provides the functions required to setup the
web application as it was developed during the KWB project
'Flusshygiene'.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

### Temporary workaround on Windows to fix bug in CRAN version v2.0.2
### of "remotes" (see https://github.com/r-lib/remotes/issues/248)

remotes::install_github("r-lib/remotes@18c7302637053faf21c5b025e1e9243962db1bdc")
remotes::install_github("KWB-R/kwb.flusshygiene.app")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.flusshygiene.app](https://kwb-r.github.io/kwb.flusshygiene.app)

Development: [https://kwb-r.github.io/kwb.flusshygiene.app/dev](https://kwb-r.github.io/kwb.flusshygiene.app/dev)
