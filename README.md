# kwb.flusshygiene.app

[![Appveyor build Status](https://ci.appveyor.com/api/projects/status/1ow2xhn25eg7e188/branch/master?svg=true)](https://ci.appveyor.com/project/KWB-R/kwb-flusshygiene-app/branch/master)
[![Travis build Status](https://travis-ci.org/KWB-R/kwb.flusshygiene.app.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.flusshygiene.app)
[![codecov](https://codecov.io/github/KWB-R/kwb.flusshygiene.app/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.flusshygiene.app)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/kwb.flusshygiene.app)]()

This GitHub repository contains the code of the R package
kwb.flusshygiene.app. The package provides functions to run a prediction
of the bathing water quality at two bathing spots in Berlin, Germany.
The main function is intended to be run once a day (preferably by
automation). This function

  - downloads new rain and flow measurements from two FTP servers,
  - merges the new data with existing data in local files,
  - prepares the data for input in statistical models,
  - runs two statistical models, one for each bathing spot, to predict
    the water quality at these spots,
  - saves the predictions in local files, and, optionally,
  - uploads the predictions for the current day to a web server.

## Installation

You need to have R installed. R is a free software environment for
statistical computing and graphics. You can download it
[here](https://cran.uni-muenster.de/).

Run R and install the following packages first:

``` r
install.packages("remotes")
install.packages("usethis")
```

Then, install kwb.flusshygiene.app directly from
[GitHub](https://github.com/) with:

``` r
remotes::install_github("KWB-R/kwb.flusshygiene.app")
```

## Giving access to the servers

The package needs access to at least two FTP servers to which rain and
water flow measurements are uploaded on a regular basis. You need to
specify the Uniform Resource Locators (URLs) to these servers as well as
the credentials (user name and password) for authentication. Therefore,
set the following environment variables:

  - `FTP_URL_KWB`: URL to KWB’s download server
  - `FTP_URL_SENATE`: URL to Senate’s download server
  - `FTP_URL_TSB`: URL to TSB’s upload server
  - `USER_PWD_KWB`: User name and password for KWB’s download server
  - `USER_PWD_SENATE`: User name and password for Senate’s download
    server
  - `USER_PWD_TSB` User name and password for TSB’s upload server

We recommend to set these variables in the `.Renviron` file that is
loaded automatically when R is started. Use the function
`edit_r_environ()` from the usethis package (installed above) to open
the `.Renviron` file in an editor:

``` r
usethis::edit_r_environ()
```

In the editor, add the following lines to the file (or make sure that
they are there). Replace `...` with the appropriate values (that you
know if you are an authenticated person).

    FTP_URL_KWB=ftp:...
    FTP_URL_SENATE=ftp://...
    FTP_URL_TSB=https://...
    
    USER_PWD_KWB=...:...
    USER_PWD_SENATE=...:...
    USER_PWD_TSB=...:...

Save and close the `.Renviron` file.

## Main script

Once the environment variables are set you can run the following main
script:

``` r
# Set the root folder below which to expect/create the app's folder structure
kwb.flusshygiene.app::set_root("~/projekte/flusshygiene/fruehwarnsystem")

# Update rain and flow databases by downloading current data and run the model
kwb.flusshygiene.app::update_data_and_predict()
```

With the first command you tell the package where to store downloaded
files, model input files and model results. Make sure that the folder
exists. Required subfolders will be created automatically.

With the second command you run the main function that performs all the
steps that are outlined in the preface above. The function has an
argument `upload` that is set to `FALSE` by default. The argument
specifies whether to upload the result of the daily prediction to the
web server of Technologiestiftung Berlin (TSB). Set this argument to
`TRUE` only if you know what you are doing\!

``` r
kwb.flusshygiene.app::update_data_and_predict(upload = TRUE)
```

We recommend that you setup a so called
[cron-job](https://en.wikipedia.org/wiki/Cron) that runs the main script
on a daily basis.

## Folder structure

The package uses the following structure of files and folders. The
folder structure will be created below the folder that you specify in
the `set_root()` call of the main script (see above).

    database\
      flows.csv - Flow data in text format (CSV)
      flows.fst - Flow data in binary format (fst, to be read with fst::read_fst())
      model_input.csv - Model input data in text format (CSV)
      rain-ruhleben.csv - Rain and Flow data (at outlet of WWT Ruhleben, CSV format)
      rain-ruhleben.fst - Rain and Flow data (at outlet of WWT Ruhleben, fst format)
    downloads\
      bwb\
        Regenschreiber_190615_0810.txt - Rain and Flow data of one day
        Regenschreiber_190616_0810.txt
        Regenschreiber_190617_0810.txt
        ...
      senate\
        TW_SW_190702.txt - Flow data of one day
    predictions\
      Vorhersage_2019-07-01.csv - Predictions for two sites and one day
      Vorhersage_2019-07-02.csv
      ...
      Vorhersagen.csv - All Predictions for the two sites so far
