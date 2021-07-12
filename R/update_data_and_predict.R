# update_data_and_predict ------------------------------------------------------

#' Update Model Input Data and Predict Water Quality
#'
#' This is the main function. It is assumed to be run on a daily basis. It
#' downloads current rain and flow data, does a simple validation of the data,
#' prepares the data for the model and runs the model to predict the water
#' quality.
#'
#' The following environment variables need to be set:
#' \describe{
#'   \item{FTP_URL_KWB}{URL to KWB's download FTP server}
#'   \item{FTP_URL_SENATE}{URL to Senate's download FTP server}
#'   \item{FTP_URL_TSB}{URL to TSB's upload server}
#'   \item{USER_PWD_KWB}{User name and password for KWB's download FTP server}
#'   \item{USER_PWD_SENATE}{User name and password for  Senate's download FTP
#'   server}
#'   \item{USER_PWD_TSB}{User name and password for TSB's upload server}
#' }
#'
#' You may use \code{usethis::edit_r_environ()} to open the .Renviron file in
#' which you can add the corresponding assignments.
#'
#' @param day_string day for which to predict the water quality, in format
#'   yyyy-mm-dd, e.g. "2019-07-01". Default: \code{as.character(Sys.Date())}
#' @param upload logical. If \code{TRUE} the prediction file
#'   \code{Vorhersage_yyyy-mm-dd.csv} is uploaded to the server of
#'   Technologiestiftung Berlin (TSB).
#' @param dbg debug level. The higher the value, the more verbose the output
#' @export
#' @importFrom fst read_fst
update_data_and_predict <- function(
  day_string = as.character(Sys.Date()), upload = FALSE, dbg = 1
)
{
  #kwb.utils::assignPackageObjects("kwb.flusshygiene.app")

  # Update local database of data provided by Berlin Wasserbetriebe (BWB)
  update_bwb_database()

  # Update local database of data provided by the Senate of Berlin
  update_senate_database()

  # This function should always return TRUE
  if (FALSE) {
    kwb.utils::catAndRun("Checking database integrity", {
      stopifnot(check_database_file_identity())
    })
  }

  # Read rain and Ruhleben data
  rain_ruhleben <- fst::read_fst(db_path("rain-ruhleben.fst"))

  # Read Tiefwerder (and Sophienwerder) data
  flows <- fst::read_fst(db_path("flows.fst"))

  # Prepare model input data
  model_input <- prepare_model_input(
    rain_ruhleben = prepare_rain_ruhleben(rain_ruhleben),
    tiefwerder = prepare_tiefwerder(flows, dbg = dbg)
  )

  # Path to model input file
  input_file <- db_path("model_input.csv")

  # Write model input file
  write_input_file(model_input, input_file, context = "model input")

  # Check if reading model input data back works
  if (FALSE) {
    testthat::expect_equal(
      object = read_model_input_csv(input_file),
      expected = as.data.frame(model_input)
    )
  }

  # Calculate prediction for the model input data
  prediction_today <- input_file %>%
    read_model_input_csv() %>%
    get_model_input(logRain = TRUE, fittingData = FALSE) %>%
    filter_for_day_string(day_string) %>%
    get_two_site_prediction(dbg = dbg)

  # Save file with prediction of today locally
  file <- save_prediction_today(prediction_today, day_string)

  # Upload the locally saved file to the TSB server
  if (upload) {
    upload_prediction_today(file)
  }

  # Read "Vorhersagen.txt", append data, rewrite the file
  update_all_predictions(prediction_today)
}
