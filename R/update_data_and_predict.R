# update_data_and_predict ------------------------------------------------------

#' Update Model Input Data and Predict Water Quality
#'
#' @param day_string day for which to predict the water quality, in format
#'   yyyy-mm-dd, e.g. "2019-07-01". Default: \code{as.character(Sys.Date())}
#' @param upload logical. If \code{TRUE} the prediction file
#'   \code{Vorhersage_yyyy-mm-dd.csv} is uploaded to the server of
#'   Technologiestiftung Berlin (TSB).
#' @export
#'
update_data_and_predict <- function(
  day_string = as.character(Sys.Date()), upload = FALSE
)
{
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
    tiefwerder = prepare_tiefwerder(flows)
  )

  # Path to model input file
  input_file <- db_path("model_input.csv")

  # Write model input file
  write_input_file(model_input, input_file, "model input")

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
    get_two_site_prediction()

  # Save file with prediction of today locally
  file <- save_prediction_today(prediction_today, day_string)

  # Upload the locally saved file to the TSB server
  if (upload) {
    upload_prediction_today(file)
  }

  # Read "Vorhersagen.txt", append data, rewrite the file
  update_all_predictions(prediction_today)
}
