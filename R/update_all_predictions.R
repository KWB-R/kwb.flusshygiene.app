# update_all_predictions -------------------------------------------------------
#' Update All Predictions
#'
#' @param prediction_today prediction_today
#' @param root default: \link{get_root}
#'
#' @return update all predictions
#' @export
#'
#' @importFrom utils read.csv
#' @importFrom kwb.utils safePath selectColumns
#' @importFrom dplyr arrange
update_all_predictions <- function(prediction_today, root = get_root())
{
  #kwb.utils::assignPackageObjects("kwb.flusshygiene.app")
  paths <- create_folder_structure(root)

  filename <- "Vorhersagen.csv"
  file <- file.path(paths$predictions, filename)

  if (file.exists(file)) {

    all_predictions <- utils::read.csv(file, sep = ";")

    # Delete possible existing predictions
    all_date_strings <- kwb.utils::selectColumns(all_predictions, "Datum")
    new_date_strings <- kwb.utils::selectColumns(prediction_today, "Datum")

    is_to_be_replaced <- all_date_strings %in% new_date_strings

    if (any(is_to_be_replaced)) {

      all_predictions <- kwb.utils::catAndRun(
        messageText = sprintf(
          "Overwriting %d rows in '%s'", sum(is_to_be_replaced), filename
        ),
        expr = all_predictions[! is_to_be_replaced, ]
      )
    }

    all_predictions <- rbind(all_predictions, prediction_today) %>%
      dplyr::arrange(.data$Datum)

    write_input_file(
      x = all_predictions,
      file = file,
      context = "all predictions"
    )

  } else {

    # Check if target directory exists
    kwb.utils::safePath(dirname(file))

    write_input_file(
      x = prediction_today,
      file = file,
      context = "all predictions (initialised with prediction of today)"
    )
  }
}
