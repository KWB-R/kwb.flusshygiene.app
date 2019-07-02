# update_all_predictions -------------------------------------------------------
update_all_predictions <- function(prediction_today, root = get_root())
{
  paths <- create_folder_structure(root)

  file <- file.path(paths$predictions, "Vorhersagen.csv")

  if (exists(file)) {

    all_predictions <- utils::read.csv(file, sep = ";")

    write_input_file(
      x = rbind(all_predictions, prediction_today),
      file = file,
      subject = "all predictions"
    )

  } else {

    # Check if target directory exists
    kwb.utils::safePath(dirname(file))

    write_input_file(
      x = prediction_today,
      file = file,
      subject = "all predictions (initialised with prediction of today)"
    )
  }
}
