# update_all_predictions -------------------------------------------------------
update_all_predictions <- function(prediction_today, file)
{
  if (exists(file)) {

    all_predictions <- utils::read.csv(file, sep = ";")

    write_input_file(
      x = rbind(all_predictions, prediction_today),
      file = file,
      subject = "all predictions"
    )

  } else {

    write_input_file(
      x = prediction_today,
      file = file,
      subject = "all predictions (initialised with prediction of today)"
    )
  }
}
