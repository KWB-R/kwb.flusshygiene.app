# save_prediction_today --------------------------------------------------------
save_prediction_today <- function(
  prediction_today, today_string, root = get_root()
)
{
  paths <- create_folder_structure(root)

  filename <- sprintf("Vorhersage_%s.csv", today_string)
  file <- file.path(paths$predictions, filename)

  utils::write.csv(prediction_today, file, row.names = FALSE)

  file
}
