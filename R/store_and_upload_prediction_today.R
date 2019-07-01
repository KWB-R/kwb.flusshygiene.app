# store_and_upload_prediction_today --------------------------------------------
store_and_upload_prediction_today <- function(prediction_today, file)
{
  utils::write.csv(prediction_today, file, row.names = FALSE)

  url <- get_environment_variable("FTP_UPLOAD_TSB")
  user_pwd <- get_environment_variable("USER_PWD_TSB")

  user_pwd_parts <- strsplit(user_pwd, ":")[[1]]

  httr::POST(
    url = url,
    body = list(
      user = user_pwd_parts[1],
      pwd = user_pwd_parts[2],
      file = httr::upload_file(file)
    )
  )
}
