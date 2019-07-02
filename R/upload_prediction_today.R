# upload_prediction_today ------------------------------------------------------
upload_prediction_today <- function(file)
{
  # Get download URL and credentials from environment variables
  url <- get_environment_variable("FTP_URL_TSB")
  user_pwd <- get_environment_variable("USER_PWD_TSB")

  user_pwd_parts <- strsplit(user_pwd, ":")[[1]]

  kwb.utils::catAndRun(
    messageText = sprintf("Uploading '%s' to the TSB server", file),
    expr = httr::POST(
      url = url,
      body = list(
        user = user_pwd_parts[1],
        pwd = user_pwd_parts[2],
        file = httr::upload_file(file)
      )
    )
  )
}
