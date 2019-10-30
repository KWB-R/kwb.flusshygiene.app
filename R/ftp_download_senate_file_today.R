# ftp_download_senate_file_today -----------------------------------------------
ftp_download_senate_file_today <- function(dest_folder, dbg = TRUE)
{
  # Get download URL and credentials from environment variables
  ftp_url <- get_environment_variable("FTP_URL_SENATE")

  if (! nzchar(ftp_url)) {
    message("Skipping the download of files from Berlin Senate's FTP server.")
    return()
  }

  user_pwd <- get_environment_variable("USER_PWD_SENATE")

  url <- file.path(add_credentials(ftp_url, user_pwd), "ExportFlusshygiene.csv")

  today <- format(as.Date(Sys.time()), "%y%m%d")

  destfile <- file.path(dest_folder, sprintf("TW_SW_%s.txt", today))

  download_file(url, destfile)
}
