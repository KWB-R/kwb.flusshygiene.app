# ftp_download_bwb_files_of_days -----------------------------------------------
ftp_download_bwb_files_of_days <- function(missing_days, target_dir, dbg = TRUE)
{
  # Get download URL and credentials from environment variables
  ftp_url <- get_environment_variable("FTP_URL_KWB")

  if (! nzchar(ftp_url)) {
    message("Skipping the download of files from KWB's FTP server.")
    return(character())
  }

  user_pwd <- get_environment_variable("USER_PWD_KWB")

  # - days that are available for download
  ftp_files_all <- kwb.dwd::list_url(ftp_url, userpwd = user_pwd, dbg = FALSE)
  ftp_files <- grep("^Regenschreiber_", ftp_files_all, value = TRUE)

  # Files that need to be downloaded
  missing_files <- ftp_files[extract_date_string(ftp_files) %in% missing_days]

  if (length(missing_files) == 0) {

    message("Already up to date.")
    return()
  }

  urls <- file.path(ftp_url, missing_files)

  # Loop through URLs with credentials added
  for (url in add_credentials(urls, user_pwd)) {

    download_file(url, file.path(target_dir, basename(url)), dbg = dbg)
  }

  missing_files
}

# extract_date_string ----------------------------------------------------------
extract_date_string <- function(x)
{
  start <- nchar("Regenschreiber_") + 1

  substr(x, start, start + 5)
}
