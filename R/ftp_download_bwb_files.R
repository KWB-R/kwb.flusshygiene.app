# ftp_download_bwb_files_of_days -----------------------------------------------
ftp_download_bwb_files_of_days <- function(
  missing_days, target_dir, user_pwd, dbg = TRUE
)
{
  # - days that are available for download
  ftp_url <- "ftp://ftp.kompetenz-wasser.de/"
  ftp_files_all <- kwb.dwd::list_url(ftp_url, userpwd = user_pwd)
  ftp_files <- grep("^Regenschreiber_", ftp_files_all, value = TRUE)

  # Files that need to be downloaded
  missing_files <- ftp_files[extract_date_string(ftp_files) %in% missing_days]

  if (length(missing_files) == 0) {

    message("Already up to date.")
    return()
  }

  urls <- paste0(ftp_url, missing_files)

  credential_urls <- gsub("://", sprintf("://%s@", user_pwd), urls)

  # Loop through URLs with credentials added
  for (url in credential_urls) {

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
