# update_bwb_database ----------------------------------------------------------

#' Download New Files and Update Local "Database"
#'
#' This function checks for files that are available on KWB's FTP server and
#' that are not yet contained in the download folder <root>/downloads/bwb/. It
#' downloads new files from the FTP server into the download folder. New data
#' are read from the downloaded files and appended to the "database", i.e. to
#' two files in <root>/database/: 'rain-ruhleben.fst' (to be read with
#' fst::read.fst) and 'rain-ruhleben.csv'.
#'
#' @param root path to "root" folder below which to find subfolders "downloads"
#'   and "database"
#' @param user_pwd user and password string to access KWB's FTP server
#' @param dbg if \code{TRUE} debug messages are shown
#' @export
#'
update_bwb_database <- function(root, user_pwd, dbg = TRUE)
{
  # Create folder structure as necessary
  paths <- create_folder_structure(root)

  download_dir <- paths$downloads_bwb

  file_bwb_database_fst <- db_path(root, "rain-ruhleben.fst")
  file_bwb_database_csv <- db_path(root, "rain-ruhleben.csv")

  # Get paths to files that are available locally
  files_bwb <- dir(download_dir, full.names = TRUE)

  if (length(files_bwb) == 0) {

    bwb_data <- NULL
    start_day <- as.Date("2017-06-22")

  } else {

    if (file.exists(file_bwb_database_fst)) {

      bwb_data <- fst::read.fst(file_bwb_database_fst)

    } else {

      bwb_data <- read_rain_from_files(files = files_bwb, dbg = dbg)

      fst::write_fst(bwb_data, file_bwb_database_fst)
    }

    start_day <- lubridate::as_date(min(bwb_data$tBeg))
  }

  # Define the files that have to be downloaded

  # - days that have been downloaded
  existing_days <- extract_date_string(basename(files_bwb))

  # - all days from start day until today
  all_days <- day_strings_until_today(start_day)

  # - days that need to be downloaded
  missing_days <- setdiff(all_days, existing_days)

  # - days that are available for download
  url <- sprintf("ftp://%s@ftp.kompetenz-wasser.de/", user_pwd)
  ftp_files <- grep("^Regenschreiber_", kwb.dwd::list_url(url), value = TRUE)

  # Files that need to be downloaded
  missing_files <- ftp_files[extract_date_string(ftp_files) %in% missing_days]

  if (length(missing_files) == 0) {

    message("Already up to date.")
    return()
  }

  # Download files from FTP-Server to a temporary directory and copy them to the
  # rain data directory
  download_files_from_ftp_server(
    missing_files,
    target_dir = download_dir,
    user_pwd = user_pwd
  )

  # Read the new files and update the rain "database"
  bwb_data_new <- read_rain_from_files(
    files = file.path(download_dir, missing_files),
    dbg = dbg
  )

  bwb_data <- dplyr::bind_rows(bwb_data, bwb_data_new)
  # %>% remove_duplicates_and_reorder()

  stopifnot(! is.unsorted(bwb_data$tBeg))
  stopifnot(sum(duplicated(bwb_data$tBeg)) == 0)

  # Save the updated rain "database"
  fst::write_fst(bwb_data, file_bwb_database_fst)

  # Save rain "database" as CSV
  write_input_file(bwb_data, file_bwb_database_csv)
}
