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
  #kwb.utils::assignPackageObjects("kwb.flusshygiene.app")

  # Set the subject (used in debug messages)
  subject <- "BWB data"

  # Create folder structure as necessary
  paths <- create_folder_structure(root)

  download_dir <- paths$downloads_bwb

  db_file_fst <- db_path(root, "rain-ruhleben.fst")
  db_file_csv <- db_path(root, "rain-ruhleben.csv")

  # Get paths to files that are available locally
  files_bwb <- dir(download_dir, "^Regenschreiber_.*\\.txt$", full.names = TRUE)

  if (length(files_bwb) == 0) {

    bwb_data <- NULL
    start_day <- as.Date("2017-06-22")

  } else {

    if (file.exists(db_file_fst)) {

      bwb_data <- fst::read.fst(db_file_fst)

    } else {

      bwb_data <- read_rain_from_files(files = files_bwb, dbg = dbg)

      write_fst_file(bwb_data, db_file_fst, subject)
      write_input_file(bwb_data, db_file_csv, subject)
    }

    start_day <- lubridate::as_date(min(bwb_data$tBeg))
  }

  # Define the files that have to be downloaded

  # - days that are already in the download folder
  existing_days <- extract_date_string(basename(files_bwb))

  # - all days from start day until today
  all_days <- day_strings_until_today(start_day)

  # - days that need to be downloaded
  missing_days <- setdiff(all_days, existing_days)

  # Download files from FTP-Server to a temporary directory and copy them to the
  # rain data directory
  downloaded_files <- ftp_download_bwb_files_of_days(
    missing_days, target_dir = download_dir, user_pwd = user_pwd
  )

  # Read the new files and update the rain "database"
  if (length(downloaded_files) == 0) {

    message("No files have been downloaded.")
    return()
  }

  bwb_data <- rbind(bwb_data, read_rain_from_files(
    files = file.path(download_dir, downloaded_files),
    dbg = dbg
  ))

  stopifnot(! is.unsorted(bwb_data$tBeg))
  stopifnot(sum(duplicated(bwb_data$tBeg)) == 0)

  # Save the updated rain "database" as fst and csv file
  write_fst_file(bwb_data, db_file_fst, subject)
  write_input_file(bwb_data, db_file_csv, subject)
}
