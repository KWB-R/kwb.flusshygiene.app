# update_senate_database -------------------------------------------------------

#' Download New Files and Update Local "Database" of Senate's Data
#'
#' @param root path to "root" folder below which to find subfolders "downloads"
#'   and "database"
#' @param user_pwd user and password string to access KWB's FTP server
#' @export
#'
update_senate_database <- function(root, user_pwd)
{
  #kwb.utils::assignPackageObjects("kwb.flusshygiene.app")

  # Create download folder if necessary
  path_senate <- kwb.utils::createDirectory(file.path(root, "downloads", "senate"))

  # Create database folder if necessary
  kwb.utils::createDirectory(file.path(root, "database"))

  # List all files that are available locally
  files_before <- dir(path_senate, full.names = TRUE)

  # Download new data from the senate's ftp server
  ftp_download_senate_file_today(path_senate, user_pwd)

  # List all files that are available locally
  files_after <- dir(path_senate, full.names = TRUE)

  # Determine the new files that have been downloaded
  new_files <- setdiff(files_after, files_before)

  if (length(new_files) == 0) {

    message("Already up to date.")
    return()
  }

  # Read data from the new files
  new_data_list <- read_flows_from_files(files = new_files)

  # Define paths to fst files
  file_database_TW <- db_path(root, "flow-tiefwerder.fst")
  file_database_SW <- db_path(root, "flow-sophienwerder.fst")

  # Read existing data from fst files (NULL if fst files do not exist)
  data_TW <- if (file.exists(file_database_TW)) fst::read_fst(file_database_TW)
  data_SW <- if (file.exists(file_database_SW)) fst::read_fst(file_database_SW)

  # Append new data
  data_TW <- merge_flow_data(data_TW, new_data_list$TW)
  data_SW <- merge_flow_data(data_SW, new_data_list$SW)

  # Update the databases (fst files)
  fst::write_fst(data_TW, file_database_TW)
  fst::write_fst(data_SW, file_database_SW)

  # Update the databases (CSV files)
  write_input_file(data_TW, db_path(root, "flow-tiefwerder.csv"))
  write_input_file(data_SW, db_path(root, "flow-sophienwerder.csv"))
}

# merge_flow_data --------------------------------------------------------------
merge_flow_data <- function(data, new_data)
{
  result <- dplyr::bind_rows(data, new_data)
  result[! duplicated(result$DateTime), ] %>%
    dplyr::arrange(.data$DateTime)
}

# read_flows_from_files --------------------------------------------------------
read_flows_from_files <- function(files)
{
  # Read the new files into a list of lists. Each sublist contains two
  # data frames, one for Tiefwerder (TW) and one for Sophienwerder (SW).
  # Exclude NULL elements (if a file did not exactly contain two headers)
  Q_list <- kwb.utils::excludeNULL(lapply(files, read_flows))

  # 3. Collect the "TW" elements and the SW elements and combine and clean them
  TW <- bind_and_clean(lapply(Q_list, kwb.utils::selectElements, "TW"))
  SW <- bind_and_clean(lapply(Q_list, kwb.utils::selectElements, "SW"))

  # high negative flow on june 23th --> False measurement?
  TW[which(TW$Flow < -10), ] <- NA

  list(TW = TW, SW = SW)
}
