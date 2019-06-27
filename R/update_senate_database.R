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

  # Create folder structure as necessary
  paths <- create_folder_structure(root)

  download_dir <- paths$downloads_senate

  # List all files that are available locally
  files_before <- dir(download_dir, full.names = TRUE)

  # Download new data from the senate's ftp server
  ftp_download_senate_file_today(download_dir, user_pwd)

  # List all files that are available locally
  files_after <- dir(download_dir, full.names = TRUE)

  # Determine the new files that have been downloaded
  new_files <- setdiff(files_after, files_before)

  if (length(new_files) == 0) {

    message("Already up to date.")
    return()
  }

  # Read data from the new files
  new_flows <- read_flows_from_files(files = new_files)

  # Define paths to fst files
  file_database <- db_path(root, "flows.fst")

  # Read existing data from fst files (NULL if fst files do not exist)
  old_flows <- if (file.exists(file_database)) fst::read_fst(file_database)

  # Append new data
  flows <- rbind(old_flows, new_flows)

  # Update the database files (fst and csv)
  fst::write_fst(flows, file_database)
  write_input_file(flows, db_path(root, "flows.csv"))
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
  # Read the new files into a list of data frames each of which contains the
  # data from one input file and each of which has data for both sites,
  # "sophienwerder" and "tiefwerder", indicated by the value in column "site".
  # Name the list elements by the file names
  flows_list <- stats::setNames(lapply(files, read_flows), nm = basename(files))

  # Exclude NULL elements (if a file did not contain exactly two headers)
  flows_list <- kwb.utils::excludeNULL(flows_list)

  # Row-bind the data frames and keep the file name in column "file"
  flows <- dplyr::bind_rows(.id = "file", flows_list)

  # Split flow data into two data frames, one for each site
  flows_by_site <- split(flows, flows$site)

  # Check for duplicates within each site
  partial_duplicates <- lapply(
    X = flows_by_site,
    FUN = kwb.utils::findPartialDuplicates,
    key_columns = c("file", "DateTime", "site")
  )

  if (any(! sapply(partial_duplicates, is.null))) {

    message("There are unexpected partial duplicates in the flow data:")
    cat(capture.output(str(partial_duplicates)))
  }

  # Remove duplicates
  is_duplicated <- duplicated(kwb.utils::selectColumns(flows, c("site", "DateTime")))

  if (any(is_duplicated)) {

    flows <- kwb.utils::catAndRun(
      sprintf("Removing %d duplicated rows in flow data", sum(is_duplicated)),
      flows[! is_duplicated, ]
    )

    flows_by_site <- split(flows, flows$site)
  }

  message("Time ranges with constant time step:")
  print(lapply(flows_by_site, function(data) kwb.datetime::getEqualStepRanges(
    data$DateTime
  )))

  message("Range of flow values:")
  print(lapply(flows_by_site, function(data) range(data$Flow)))

  # high negative flow on june 23th --> False measurement?
  which_too_low <- which(flows$site == "tiefwerder" & flows$Flow < -10)

  if (length(which_too_low)) {

    kwb.utils::catAndRun(
      messageText = sprintf(
        "Setting Flow %d-times to NA where Flow < -10", length(which_too_low)
      ),
      expr = flows$Flow[which_too_low] <- NA
    )
  }

  flows
}
