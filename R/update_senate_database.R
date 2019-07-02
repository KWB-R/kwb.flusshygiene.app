# update_senate_database -------------------------------------------------------

#' Download New Files and Update Local "Database" of Senate's Data
#'
#' \enumerate{
#'   \item If database/flows.fst exists, load a data frame from there
#'   \item If database/flows.fst does not exist, read all files in
#'     downloads/senate/ into a data frame and save this data frame in
#'     database/flows.fst as well as in database/flows.csv
#'   \item Download one new file into downloads/senate/
#'   \item Read the downloaded file int a data frame containing data for both
#'     sites, Sophienwerder and Tiefwerder
#'   \item Row-bind the data frame read in 4) with the data frame loaded in 1)
#'     or read in 2)
#'   \item Save the data frame resulting from 5) in database/flows.fst as well
#'     as in database/flows.csv
#' }
#'
#' @param root path to "root" folder below which to find subfolders "downloads"
#'   and "database"
#' @export
#'
update_senate_database <- function(root = get_root())
{
  #kwb.utils::assignPackageObjects("kwb.flusshygiene.app")
  user_pwd <- get_environment_variable("USER_PWD_SENATE")

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

  # Path to flow database
  file_database <- db_path("flows.fst", root)

  # Does the database already exist?
  db_exists <- file.exists(file_database)

  if (db_exists && length(new_files) == 0) {

    message("Flow database exists and is up to date.")
    return()
  }

  # Determine the files that need to be read: only the new files or all files
  # if there is no database file yet
  files <- if (db_exists) new_files else files_after

  # Read data from the (new) files
  new_flows <- read_flows_from_files(files)

  # Set unplausible flow values to NA
  new_flows <- set_unplausible_flows_to_NA(new_flows)

  # Read existing data from fst files (NULL if fst files do not exist)
  old_flows <- if (db_exists) fst::read_fst(file_database)

  # Append new data
  flows <- rbind(old_flows, new_flows)

  # Update the database files (fst and csv)
  subject <- "flow data"
  write_fst_file(flows, file_database, subject)
  write_input_file(flows, db_path("flows.csv", root), subject)
}

# read_flows_from_files --------------------------------------------------------
read_flows_from_files <- function(files)
{
  # Read the new files into a list of data frames each of which contains the
  # data from one input file and each of which has data for both sites,
  # "sophienwerder" and "tiefwerder", indicated by the value in column "site".
  # Name the list elements by the file names
  flows_list <- stats::setNames(
    object = lapply(files, read_flows, columns = NULL),
    nm = basename(files)
  )

  # Exclude NULL elements (if a file did not contain exactly two headers)
  flows_list <- kwb.utils::excludeNULL(flows_list)

  # Row-bind the data frames and keep the file name in column "file"
  flows <- dplyr::bind_rows(.id = "file", flows_list)

  # Remove column "Remarks" if it is empty
  flows <- remove_column_expected_empty(flows, "Remarks")

  # Split flow data into two data frames, one for each site
  flows_by_site <- split(flows, flows$site)

  # Check for duplicates within each site
  partial_duplicates <- lapply(
    X = flows_by_site,
    FUN = kwb.utils::findPartialDuplicates,
    key_columns = "DateTime", skip_columns = "file"
  )

  has_partial_duplicates <- ! sapply(partial_duplicates, is.null)

  if (any(has_partial_duplicates)) {

    message("There are unexpected partial duplicates in the flow data:")

    for (site in names(which(has_partial_duplicates))) {
      message(sprintf("Site '%s':", site))
      print(partial_duplicates[[site]])
    }
  }

  # Remove duplicates
  is_duplicated <- duplicated(
    kwb.utils::selectColumns(flows, c("site", "DateTime"))
  )

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

  # Start the result data frame with a DateTime column containing all times
  seq_arguments <- c(as.list(range(flows$DateTime)), by = 60*15)
  result <- data.frame(DateTime = do.call(seq.POSIXt, seq_arguments))

  # Merge the flows at the different sites one by one
  for (site in names(flows_by_site)) {

    #site <- names(flows_by_site)[1]

    y <- kwb.utils::renameAndSelect(flows_by_site[[site]], list(
      "DateTime", "Flow" = paste0("Q.", site)
    ))

    result <- kwb.utils::catAndRun(
      sprintf("Merging flows at site '%s'", site),
      dplyr::left_join(result, y, by = "DateTime")
    )
  }

  result
}

# set_unplausible_flows_to_NA --------------------------------------------------
set_unplausible_flows_to_NA <- function(flows)
{
  # high negative flow on june 23th --> False measurement?
  which_too_low <- which(kwb.utils::selectColumns(flows, "Q.tiefwerder") < -10)

  if (length(which_too_low)) {

    print(flows[which_too_low, ])

    kwb.utils::catAndRun(
      messageText = sprintf(
        "\nSetting the flow at Tiefwerder %d-times to NA where flow < -10 (%s)",
        length(which_too_low), "see above"
      ),
      expr = flows$Q.tiefwerder[which_too_low] <- NA
    )
  }

  flows
}
