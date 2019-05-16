# read_rain_from_files ---------------------------------------------------------
read_rain_from_files <- function(files, dbg = TRUE)
{
  # Read all those files and give consistent column names (dangerous!)
  rain_data <- lapply(
    files,
    read_bwb_file,
    column_names = get_bwb_rain_data_columns(),
    dbg = dbg
  )

  # 2. Combine all file contents to one data frame
  rain_data <- dplyr::bind_rows(rain_data)

  # Delete rows with duplicated begin times and reorder by time
  remove_duplicates_and_reorder(rain_data)
}

# remove_duplicates_and_reorder ------------------------------------------------
remove_duplicates_and_reorder <- function(rain_data)
{
  rain_data[! duplicated(rain_data$tBeg), ] %>%
    dplyr::arrange(.data$tBeg)
}
