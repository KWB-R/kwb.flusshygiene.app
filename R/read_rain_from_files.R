# read_rain_from_files ---------------------------------------------------------
read_rain_from_files <- function(files, dbg = TRUE)
{
  # Read all those files and give consistent column names (dangerous!)
  rain <- lapply(files, read_bwb_file, dbg = dbg)

  # Name the list elements by the corresponding file names
  names(rain) <- basename(files)

  # 2. Combine all file contents to one data frame
  rain <- dplyr::bind_rows(rain, .id = "file")

  # There should not be duplicates and the data should be sorted by time!
  stopifnot(! any(duplicated(rain$tBeg)))
  stopifnot(! is.unsorted(rain$tBeg))

  kwb.utils::printIf(dbg, kwb.datetime::getEqualStepRanges(rain$tBeg))

  rain
}
