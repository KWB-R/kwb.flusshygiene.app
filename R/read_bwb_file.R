# read_bwb_file ----------------------------------------------------------------

#' Read BWB Rain Data File
#'
#' @param file full path to rain data file
#' @param first_day_only if \code{TRUE} (the default) only the data
#'   of the first day in the file is kept and the data belonging to further days
#'   are removed. Otherwise everything is returned.
#' @param dbg write debug messages if \code{TRUE}
#' @export
#'
read_bwb_file <- function(file, first_day_only = TRUE, dbg = TRUE)
{
  # Read CSV file with first two columns character and all others numeric
  x <- utils::read.csv(
    file = file, header = TRUE, sep = "\t", dec = ",",
    fileEncoding = "Windows-1252", stringsAsFactors = FALSE,
    na.strings = c("[-11059]NoGoodDataForCalculation", "Resizetoshowallvalues"),
    colClasses = c(rep("character", 2), rep("numeric", 43))
  )

  # Check that the file has the structure that we expect
  stopifnot(identical(names(x)[1:2], c("Niederschlag", "X")))
  stopifnot(ncol(x) == 45)

  # Name the first two columns
  names(x)[1:2] <- c("tBeg", "tEnd")

  # Do not expect the time column to be NA
  if (any(is.na(x$tBeg))) {
    stop("tBeg column contains NA!")
  }

  if (first_day_only) {

    # Extract the date parts of the timestamps
    daystrings <- substr(x$tBeg, 1, 9)

    # The time series of two consecutive files overlap. Keep only the data that
    # refers to the day (delete the last rows representing the next day)
    x <- kwb.utils::catAndRun(
      dbg = dbg,
      sprintf("Keeping only rows of day '%s'", daystrings[1]),
      x[daystrings == daystrings[1], ]
    )
  }

  # Define function that converts the text timestamps to POSIXct objects
  to_posix <- function(xx) {
    kwb.datetime::textToEuropeBerlinPosix(
      dbg = dbg,
      x = kwb.utils::multiSubstitute(xx, list(
        Jan = "01", Feb = "02", Mrz = "03", Apr = "04", Mai = "05", Jun = "06",
        Jul = "07", Aug = "08", Sep = "09", Okt = "10", Nov = "11", Dez = "12"
      )),
      format = "%d-%m-%y %H:%M:%S"
    )
  }

  # Convert texts to timestamps
  x$tBeg <- to_posix(x$tBeg)
  x$tEnd <- to_posix(x$tEnd)

  # Check the types
  stopifnot(all(sapply(x, inherits, "POSIXct")[1:2]))
  stopifnot(all(sapply(x, is.numeric)[-(1:2)]))

  # Check for duplicated times
  if (! kwb.datetime::isValidTimestampSequence(x$tBeg)) {
    message("Column 'tBeg' does not contain a valid timestamp sequence.")
  }

  x
}
