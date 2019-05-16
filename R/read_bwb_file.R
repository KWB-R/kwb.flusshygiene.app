# read_bwb_file ----------------------------------------------------------------

#' Read BWB Rain Data File
#'
#' @param file full path to rain data file
#' @param column_names names to be given to the columns of the returned rain
#'   data. Defaults to \code{kwb.flusshygiene.app:::get_bwb_rain_data_columns()}
#' @param dbg write debug messages if \code{TRUE}
#' @export
#'
read_bwb_file <- function(
  file, column_names = get_bwb_rain_data_columns(), dbg = TRUE
)
{
  file_name <- basename(file)

  x <- kwb.utils::catAndRun(dbg = dbg, paste("1. Reading", file_name), {
    read_bwb_file_raw(file, column_names)
  })

  x <- kwb.utils::catAndRun(dbg = dbg, paste("2. Formatting", file_name), {
    format_bwb_file_content(x)
  })

  x
}

# get_bwb_rain_data_columns ----------------------------------------------------

#' Read Names of BWB Rain Gauges from Example Rain Data File
#'
get_bwb_rain_data_columns <- function()
{
  package <- "kwb.flusshygiene.app"
  file <- system.file("extdata", "170622_head.txt", package = package)

  bwb_rain <- utils::read.csv(file, sep = "\t", stringsAsFactors = FALSE)

  columns <- strsplit(readLines(file, 1), "\t")[[1]]
  columns <- gsub(" ", ".", columns)
  columns[1:2] <- c("tBeg", "tEnd")

  columns
}

# read_bwb_file_raw ------------------------------------------------------------

#' Read Rain Data from Tab-Limited File and Give Column Names
#'
#' @param file full path to file from which to read
#' @param column_names vector of names to be given to the columns in the
#'   returned data frame
#'
read_bwb_file_raw <- function(file, column_names)
{
  x <- utils::read.csv(
    file = file, header = FALSE, skip = 1, sep = "\t", dec = ",",
    stringsAsFactors = FALSE,
    na.strings = "[-11059] No Good Data For Calculation"
  )

  stats::setNames(x, column_names)
}

# format_bwb_file_content ------------------------------------------------------
format_bwb_file_content <- function(x)
{
  # Replace all "" with NA
  x[x == ""] <- NA

  # Delete rows and columns with NA only
  x <- x[rowSums(is.na(x)) != ncol(x), ]
  x <- x[, colSums(is.na(x)) != nrow(x)]

  # Convert Mrz
  convert_march <- function(x) stringr::str_replace(x, "Mrz", "03")

  x$tBeg <- convert_march(x$tBeg)
  x$tEnd <- convert_march(x$tEnd)

  # Determine time conversion function
  FUN <- ifelse(
    is.na(lubridate::dmy_hm(x$tBeg[1], quiet = TRUE)),
    lubridate::dmy_hms,
    lubridate::dmy_hm
  )

  # Convert texts to timestamps with appropriate time conversion function
  x$tBeg <- FUN(x$tBeg)
  x$tEnd <- FUN(x$tEnd)

  # Helper function to replace comma with decimal point and convert to numeric
  to_numeric <- function(x) as.numeric(gsub(",", ".", x))

  # Convert all but first two columns to numeric
  x[, -c(1, 2)] <- apply(X = x[, -c(1, 2)], 2, FUN = to_numeric)

  x
}
