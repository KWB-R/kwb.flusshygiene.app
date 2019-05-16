# bind_and_clean ---------------------------------------------------------------

#' Row-Bind and Clean Time Series in Data Frames
#'
#' @param x list of data frames each of which contains a column \code{DateTime}
#' @export
#'
bind_and_clean <- function(x)
{
  x <- dplyr::bind_rows(x)

  x <- x[! is.na(x$DateTime), ] %>% dplyr::arrange(.data$DateTime)

  # Exclude duplicates by keeping rows with non-duplicated times
  x[! duplicated(x$DateTime), ]
}

# day_strings_until_today ------------------------------------------------------

#' Day Strings from Start Day Up to Today
#'
#' @param start_day start day as string (yyyy-mm-dd) or POSIXt object
#' @return vector of character containing date strings in format "yymmdd"
#' @export
#'
day_strings_until_today <- function(start_day)
{
  format(seq(as.Date(start_day), as.Date(Sys.time()), 1), "%y%m%d")
}

# download_file ----------------------------------------------------------------

#' Download a File
#'
#' @param url URL to the file to be downloaded
#' @param destfile full path to target file name
#' @param dbg show message about the downloading process if \code{TRUE}
#'
download_file <- function(url, destfile, dbg = TRUE)
{
  kwb.utils::catAndRun(
    dbg = dbg,
    messageText = paste("Downloading file to ", destfile),
    expr = utils::download.file(url, destfile, quiet = TRUE)
  )
}

# list_files_on_ftp_server -----------------------------------------------------
list_files_on_ftp_server <- function(user_pwd)
{
  url <- "ftp://ftp.kompetenz-wasser.de/"

  info_block <- RCurl::getURL(url, userpwd = user_pwd)

  info_lines <- unlist(strsplit(info_block, "\n-rw-r--r--"))

  # Extract column number 9 containing the file names
  sapply(strsplit(info_lines, "\\s+"), "[[", 9)
}

# write_input_file -------------------------------------------------------------

#' Write Data Frame to CSV File
#'
#' @param x data frame to be written to CSV file
#' @param file full path to target file
#' @export
#'
write_input_file <- function(x, file)
{
  utils::write.table(x, file, sep = ";", dec = ".", row.names = FALSE)
}
