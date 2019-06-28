# add_day_column_from ----------------------------------------------------------
add_day_column_from <- function(df, column)
{
  date_time <- kwb.utils::selectColumns(df, column)

  kwb.utils::setColumns(df, Day = lubridate::as_date(date_time))
}

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

# db_path ----------------------------------------------------------------------

#' Path to "Database" File
#'
#' @param root path to "root" directory of the application
#' @param file file name within application's "database" folder
#'
#' @export
#'
db_path <- function(root, file)
{
  file.path(root, "database", file)
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

# get_environment_variable -----------------------------------------------------

#' Get Environment Variable Giving a Hint on Missing Variables
#'
#' @param x name of environment variable
#' @export
#'
get_environment_variable <- function(x)
{
  content <- Sys.getenv(x)

  if (content == "") {

    message(
      sprintf("There is no such environment variable '%s'.\n", x),
      "Use the following command to set the variable:\n",
      sprintf("Sys.setenv(%s = \"<value-of-%s>\")", x, x)
    )
  }

  content
}

# remove_column_expected_empty -------------------------------------------------
remove_column_expected_empty <- function(df, column, dbg = FALSE)
{
  if (all(kwb.utils::isNaOrEmpty(kwb.utils::selectColumns(df, column)))) {

    return(kwb.utils::removeColumns(df, column, dbg = dbg, ))
  }

  message(sprintf(
    "Column '%s' is not empty as expected and thus kept!", column
  ))

  df
}

# round_2 ----------------------------------------------------------------------
round_2 <- function(x)
{
  round(x, digits = 2)
}

# write_fst_file ---------------------------------------------------------------
write_fst_file <- function(x, file, subject = deparse(substitute(x)))
{
  kwb.utils::catAndRun(
    sprintf("Writing %s to '%s'", subject, file),
    fst::write_fst(x, file)
  )
}

# write_input_file -------------------------------------------------------------

#' Write Data Frame to CSV File
#'
#' @param x data frame to be written to CSV file
#' @param file full path to target file
#' @param subject text describing the kind of data that is written. This text
#'   will appear in the debug message
#' @export
#'
write_input_file <- function(x, file, subject = deparse(substitute(x)))
{
  kwb.utils::catAndRun(
    sprintf("Writing %s to '%s'", subject, file),
    utils::write.table(x, file, sep = ";", dec = ".", row.names = FALSE)
  )
}
