# add_day_column_from ----------------------------------------------------------
add_day_column_from <- function(df, column)
{
  df$Day <- lubridate::as_date(kwb.utils::selectColumns(df, column))

  df
}

# clean_stop -------------------------------------------------------------------
clean_stop <- function(...)
{
  stop(call. = FALSE, ...)
}

# db_path ----------------------------------------------------------------------

#' Path to "Database" File
#'
#' @param root path to "root" directory of the application
#' @param file file name within application's "database" folder
#'
#' @export
#'
db_path <- function(file, root = get_root())
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
    messageText = paste("Downloading file to\n ", destfile),
    expr = utils::download.file(url, destfile, quiet = TRUE)
  )
}

# get_environment_variable -----------------------------------------------------

#' Get Environment Variable Giving a Hint on Missing Variables
#'
#' @param x name of environment variable
#' @param do_stop logical. If \code{TRUE} an error is raised if the environment
#'   variable is not set. Otherwise (the default), a message is shown.
#' @export
#'
get_environment_variable <- function(x, do_stop = FALSE)
{
  content <- Sys.getenv(x)

  if (content == "") {

    text <- paste0(
      sprintf("There is no such environment variable '%s'.\n", x),
      "Use the following command to set the variable:\n",
      sprintf("Sys.setenv(%s = \"<value-of-%s>\")", x, x)
    )

    if (do_stop) {

      clean_stop(text)

    } else {

      message(text)
    }
  }

  content
}

# get_root ---------------------------------------------------------------------
get_root <- function()
{
  path.expand(get_environment_variable("FLUSSHYGIENE_ROOT", do_stop = TRUE))
}

# message_updating -------------------------------------------------------------
message_updating <- function(context, root)
{
  message("\nUpdating the ", context, " (root folder: ", root, ")\n")
}

# remove_column_expected_empty -------------------------------------------------
remove_column_expected_empty <- function(df, column, dbg = FALSE)
{
  if (all(kwb.utils::isNaOrEmpty(kwb.utils::selectColumns(df, column)))) {

    return(kwb.utils::removeColumns(df, column, dbg = dbg))
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

# set_root ---------------------------------------------------------------------

#' Set the Root Folder of the Flusshygiene App
#'
#' The root folder will be stored in the environment variable
#' "FLUSSHYGIENE_ROOT"
#'
#' @param root path to the root folder
#' @export
#'
set_root <- function(root)
{
  Sys.setenv(FLUSSHYGIENE_ROOT = root)
}

# write_fst_file ---------------------------------------------------------------
write_fst_file <- function(x, file, context = deparse(substitute(x)))
{
  kwb.utils::catAndRun(
    sprintf("Writing %s to\n  '%s'", context, file),
    fst::write_fst(x, file)
  )
}

# write_input_file -------------------------------------------------------------

#' Write Data Frame to CSV File
#'
#' @param x data frame to be written to CSV file
#' @param file full path to target file
#' @param context text describing the kind of data that is written. This text
#'   will appear in the debug message
#' @param sep column separator, default: ";"
#' @export
#'
write_input_file <- function(
  x, file, context = deparse(substitute(x)), sep = ";"
)
{
  kwb.utils::catAndRun(
    sprintf("Writing %s to\n  '%s'", context, file),
    utils::write.table(x, file, sep = sep, dec = ".", row.names = FALSE)
  )
}
