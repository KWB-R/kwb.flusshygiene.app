# read_flows -------------------------------------------------------------------

#' Read Flow Data from File
#'
#' @param file path to text file containing flow data
#' @param columns names of columns to be selected. Default:
#'   \code{c("DateTime", "Flow")}. Set to \code{NULL} to get all columns
#' @param switches passed to \code{\link[kwb.datetime]{textToEuropeBerlinPosix}}
#' @param fileEncoding encoding used in \code{file}. Default:
#'   \code{"Windows-1252"}
#' @param dbg if \code{TRUE} (default) debug messages are shown
#' @export
#' @importFrom kwb.utils catAndRun
read_flows <- function(
  file, columns = c("DateTime", "Flow"), switches = TRUE,
  fileEncoding = "Windows-1252", dbg = TRUE
)
{
  # Open a connection to the text file (in order to set the encoding)
  con <- file(file, encoding = fileEncoding)

  # Close the connection when exiting this function
  on.exit(close(con))

  # Read the file as text
  kwb.utils::catAndRun(
    dbg = dbg,
    sprintf(
      "Reading flow data from '%s/%s'", basename(dirname(file)), basename(file)
    ),
    expr = {

      # Read the file as text
      text_lines <- readLines(con)

      # Find the header rows in the file (exactly two expected!)
      header_rows <- find_header_rows(text_lines, file)

      stopifnot(length(header_rows) == 2)

      # Return a data frames with columns
      # - site: one of "sophienwerder", "tiefwerder"
      # - DateTime: POSIXct object in time zone Europe/Berlin
      # - Flow: flow in m3/s

      data_sophienwerder <- lines_to_flow_data(
        x = text_lines[header_rows[1]:(header_rows[2] - 1)],
        columns = columns,
        switches = switches
      )

      data_tiefwerder <- lines_to_flow_data(
        x = text_lines[header_rows[2]:(length(text_lines) - 1)],
        columns = columns,
        switches = switches
      )

      rbind(
        cbind(data_sophienwerder, site = "sophienwerder"),
        cbind(data_tiefwerder, site = "tiefwerder")
      )
    }
  )
}

# find_header_rows -------------------------------------------------------------
find_header_rows <- function(text_lines, file)
{
  # Find the header rows (rows containing "Datum")
  header_rows <- grep("^Datum", text_lines)

  # Return if not exactly two header rows were found
  if (length(header_rows) != 2) {

    message(
      "I did not find exactly two header rows containing 'Datum' ",
      "but ", length(header_rows), " header rows in:\n", file
    )

    return()
  }

  expected_header <- paste(collapse = ";", c(
    "Datum/Zeit",
    "Datum",
    "Zeit",
    "Wert [Kubikmeter pro Sekunde]",
    "Qualit\xe4tskennzeichen",
    "Interpolationstyp",
    "runoffvalue [Liter pro Sekunde pro Quadratkilometer]",
    "runoffvalue.status",
    "runoffvalue.intpol",
    "",
    "tagBemerkungen"
  ))

  Encoding(expected_header) <- "latin1"

  is_expected <- text_lines[header_rows] == expected_header

  if (! all(is_expected)) {
    message("Unexpected header(s):")
    print(text_lines[header_rows[! is_expected]])
    message("Expected:")
    print(expected_header)
  }

  header_rows
}

# lines_to_flow_data -----------------------------------------------------------
#' Lines to Flow Data
#'
#' @param x data frame
#' @param columns default: c("DateTime", "Flow")
#' @param switches default: TRUE
#'
#' @return Return only selected columns unless "columns" is NULL
#' @export
#'
#' @importFrom utils read.table
#' @importFrom kwb.datetime textToEuropeBerlinPosix
#' @importFrom kwb.utils selectColumns
lines_to_flow_data <- function(
  x, columns = c("DateTime", "Flow"), switches = TRUE
)
{
  colClasses <- c(rep("character", 3), rep("numeric", 6), rep("character", 2))

  data <- utils::read.table(
    text = x, sep = ";", dec = ",", header = TRUE, stringsAsFactors = FALSE,
    colClasses = colClasses, na.strings = "---"
  )

  names(data) <- c(
    "DateTime", "Date", "Time", "Flow", "Quality", "IntpolType",
    "RunoffValue", "RunoffStatus", "RunoffIntpol", "X", "Remarks"
  )

  # Remove column "X" that is expected to be empty
  data <- remove_column_expected_empty(data, "X")

  attr(data, "units") <- list(
    Flow = "m3/s",
    RunoffValue = "l/s/km2"
  )

  # Convert texts to timestamps with appropriate time conversion function
  data$DateTime <- kwb.datetime::textToEuropeBerlinPosix(
    data$DateTime, format = "%d.%m.%Y %H:%M:%S", switches = switches,
    dbg = FALSE
  )

  # Return only selected columns unless "columns" is NULL
  if (! is.null(columns)) {
    kwb.utils::selectColumns(data, columns)
  } else {
    data
  }
}
