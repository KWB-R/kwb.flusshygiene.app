# read_flows -------------------------------------------------------------------

#' Read Flow Data from File
#'
#' @param file path to text file containing flow data
#' @param columns names of columns to be selected. Default:
#'   \code{c("DateTime", "Flow")}. Set to \code{NULL} to get all columns
#' @param switches passed to \code{\link[kwb.datetime]{textToEuropeBerlinPosix}}
#' @param fileEncoding encoding used in \code{file}. Default:
#'   \code{"Windows-1252"}
#' @export
#'
read_flows <- function(
  file, columns = c("DateTime", "Flow"), switches = TRUE,
  fileEncoding = "Windows-1252"
)
{
  # Open a connection to the text file (in order to set the encoding)
  con <- file(file, encoding = fileEncoding)
  on.exit(close(con))

  # Read the file as text c.names.Q
  text_lines <- readLines(con)

  # Find the header rows in the file (exactly two expected!)
  header_rows <- find_header_rows(text_lines, file)

  stopifnot(length(header_rows) == 2)

  # Determine the row numbers belonging to Sophienwerder (SW) and Tiefwerder
  # (TW), respectively
  text_sophienwerder <- text_lines[header_rows[1]:(header_rows[2] - 1)]
  text_tiefwerder <- text_lines[header_rows[2]:(length(text_lines) - 1)]

  # Return a list of two data frames (one representing Sopienwerder and one
  # representing Tiefwerder), each of which has two columns: "DateTime" and
  # "Flow"
  list(
    SW = lines_to_flow_data(text_sophienwerder, columns, switches),
    TW = lines_to_flow_data(text_tiefwerder, columns, switches)
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

  attr(data, "units") <- list(
    Flow = "m3/s",
    RunoffValue = "l/s/km2"
  )

  # Convert texts to timestamps with appropriate time conversion function
  data$DateTime <- kwb.datetime::textToEuropeBerlinPosix(
    data$DateTime, switches = switches
  )

  # Return only selected columns unless "columns" is NULL
  if (! is.null(columns)) {
    kwb.utils::selectColumns(data, columns)
  } else {
    data
  }
}
