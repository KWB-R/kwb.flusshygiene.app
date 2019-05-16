# read_flows -------------------------------------------------------------------

#' Read Flow Data from File
#'
#' @param file path to text file containing flow data
#' @export
#'
read_flows <- function(file)
{
  # Read the file as text c.names.Q
  Q_lines <- readLines(file, warn = FALSE)

  # Find the header rows (rows containing "Datum")
  header_rows <- grep("Datum", substring(Q_lines, first = 1, last = 5))

  # Return if not exactly two header rows were found
  if (length(header_rows) != 2) {

    message(
      "I did not find exactly two header rows containing 'Datum' ",
      "but ", length(header_rows), " header rows"
    )

    return()
  }

  # Determine the row numbers belonging to Sophienwerder (SW) and Tiefwerder
  # (TW), respectively
  rows_SW <- header_rows[1]:(header_rows[2] - 1)
  rows_TW <- header_rows[2]:(length(Q_lines) - 1)

  # Return a list of two data frames (one representing Sopienwerder and one
  # representing Tiefwerder), each of which has two columns: "DateTime" and
  # "Flow"
  list(
    SW = lines_to_flow_data(Q_lines[rows_SW]),
    TW = lines_to_flow_data(Q_lines[rows_TW])
  )
}

# lines_to_flow_data -----------------------------------------------------------
lines_to_flow_data <- function(x)
{
  Q <- utils::read.csv(
    text = paste(x, collapse = "\n"), header = TRUE, sep = ";", dec = ",",
    na = "---", stringsAsFactors = FALSE
  )

  # Select and name DateTime and Flow column
  Q <- stats::setNames(Q[c(1, 4)], c("DateTime", "Flow"))

  # Determine time conversion function
  FUN <- ifelse(
    is.na(lubridate::dmy_hm(Q$DateTime[1], quiet = TRUE)),
    lubridate::dmy_hms,
    lubridate::dmy_hm
  )

  # Convert texts to timestamps with appropriate time conversion function
  Q$DateTime <- FUN(Q$DateTime)

  Q
}
