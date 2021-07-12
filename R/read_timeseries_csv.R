# read_timeseries_csv ----------------------------------------------------------
#' Read Timeseries Csv
#'
#' @param file file
#' @param time_columns default: c("tBeg", "tEnd")
#' @param switches default: TRUE
#'
#' @return data frame with timeseries
#' @export
#'
#' @importFrom kwb.utils selectColumns
#' @importFrom kwb.datetime textToEuropeBerlinPosix
read_timeseries_csv <- function(
  file, time_columns = c("tBeg", "tEnd"), switches = TRUE
)
{
  data <- utils::read.table(
    file, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE
  )

  data[time_columns] <- lapply(
    X = kwb.utils::selectColumns(data, time_columns, drop = FALSE),
    FUN = kwb.datetime::textToEuropeBerlinPosix,
    format = "%Y-%m-%d %H:%M:%S",
    switches = switches
  )

  data
}
