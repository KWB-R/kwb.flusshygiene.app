# read_timeseries_csv ----------------------------------------------------------
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
