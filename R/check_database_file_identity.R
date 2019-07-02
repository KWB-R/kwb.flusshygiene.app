# check_database_file_identity -------------------------------------------------

#' Check Identity of Database Files (fst versus csv)
#'
#' @export
#'
check_database_file_identity <- function()
{
  # Check rain and Ruhleben data
  rain_ruhleben_ok <- check_identity_fst_csv(
    file_fst = db_path("rain-ruhleben.fst"),
    file_csv = db_path("rain-ruhleben.csv")
  )

  flows_ok <- check_identity_fst_csv(
    file_fst = db_path("flows.fst"),
    file_csv = db_path("flows.csv"),
    time_columns = "DateTime"
  )

  rain_ruhleben_ok && flows_ok
}

# check_identity_fst_csv -------------------------------------------------------
check_identity_fst_csv <- function(file_fst, file_csv, ...)
{
  # Check if we read exactly the same from the CSV file
  if (identical(fst::read_fst(file_fst), read_timeseries_csv(file_csv, ...))) {
    return(TRUE)
  }

  message("The contents of\n  ", file_fst, "\nand\n  ", file_csv, "differ!")
  FALSE
}
