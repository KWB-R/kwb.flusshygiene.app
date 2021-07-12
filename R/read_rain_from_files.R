# read_rain_from_files ---------------------------------------------------------
#' Read Rain from Files
#'
#' @param files files
#' @param dbg default: TRUE
#'
#' @return data frame with imported rain data
#' @export
#'
#' @importFrom kwb.utils catAndRun printIf removeColumns
#' @importFrom dplyr bind_rows
#' @importFrom kwb.datetime getEqualStepRanges
read_rain_from_files <- function(files, dbg = TRUE)
{
  # Read all given files
  rain <- lapply(seq_along(files), function(i) {

    file <- files[i]

    kwb.utils::catAndRun(
      sprintf("Reading file %d/%d: '%s'", i, length(files), basename(file)),
      expr = read_bwb_file(file, dbg = FALSE)
    )
  })

  # Name the list elements by the corresponding file names
  names(rain) <- basename(files)

  # Combine all file contents in one data frame
  rain <- kwb.utils::catAndRun(
    sprintf("Combining contents of %d files", length(rain)), {
      dplyr::bind_rows(rain, .id = "file")
    }
  )

  # There should not be duplicates and the data should be sorted by time!
  stopifnot(! any(duplicated(rain$tBeg)))
  stopifnot(! is.unsorted(rain$tBeg))

  kwb.utils::printIf(dbg > 1, kwb.datetime::getEqualStepRanges(rain$tBeg))

  # Remove (hopefully) empty column "X.1" and remove column "file"
  remove_column_expected_empty(rain, "X.1") %>%
    kwb.utils::removeColumns("file", )
}
