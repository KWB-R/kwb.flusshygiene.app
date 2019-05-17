# read_bwb_data ----------------------------------------------------------------
read_bwb_data <- function(file)
{
  bwb_data <- utils::read.csv(
    file, header = TRUE, sep = ";", dec = ".", stringsAsFactors = FALSE
  )

  for (column in names(bwb_data)) {

    x <- bwb_data[[column]]

    bwb_data[[column]] <- if (column %in% c("tBeg", "tEnd")) {
      as.POSIXct(x, format = "%Y-%m-%d %H:%M")
    } else {
      as.numeric(x)
    }
  }

  bwb_data
}
