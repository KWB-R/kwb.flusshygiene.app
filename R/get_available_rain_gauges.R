# get_available_rain_gauges ----------------------------------------------------

#' Get Information on Available Rain Gauges
#'
#' @export
#' @importFrom utils read.csv
#' @importFrom kwb.utils noFactorDataFrame
get_available_rain_gauges <- function()
{
  package <- "kwb.flusshygiene.app"

  file <- system.file("extdata", "rain_gauges_bwb.csv", package = package)

  gauges <- utils::read.csv(
    file, header = TRUE, sep = ";", stringsAsFactors = FALSE
  )

  gauges <- gauges[gauges$FLUSSHYGIENE == "Ja", ]

  kwb.utils::noFactorDataFrame(
    Bez = gsub(" ", ".", gauges$PI_Bez),
    Gage = gauges$Bezeichnung
  )
}
