# read_tiefwerder_data ---------------------------------------------------------
read_tiefwerder_data <- function(file)
{
  TW <- utils::read.csv(file, sep = ";", header = TRUE, dec = ".")

  TW$DateTime <- as.POSIXct(TW$DateTime)

  TW
}
