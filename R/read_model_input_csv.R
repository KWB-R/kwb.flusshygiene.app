# read_model_input_csv ---------------------------------------------------------
read_model_input_csv <- function(file)
{
  content <- utils::read.csv(file, header = TRUE, sep = ";", dec = ".")

  content$Day <- as.POSIXct(kwb.utils::selectColumns(content, "Day"))

  content
}
