# read_model_input_csv ---------------------------------------------------------
#' Read Model Input Csv
#'
#' @param file file
#'
#' @return content
#' @export
#'
#' @importFrom utils read.csv
#' @importFrom kwb.utils selectColumns
read_model_input_csv <- function(file)
{
  content <- utils::read.csv(file, header = TRUE, sep = ";", dec = ".")

  content$Day <- as.POSIXct(kwb.utils::selectColumns(content, "Day"))

  content
}
