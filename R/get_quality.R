# get_quality ------------------------------------------------------------------
get_quality <- function(prediction)
{
  structure_as_text <- function(x) paste(
    collapse = "\n",
    utils::capture.output(utils::str(x))
  )

  if (nrow(prediction) != 1) clean_stop(
    "get_quality() expects a data frame with exactly one row as input ",
    "but was given: ", structure_as_text(prediction)
  )

  qualities <- character()

  if (prediction[1, 4] > 900) {
    qualities <- c(qualities, "mangelhaft")
  }

  if (prediction[1, 4] < 900 & prediction[1, 5] > 1000) {
    qualities <- c(qualities, "ausreichend")
  }

  if (prediction[1, 5] < 1000 & prediction[1, 5] > 500) {
    qualities <- c(qualities, "gut")
  }

  if (prediction[1, 5] < 500) {
    qualities <- c(qualities, "ausgezeichnet")
  }

  if (length(qualities) == 0) clean_stop(
    "No quality could be determined from these percentiles: ",
    structure_as_text(prediction)
  )

  quality <- qualities[length(qualities)]

  if (length(qualities) > 1) message(sprintf(
    "More than one quality determined: %s\nThe last one ('%s') is returned!",
    kwb.utils::stringList(qualities), quality
  ))

  quality
}