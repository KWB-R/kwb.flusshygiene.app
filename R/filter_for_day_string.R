# filter_for_day_string --------------------------------------------------------
#' Filter For Day String
#'
#' @param df data frame
#' @param day_string day string in ISO format "year-month-day"
#'
#' @return data frame with days smaller or equal day_string
#' @export
#'
#' @importFrom kwb.utils selectColumns
filter_for_day_string <- function(df, day_string)
{
  existing_days <- format(kwb.utils::selectColumns(df, "Date"), "%Y-%m-%d")

  index <- max(which(existing_days <= day_string))

  if (existing_days[index] != day_string) {

    message(sprintf(
      "No date available for %s. Using available data from %s",
      day_string, existing_days[index]
    ))
  }

  df[index, , drop = FALSE]
}
