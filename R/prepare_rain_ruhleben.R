# prepare_rain_ruhleben --------------------------------------------------------

#' Prepare Rain and Ruhleben Data
#'
#' @param combined data frame with rain data and data from Ruhleben
#' @export
#'
prepare_rain_ruhleben <- function(combined)
{
  combined %>%
    clear_high_values_in_rain_columns(threshold = 20)
    add_average_rain_columns() %>%
    summarise_rain_ruhleben()
}

# clear_high_values_in_rain_columns --------------------------------------------
clear_high_values_in_rain_columns <- function(combined, threshold)
{
  # Determine rain data columns
  columns <- 3:(ncol(combined) - 1)

  # Set unplausible rain values to NA
  combined[, columns] <- apply(combined[, columns], 2, function(x) {
    ifelse(x > threshold, NA, x)
  })

  combined
}

# add_average_rain_columns -----------------------------------------------------

#' Add Average Rain Columns to Rain Data
#'
#' @param rain data frame containing rain data columns
#' @export
#'
add_average_rain_columns <- function(rain)
{
  # Define helper functions
  get_existing <- function(df, columns) df[, intersect(names(df), columns)]
  average_by_row <- function(df) apply(df, 1, mean, na.rm = TRUE)

  # Select rain gages (2 groups)
  gauge_info <- kwb.flusshygiene.app::get_available_rain_gauges()

  gauges <- stats::setNames(gauge_info$Bez, gauge_info$Gage)

  skip_gauges <- c("BWB24", "BWB28", "BWB29", "BWB32", "BWB34")

  important_gauges <- unname(gauges[setdiff(names(gauges), skip_gauges)])

  kwb.utils::setColumns(
    rain,
    Average1 = average_by_row(get_existing(rain, important_gauges)),
    Average2 = average_by_row(get_existing(rain, unname(gauges)))
  )
}

# summarise_rain_ruhleben ------------------------------------------------------
summarise_rain_ruhleben <- function(combined)
{
  # Ruhleben flow in "L/s" --> conversion to "1000 m3/d"
  conversion_factor <- 3600 * 24 / 1000 / 1000

  combined %>%
    add_day_column_from("tBeg") %>%
    dplyr::group_by(.data$Day) %>%
    dplyr::summarise(
      Precip1 = round_2(sum(.data$Average1)),
      Precip2 = round_2(sum(.data$Average2)),
      flowRuh = round_2(conversion_factor * mean(.data$KW.Ruh, na.rm = TRUE))
    )
}