# prepare_tiefwerder -----------------------------------------------------------

#' Prepare Tiefwerder Data
#'
#' @param tiefwerder data frame with data from Tiefwerder
#' @export
#'
prepare_tiefwerder <- function(tiefwerder)
{
  tiefwerder %>%
    handle_negative_flows() %>%
    summarise_tiefwerder() %>%
    add_missing_tiefwerder_flows()
}

# handle_negative_flows --------------------------------------------------------
handle_negative_flows <- function(tiefwerder)
{
  # Set values below 0 to 0
  tiefwerder$Flow[tiefwerder$Flow < 0 & ! is.na(tiefwerder$Flow)] <- 0

  tiefwerder
}

# summarise_tiefwerder ---------------------------------------------------------
summarise_tiefwerder <- function(tiefwerder)
{
  tiefwerder %>%
    add_day_column_from("DateTime") %>%
    dplyr::group_by(.data$Day) %>%
    dplyr::summarise(
      flowTW = round_2(mean(.data$Flow, na.rm = TRUE))
    )
}

# add_missing_tiefwerder_flows -------------------------------------------------
add_missing_tiefwerder_flows <- function(tiefwerder)
{
  flow_data <- data.frame(Day = lubridate::as_date("2017-07-14"), flowTW = 45)

  rbind(tiefwerder, flow_data)
}
