# prepare_tiefwerder -----------------------------------------------------------

#' Prepare Tiefwerder Data
#'
#' @param flows data frame with flow data from Tiefwerder and Sophienwerder
#' @param dbg debug level. The higher the value, the more verbose the output
#' @export
#' @importFrom kwb.utils renameAndSelect
prepare_tiefwerder <- function(flows, dbg = 1)
{
  x <- flows %>%
    kwb.utils::renameAndSelect(list("DateTime", Q.tiefwerder = "Flow")) %>%
    handle_negative_flows(dbg = dbg) %>%
    summarise_tiefwerder() %>%
    add_missing_tiefwerder_flows()
}

# handle_negative_flows --------------------------------------------------------
handle_negative_flows <- function(tiefwerder, dbg = 1)
{
  # Where are the flow values negative?
  which_negative <- which(tiefwerder$Flow < 0)

  n_negative <- length(which_negative)

  if (n_negative) {

    cat(sprintf("Setting %d negative flow values to 0.\n", n_negative))

    kwb.utils::printIf(dbg > 1, tiefwerder[which_negative, ])

    # Set values below 0 to 0
    tiefwerder$Flow[which_negative] <- 0
  }

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
  tiefwerder %>%
    rbind(data.frame(Day = as.Date("2017-07-14"), flowTW = 45)) %>%
    dplyr::arrange(.data$Day)
}
