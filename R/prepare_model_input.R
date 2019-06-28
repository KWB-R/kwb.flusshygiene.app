# prepare_model_input ----------------------------------------------------------

#' Prepare Model Input Data Frame
#'
#' @param rain_ruhleben Rain and Ruhleben data as prepared with
#'   \code{\link{prepare_rain_ruhleben}}
#' @param tiefwerder Tiefwerder data as prepared with
#'   \code{\link{prepare_tiefwerder}}
#' @export
#'
prepare_model_input <- function(rain_ruhleben, tiefwerder)
{
  # rain_ruhleben:
  # Day    Precip1 Precip2 flowRuh
  # <date> <dbl>   <dbl>   <dbl>

  # tiefwerder:
  # Day    flowTW
  # <date> <dbl>

  base_input <- rain_ruhleben %>%
    dplyr::full_join(tiefwerder, by = "Day") %>%
    dplyr::arrange(.data$Day)

  # base_input:
  # Day    Precip1 Precip2 flowRuh flowTW
  # <date> <dbl>   <dbl>   <dbl>   <dbl>

  base_input %>%
    add_day_shift_columns(n_days_before = 1:5) %>%
    dplyr::mutate(
      Precip1_2dsum = .data$Precip1_1d + .data$Precip1_2d,
      Precip1_3dsum = .data$Precip1_2dsum + .data$Precip1_3d,
      Precip1_4dsum = .data$Precip1_3dsum + .data$Precip1_4d,
      Precip1_5dsum = .data$Precip1_4dsum + .data$Precip1_5d,
      Precip2_2dsum = .data$Precip2_1d + .data$Precip2_2d,
      Precip2_3dsum = .data$Precip2_2dsum + .data$Precip2_3d,
      Precip2_4dsum = .data$Precip2_3dsum + .data$Precip2_4d,
      Precip2_5dsum = .data$Precip2_4dsum + .data$Precip2_5d,
      flowRuh_2dmean = (.data$flowRuh_1d + .data$flowRuh_2d) / 2,
      flowRuh_3dmean = (.data$flowRuh_2dmean * 2 + .data$flowRuh_3d) / 3,
      flowRuh_4dmean = (.data$flowRuh_3dmean * 3 + .data$flowRuh_4d) / 4,
      flowRuh_5dmean = (.data$flowRuh_4dmean * 4 + .data$flowRuh_5d) / 5,
      flowRuh_2dsum = .data$flowRuh_1d + .data$flowRuh_2d,
      flowRuh_3dsum = .data$flowRuh_2dsum + .data$flowRuh_3d,
      flowRuh_4dsum = .data$flowRuh_3dsum + .data$flowRuh_4d,
      flowRuh_5dsum = .data$flowRuh_4dsum + .data$flowRuh_5d,
      flowTW_2dmean = (.data$flowTW_1d + .data$flowTW_2d) / 2,
      flowTW_3dmean = (.data$flowTW_2dmean * 2 + .data$flowTW_3d) / 3,
      flowTW_4dmean = (.data$flowTW_3dmean * 3 + .data$flowTW_4d) / 4,
      flowTW_5dmean = (.data$flowTW_4dmean * 4 + .data$flowTW_5d) / 5
    ) %>%
    dplyr::arrange(.data$Day)
}

# add_day_shift_columns --------------------------------------------------------
add_day_shift_columns <- function(df, n_days_before = 1:5)
{
  result <- df

  for (n_days in n_days_before) {

    shift <- df
    shift$Day <- shift$Day + n_days

    # Append "_1d", "_2d", etc. to all columns except the first (day) column
    columns <- names(shift)
    columns[-1] <- sprintf("%s_%dd", columns[-1], n_days, "d")
    names(shift) <- columns

    result <- dplyr::full_join(result, shift, by = "Day")
  }

  result
}
