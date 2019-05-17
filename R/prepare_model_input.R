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
  base_input <- rain_ruhleben %>%
    dplyr::full_join(tiefwerder, by = "Day") %>%
    dplyr::arrange(.data$Day)

  result <- base_input

  for (i in 1:5) {

    shift <- base_input
    shift$Day <- shift$Day + i
    colnames(shift)[2:5] <- paste0(colnames(shift)[2:5], "_", i, "d")

    result <- dplyr::full_join(x = result, y = shift, "Day")
  }

  result %>%
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
