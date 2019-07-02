# get_model_input --------------------------------------------------------------

#' Create the Model Input List
#'
#' @param x data frame
#' @param logRain logical
#' @param fittingData logical
#' @export
#'
get_model_input <- function(x, logRain = TRUE, fittingData = TRUE)
{
  get <- function(column) kwb.utils::selectColumns(x, column)

  MI <- data.frame(
    Date = x[[1]],
    Qm0 = get("flowTW"),
    Qm1 = get("flowTW_1d"),
    Qm2 = get("flowTW_2d"),
    Qm3 = get("flowTW_3d"),
    Qm4 = get("flowTW_4d"),
    Qm5 = get("flowTW_5d"),
    Km0 = get("flowRuh"),
    Km1 = get("flowRuh_1d"),
    Km2 = get("flowRuh_2d"),
    Km3 = get("flowRuh_3d"),
    Km4 = get("flowRuh_4d"),
    Km5 = get("flowRuh_5d"),
    Rm0a = get("Precip1"),
    Rm1a = get("Precip1_1d"),
    Rm2a = get("Precip1_2d"),
    Rm3a = get("Precip1_3d"),
    Rm4a = get("Precip1_4d"),
    Rm5a = get("Precip1_5d"),
    Rm0b = get("Precip2"),
    Rm1a = get("Precip2_1d"),
    Rm2b = get("Precip2_2d"),
    Rm3b = get("Precip2_3d"),
    Rm4b = get("Precip2_4d"),
    Rm5b = get("Precip2_5d"),
    Km2sum = get("flowRuh_2dsum"),
    Km3m1sum = get("flowRuh_3dsum") - get("flowRuh_1d"),
    Km4m1sum = get("flowRuh_4dsum") - get("flowRuh_1d"),
    Km5m1sum = get("flowRuh_5dsum") - get("flowRuh_1d"),
    Qm2mean = get("flowTW_2dmean"),
    Qm3mean = get("flowTW_3dmean"),
    Qm4mean = get("flowTW_4dmean"),
    Qm5mean = get("flowTW_5dmean"),
    Rm2asum = get("Precip1_2dsum"),
    Rm3asum = get("Precip1_3dsum"),
    Rm4asum = get("Precip1_4dsum"),
    Rm5asum = get("Precip1_5dsum"),
    Rm5m2asum = get("Precip1_5dsum") - get("Precip1_2dsum"),
    Rm2bsum = get("Precip2_2dsum"),
    Rm3bsum = get("Precip2_3dsum"),
    Rm4bsum = get("Precip2_4dsum"),
    Rm5bsum = get("Precip2_5dsum")
  )

  if (fittingData) {
    MI$e.coli = get("Wert")
  }

  is_rain_column <- grepl("^Rm", names(MI))

  if (logRain) {
    MI[, is_rain_column] <- log(MI[, is_rain_column] + 1)
  }

  MI
}
