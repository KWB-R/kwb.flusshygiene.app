# get_two_site_prediction ------------------------------------------------------
get_two_site_prediction <- function(d_predict_today, dbg = 1)
{
  message("\nPredicting water quality for Kleine Badewiese and Grunewaldturm\n")

  # Helper function
  get_dates_respecting_null <- function(prediction) {
    if (is.null(prediction)) {
      as.character(d_predict_today$Date)
    } else {
      rownames(prediction)
    }
  }

  # Kleine Badewiese
  prediction_kbw <- get_prediction(
    model = model_kleine_badewiese,
    d_predict = d_predict_today
  )

  # Grunewaldturm
  prediction_gwt <- get_prediction(
    model = model_grunewaldturm,
    d_predict = d_predict_today
  )

  kwb.utils::printIf(dbg > 1, prediction_kbw)
  kwb.utils::printIf(dbg > 1, prediction_gwt)

  record_kbw <- data.frame(
    id = 36,
    badname = "kleine Badewiese",
    Datum = get_dates_respecting_null(prediction_kbw),
    Vorhersage = get_quality(prediction_kbw)
  )

  record_gwt <- data.frame(
    id = 28,
    badname = "Grunewaldturm",
    Datum = get_dates_respecting_null(prediction_gwt),
    Vorhersage = get_quality(prediction_gwt)
  )

  rbind(record_kbw, record_gwt)
}

# get_prediction ---------------------------------------------------------------
get_prediction <- function(
  model, d_predict, quantile_probs = c(0.025, 0.975, 0.5, 0.9, 0.95), dbg = 1,
  context = deparse(substitute(model))
)
{
  #kwb.utils::assignPackageObjects("kwb.flusshygiene.app")
  newdata <- reduce_model_input(ModInput = d_predict, model, context = context)

  if (nrow(newdata) == 0) {

    kwb.utils::catIf(
      dbg > 1,
      "reduce_model_input() did not return any data. Returning NULL!\n"
    )

    return (NULL)
  }

  predicted_values <- rstanarm::posterior_predict(model, newdata = newdata)

  # The following variable "Values" is never used!
  # Values <- rstanarm::posterior_linpred(model, newdata = newdata)

  prediction <- apply(predicted_values, 2, stats::quantile, quantile_probs)

  colnames(prediction) <- as.character(newdata$Date)

  prediction <- t(10^prediction)

  prediction
}
