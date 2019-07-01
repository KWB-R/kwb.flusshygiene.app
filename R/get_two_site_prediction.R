# get_two_site_prediction ------------------------------------------------------
get_two_site_prediction <- function(d_predict_today)
{
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

  record_kbw <- data.frame(
    id = 36,
    badname = "kleine Badewiese",
    Datum = rownames(prediction_kbw),
    Vorhersage = get_quality(prediction_kbw)
  )

  record_gwt <- data.frame(
    id = 28,
    badname = "Grunewaldturm",
    Datum = rownames(prediction_gwt),
    Vorhersage = get_quality(prediction_gwt)
  )

  rbind(record_kbw, record_gwt)
}

# get_prediction ---------------------------------------------------------------
get_prediction <- function(
  model, d_predict, quantile_probs = c(0.025, 0.975, 0.5, 0.9, 0.95)
)
{
  #kwb.utils::assignPackageObjects("kwb.flusshygiene.app")
  newdata <- ModInputLite(ModInput = d_predict, model)

  if (nrow(newdata) == 0) {
    message("ModInputLite() did not return any data. Returning NULL!")
    return()
  }

  predicted_values <- rstanarm::posterior_predict(model, newdata = newdata)

  # The following variable "Values" is never used!
  # Values <- rstanarm::posterior_linpred(model, newdata = newdata)

  prediction <- apply(predicted_values, 2, stats::quantile, quantile_probs)

  colnames(prediction) <- as.character(newdata$Date)

  prediction <- t(10^prediction)

  prediction
}
