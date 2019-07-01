# get_prediction ---------------------------------------------------------------
get_prediction <- function(
  model, d_predict, quantile_probs = c(0.025, 0.975, 0.5, 0.9, 0.95)
)
{
  newdata <- ModInputLite(d_predict, model)

  predicted_values <- rstanarm::posterior_predict(model, newdata = newdata)

  # The following variable "Values" is never used!
  # Values <- rstanarm::posterior_linpred(model, newdata = newdata)

  prediction <- apply(predicted_values, 2, stats::quantile, quantile_probs)

  colnames(prediction) <- as.character(newdata$Date)

  prediction <- t(10^prediction)

  prediction
}
