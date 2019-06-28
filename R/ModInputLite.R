# ModInputLite -----------------------------------------------------------------

#' Reduce Model Input to Relevant Data
#'
#' @param ModInput data frame
#' @param model model object
#' @export
#'
ModInputLite <- function(ModInput, model)
{
  coefficient_names <- names(model$coefficients)
  variable_names <- unlist(strsplit(coefficient_names, split = ":"))

  # Names of all input variables
  input_names <- names(ModInput)

  # Filter the regressors of the selected Model
  columns <- c(input_names[1], intersect(input_names, variable_names))

  # Select only relevant columns and remove rows containing NA
  stats::na.omit(ModInput[, columns])
}
