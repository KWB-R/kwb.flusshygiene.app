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

  result <- ModInput[, columns, drop = FALSE]

  # Select only relevant columns and remove rows containing NA
  is_complete <- stats::complete.cases(result)

  if (any(! is_complete)) {

    message(
      "There are NA values in the model input. The corresponding rows are ",
      "removed:"
    )

    print(result[! is_complete, , drop = FALSE])
  }

  result[is_complete, , drop = FALSE]
}
