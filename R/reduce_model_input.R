# reduce_model_input -----------------------------------------------------------

#' Reduce Model Input to Relevant Data
#'
#' @param model_input data frame
#' @param model model object
#' @param context context string to appear in error messages. Default: name of
#'   the object passed in \code{model}
#' @export
#'
reduce_model_input <- function(
  model_input, model, context = deparse(substitute(model))
)
{
  coefficient_names <- names(model$coefficients)
  variable_names <- unlist(strsplit(coefficient_names, split = ":"))

  # Names of all input variables
  input_names <- names(model_input)

  # Filter the regressors of the selected Model
  columns <- c(input_names[1], intersect(input_names, variable_names))

  result <- model_input[, columns, drop = FALSE]

  # Select only relevant columns and remove rows containing NA
  is_complete <- stats::complete.cases(result)

  if (any(! is_complete)) {

    message(sprintf(
      "There are missing values in the input to '%s': ", context
    ))

    print(result[! is_complete, , drop = FALSE])
  }

  result[is_complete, , drop = FALSE]
}
