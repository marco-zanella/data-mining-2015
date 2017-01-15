#-----------------------------------------------------------------------
# Computes squared error.
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
#-----------------------------------------------------------------------
if (!exists('squared_error.R')) { squared_error.R <- T
#-----------------------------------------------------------------------


#' Computes sum of squared errors.
#'
#' Computes sum of squared error between observed values and predicted
#' values.
#'
#' @param y.observed    Vector of observed values
#' @param y.predicted   Vector of predicted values
#' @return Sum of squared errors
#'
#' @examples
#' y1 <- c(1.0, 2.0, 3.0)
#' y2 <- c(1.1, 1.9, 3.05)
#' e  <- SquaredError(y1, y2)
SquaredError <-
function(y.observed, y.predicted)
{
  sum((y.observed - y.predicted) ^ 2)
}


#' Computes mean of squared errors.
#'
#' Computes mean of squared error between observed values and predicted
#' values.
#'
#' @param y.observed    Vector of observed values
#' @param y.predicted   Vector of predicted values
#' @return Mean of squared errors
#'
#' @examples
#' y1 <- c(1.0, 2.0, 3.0)
#' y2 <- c(1.1, 1.9, 3.05)
#' e  <- MeanSquaredError(y1, y2)
MeanSquaredError <-
function(y.observed, y.predicted)
{
  mean((y.observed - y.predicted) ^ 2)
}

} # endif