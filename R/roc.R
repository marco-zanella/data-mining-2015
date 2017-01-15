#-----------------------------------------------------------------------
# Plots a ROC curve.
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
#-----------------------------------------------------------------------
if (!exists('roc.R')) { roc.R <- T
#-----------------------------------------------------------------------

#' Generates ROC curve.
#'
#' Generates a ROC curve, and either plots it or return points.
#'
#' @param predicted     Vector of predicted probabilities
#' @param population    Vector containing 1 if and only if observation
#'                      is of the correct class
#' @param draw          Tells whether the curve should be drawn (default = T)
#' @param mode          Smoothing, one of 'raw' | 'smooth' | 'binning'.
#'                      Default is 'raw' (no smoothing)
#' @param h             If mode == 'smooth', this parameter controls
#'                      smoothing; h is (0, 1.0], default = 0.05
#' @param mark          If draw == T, tells whether to draw the line y = x
#' @param ...           Additional parameters to be passed to plot (if
#'                      draw == T)
#' @return Nothing if draw == T, a data frame containing two column, one
#'         with the x, one with the y
#'
#' @examples
#' roc(predicted1, population, mode = 'smooth', h = 0.1, mark = T, type = 'l)
#' ps <- roc(predicted2, population, mode = 'smooth', h = 0.1)
#' points(ps, type = 'l', col = 2)
roc <-
function (predicted, population, draw = T, mode = 'raw', h = 0.05, mark = F, ...)
{
  # Sanity checks
  if (!is.numeric(population)) {
    population <- as.numeric(population)
  }
  
  # Computes raw x and y
  idx <- rev(order(predicted))
  n   <- length(population)
  x   <- cumsum(1 - population[idx]) / sum(1 - population)
  y   <- cumsum(population[idx]) / sum(population)
  
  
  # Updates x and y if smoothing
  if (mode == 'smooth') {
    library(sm)
    a   <- sm.regression(x, log((y + 1e-5) / (1 - y + 2e-5)), h = h, display="none")
    x   <- a$eval
    y   <- exp(a$estimate) / (1 + exp(a$estimate))
  }
  
  # Updates x and y if binning
  else if (mode == 'binning') {
    b <- binning(x, y, breaks = (0 : 10) / 10)
    x <- c(0, seq(0.0, 0.9, by = 0.1), 1)
    y <- c(0, b$means, 1)
  }
  
  # Draws lift and mark line, if asked to...
  if (draw) {
    plot(x, y, xlim = c(0, 1), ylim = c(0, 1), ...)
    if (mark) {
      abline(0, 1, lty = 2)
    }
  }
  # ... otherwise, returns data frame with points
  else {
    data.frame(x = x, y = y)
  }
}

} # endif