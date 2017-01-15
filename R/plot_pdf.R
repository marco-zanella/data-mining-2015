#-----------------------------------------------------------------------
# Functions to plot into a pdf file.
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
#-----------------------------------------------------------------------
if (!exists('plot_pdf.R')) { plot_pdf.R <- T
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Loads libraries and imports local scripts
source('R/plot_functions.R')


#' Plots into a pdf file.
#'
#' Opens a pdf file and saves the graphical output of plot.function into it.
#'
#' @param file          Path to the output file
#' @param plot.function Runction producing a plot
#' @param ...           Parameters to be passed to pdf driver
#' @return Nothing
#'
#' @examples
#' PlotPDF("path/to/myfile.pdf", function(){ plot(x, y) })
PlotPDF <- function(file, plot.function, ...) {
  pdf(file, ...)
  plot.function()
  invisible(dev.off())
}


#' Plots a group of plots to the same file.
#'
#' Opens a pdf file and saves the graphical outputs of every function in
#' list into it. Plots are arranged into a grid, width and height are
#' computed automatically.
#'
#' @param file          Path to the output file
#' @param list          List of functions producing plots
#' @param ...           Parameters to be passed to pdf driver
#' @return Nothing
#'
#' @examples
#' PlotNPDF("path/to/myfile.pdf", c(foo, bar))
PlotNPDF <- function(file, list, ...) {
  n    <- NROW(list)
  cols <- ceiling(sqrt(n))
  rows <- ceiling(n / cols)
  
  pdf(file, width = 7 * cols, height = 7 * rows, ...)
  PlotN(list)
  invisible(dev.off())
}

} # endif