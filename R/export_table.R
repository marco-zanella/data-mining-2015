#-----------------------------------------------------------------------
# Exports an R object into a LaTeX table.
# Marco Zanella <marco.zanella.9@studenti.unipd.it>
#-----------------------------------------------------------------------
if (!exists('export_table.R')) { export_table.R <- T
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Loads libraries and imports local scripts
library('xtable')


#-----------------------------------------------------------------------
#' Exports an object as a LaTeX table.
#'
#' Exports an object as a LaTeX table, using xtable package.
#'
#' @param object        R object
#' @param file          File where table will be written
#' @param caption       Caption of the table
#' @param label         Label of the table
#' @param ...           Parameters to be passed to print
#' @return Nothing
#'
#' @examples
#' ExportTable(my.obj, "path/to/myfile.tex")
ExportTable <- function(object, file, caption = "", label = "", ...) {
  align <- replicate(NCOL(object) + 1, "| c ")
  table <- xtable(object, caption = caption, label = label, align = align)
  print(table, file = file, table.placement = "tb", include.rownames = F, comment = F, ...)
}

} # endif