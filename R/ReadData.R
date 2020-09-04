#' Read Data from the Clipboard
#'
#' \code{ReadData} reads tab seperated data from the clipboard and returns a
#' \code{data.frame} (default) or \code{data.table}.
#'
#' This is a helper function intended to streamline the process of reading
#' sample data posted by users on Stack Overflow. Slight errors are in optical
#' character recognition are common. Thus, minor editing in a spreadsheet
#' application may be necessary. This function reads tab seperated data in the
#' cliboard into a \code{data.frame} (default) or \code{data.table}.
#'.
#' @param first.colnames A logical scalar. Should the first row become column
#'   names?
#' @param write.clip A logical scalar. Should the resulting data be written to
#'   the clipboard for further editing?
#' @param as.DT A logical scalar. Should the data be returned as a data.table?
#'   If FALSE, a data.frame will be returned.
#' @param ... Additional paramaters to be passed to \code{read.table}.
#'
#' @return If \code{as.DT = TRUE}, a data.table will be returned. Otherwise a
#'   data.frame will be returned.
ReadData <- function(first.colnames = TRUE, as.DT = FALSE, ...){
  data <- read.table(text = clipr::read_clip(),
                     header = first.colnames, ...)
  if(as.DT == TRUE) {
    data.table::setDT(data)
  }
  return(data)
}
