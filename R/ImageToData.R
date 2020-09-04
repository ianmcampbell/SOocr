#' Convert Image to Data
#'
#' \code{ImageToData} returns a \code{data.frame} (by default) or
#' \code{data.table} from an image url using optical character recognition.
#'
#' This is a helper function intended to streamline the process of reading
#' sample data posted by users on Stack Overflow. Slight errors are common, and
#' thus by default the data is written to the clipboard for further processing
#' in the user's preferred spreadsheet software.
#'
#' @param url A character string containing the url of the image to be converted
#' @param split A character string containing a perl compatible regular
#'   expression to split cells
#' @param filter A character vector of strings which should be filtered out of
#'   the final result
#' @param lang A chracter vector of the language to be used by tesseract.
#' @param first.colnames A logical scalar. Should the first row become column
#'   names?
#' @param write.clip A logical scalar. Should the resulting data be written to
#'   the clipboard for further editing?
#' @param as.DT A logical scalar. Should the data be returned as a data.table?
#'   If FALSE, a data.frame will be returned.
#' @param resize A character vector indicating the resizing parameter. If NULL,
#'   no resizing will be performed.
#'
#' @return If \code{as.DT = TRUE}, a data.table will be returned. Otherwise a
#'   data.frame will be returned.
#'
#' @examples
#' ImageToData("https://i.stack.imgur.com/yBcfz.jpg")
#'
#'
ImageToData <- function(url, split = " ", filter = c("","="), lang = "eng",
                        first.colnames = TRUE, write.clip = TRUE, as.DT = FALSE,
                        resize = NULL) {
  image <- magick::image_read(url)
  if(is.null(resize)){
    image.resize <- image
  }
  else {
    image.resize <- magick::image_scale(image, "400x")
  }
  image.gray <- magick::image_convert(image.resize, colorspace = "gray")
  image.trim <- magick::image_trim(image.gray)
  image.ocr <- magick::image_ocr(image.trim, language = lang)
  image.split <- unlist(strsplit(x = image.ocr, split = "(\n)?\n", perl = TRUE))
  image.split <- strsplit(x = image.split, split = " ", perl = TRUE)
  image.list <- lapply(image.split, function(x) {
      x <- x[!x %in% c("", "=")]
      as.list(setNames(x, seq_along(x)))
    })
  image.matrix <- data.table::rbindlist(image.list, fill = TRUE)

  if(first.colnames == TRUE) {
      colnames(image.matrix) <- as.character(image.matrix[1,])
      image.matrix <- image.matrix[-1,]
  }
  if(write.clip == TRUE) {
      clipr::write_clip(image.matrix)
  }
  if(as.DT == FALSE) {
    data.table::setDF(image.matrix)
  }
  return(image.matrix)
}


