ImageToData <- function(url,
                        split = " ",
                        filter = c("","="),
                        first.colnames = TRUE,
                        write.clip = TRUE,
                        lang = "eng") {
  image <- magick::image_read(url)
  image.resize <- magick::image_scale(image, "400x")
  image.gray <- magick::image_convert(image.resize, colorspace = "gray")
  image.trim <- magick::image_trim(image.gray)
  image.ocr <- magick::image_ocr(image.trim, language = lang)
  image.split <- unlist(strsplit(x = image.ocr, split = "(\n)?\n"))
  image.split <- strsplit(x = image.split, split = " ")
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
      write_clip(image.matrix)
  }
  return(image.matrix)
}
