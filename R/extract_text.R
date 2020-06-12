#' Module to extract text data from various file types
#'
#' @description Takes in a file path and returns the text extracted from the document
#'
#' @importFrom tesseract ocr
#' @importFrom pdftools pdf_convert pdf_text
#' @importFrom tools file_ext
#' @importFrom qdapTools read_docx
#' @importFrom antiword antiword
#' @importFrom magick image_read image_resize image_convert image_trim image_ocr
#' @importFrom data.table fread
#' @importFrom readr read_lines
#' @importFrom quanteda corpus
#' @importFrom readxl read_excel
#' @importFrom magrittr `%>%`
#'
#' @param .filepath Character string containing the path to the file 
#'                  from which the text data is to be extracted
#' @param .pdfimage Boolean denoting if the file is an image, 
#'                  for use when \code{file_path == 'pdf'}
#' @param .filetype Character string to specify the file type
#'
#' @examples 
#' \dontrun{
#' 
#' oper655_readme <- "https://raw.githubusercontent.com/AFIT-R/oper655_fa2019/master/README.md"
#' 
#' Text = extract_text(.filepath = oper655_readme,
#'                     .filetype = "txt")
#' 
#' }
#'
#' @return text data
#' @export
extract_text <-  function(.filepath = NULL,
                          .pdfimage = F,
                          .filetype = NULL){
  
  if(is.null(.filetype)) .filetype <- tools::file_ext(.filepath)
  
  text_data <- switch(tolower(.filetype),
                      'pdf' = `if`(.pdfimage,
                                   tesseract::ocr(pdftools::pdf_convert(.filepath, dpi = 600)),
                                   pdftools::pdf_text(.filepath)),
                      'docx' = qdapTools::read_docx(.filepath),
                      'doc' = antiword::antiword(.filepath),
                      'txt' = readr::read_lines(.filepath),
                      'csv' = data.table::fread(.filepath),
                      'xls' =, 'xlsx' = readxl::read_excel(.filepath),
                      'tif' =, "png" = { magick::image_read(.filepath) %>% 
                                magick::image_resize("2000")   %>%
                                magick::image_convert(colorspace = 'gray') %>%
                                magick::image_trim() %>%
                                magick::image_ocr() })
  
  return(text_data)
  
}