#' Image import
#'
#' Import images and apply R magick functions
#'
#' @param image image path or URL
#' @param opts R magick functions by layers
#'
#' @return image
#'
#' @export
image_import <- function(image
                         , opts = NA
) {
  
  # test --------------------------------------------------------------------
  
  if (FALSE) {
    
    image = "https://huito.inkaverse.com/img/scale.pdf"
    
    opts = NA
    
    opts = list(
      "image_rotate(90)"
      , "image_flip()"
      , "image_charcoal()"
    )
    
    opts <- "image_rotate(90)"
    
  }
  
  # -------------------------------------------------------------------------
  
  opts <- if(any(is.null(opts)) || any(is.na(opts)) || any(opts == "") || any(opts == "NA")) {
    NA
  } else if(is.character(opts)) {
    opts %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() 
  } 
  
  is_pdf <- grepl(pattern = "\\.pdf(\\?.*)?$", x = trimws(image), ignore.case = TRUE)
  
  imgtype <- if(is_pdf) { 
    
    "magick::image_read_pdf(image, density = 300)"
    
  } else {
    
    "image_read(image)"
    
  }
  
  img_opts <- c(imgtype, opts) %>% 
    stats::na.omit(.) %>% 
    tibble::enframe() %>% 
    dplyr::select(.data$value) %>% 
    tibble::deframe() %>% 
    paste0(collapse = " %>% ")
  
  img_final <- eval(parse(text = img_opts))
  
  return(img_final)
  
}