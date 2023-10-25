#' Image import
#'
#' Import images and include R magick options
#'
#' @param image path or url
#' @param opts R magick functions by layers
#'
#' @return image
#'
#' @export
#'
#'

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
  
  imgtype <- if(grepl(pattern = ".*pdf$", x = image)) { 
    
    "magick::image_read_pdf(image)"
    
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
