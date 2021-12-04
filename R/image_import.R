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

    image = "https://huito.inkaverse.com/reference/figures/logo.png"
    
    opt = NA
    
    opts = list(
      "image_rotate(0)"
      , "image_flip()"
      , "image_charcoal()"
    )
    
    opts <- "image_rotate(0)*image_flip()*image_charcoal()"

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
  
  
  
  img <- magick::image_read(image)
  
# -------------------------------------------------------------------------

  if(any(is.na(opts))) { return(img) }
  
# -------------------------------------------------------------------------
  
  img_opts <- c("image_read(image)",opts) %>% 
    tibble::enframe() %>% 
    dplyr::select(.data$value) %>% 
    tibble::deframe() %>% 
    paste0(collapse = " %>% ")
  
  img_final <- eval(parse(text = img_opts))
  
  img_final

}
