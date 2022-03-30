#' Fonts import
#'
#' Import fonts from Google fonts
#'
#' @param fonts fonts names
#' 
#' @details 
#' 
#' For more fonts visit: https://fonts.google.com/ 
#'
#' @return fonts
#'
#' @export
#'

huito_fonts <- function(fonts = NA
                         ) {
  
  list_fonts <- if(any(is.null(fonts)) || any(is.na(fonts)) || any(fonts == "")) {
    stop("Add fonts from Google fonts")
  } else { fonts }
  
    fonts <- list_fonts %>%
      tibble::enframe(value = "font") %>% 
      dplyr::select(.data$font) %>% 
      unique() %>%
      tidyr::drop_na() %>%
      dplyr::mutate(fun = paste0("sysfonts::font_add_google(name = ", "'" , .data$font, "'",")")) %>%
      dplyr::select(.data$fun) %>%
      purrr::as_vector() %>%
      paste0(., collapse = "; ")
    
    eval(parse(text = fonts))
  
}
