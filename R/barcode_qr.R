#' Barcode generator
#'
#' Generate bar codes using QR codes
#'
#' @param text text to convert to QR bar code
#' @param color Bar code color
#' @param alpha Intensity of the bar code color
#' @param ecl Error correction level (percentage). "L" (7), "M" (15), "Q" (25)
#'   and "H" (30). Defaults to "H"
#'
#' @return plot
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(huito)
#' 
#' barcode_qr("LIMA-2021-11-03_15_3_4")
#'
#' }
#' 

barcode_qr <- function(text
                       , color = "black"
                       , alpha = 1
                       , ecl = "H"
                       ) {
  
  if(FALSE) {
    
    text = "LIMA-2021-11-03_15_3_4"
    
  }
  
  qrmt <- qrcode::qr_code(x = text, ecl = ecl) %>% 
    as.data.frame() %>% 
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric)) %>% 
    dplyr::select(!c(1:2)) %>% 
    dplyr::select(!c((ncol(.)-1):ncol(.))) %>% 
    utils::head(-2) %>% 
    utils::tail(-2)

  dt <- qrmt %>%
    dplyr::mutate(id = rownames(.)) %>%
    tidyr::pivot_longer(!.data$id, names_to = "key", values_to = "value") %>%
    dplyr::mutate("keyid" := gsub("[[:alpha:]]", "", .data$key)) %>% 
    dplyr::mutate(dplyr::across(c(.data$id, .data$keyid), as.numeric)) %>%
    dplyr::mutate(dplyr::across(c(.data$id, .data$keyid), as.factor))

  dt %>% 
    ggplot(aes(x = .data$id, y = .data$keyid)) +
    geom_tile(aes(fill = .data$value), alpha = alpha) +
    scale_fill_gradient(low = "white", high = color) +
    theme_void() +
    theme(legend.position = 'none')
  
}
