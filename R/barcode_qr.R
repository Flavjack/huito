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

barcode_qr <- function(text
                       , color = "black"
                       , alpha = 1
                       , ecl = "H"
                       ) {
  
  qrmt <- qrcode::qr_code(x = text, ecl = ecl) %>% 
    as.data.frame() %>% 
    dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  
  dt <- qrmt %>%
    dplyr::mutate(id = rownames(.)) %>%
    tidyr::pivot_longer(!.data$id, names_to = "key", values_to = "value") %>%
    dplyr::mutate(dplyr::across(c(.data$key, .data$id)
                              , ~ forcats::as_factor(.)))
  
  ggplot(dt, aes(x = .data$id, y = .data$key)) +
    geom_tile(aes(fill = .data$value), alpha = alpha) +
    scale_fill_gradient(low = "white", high = color) +
    theme_void() +
    theme(legend.position = 'none')
  
}
