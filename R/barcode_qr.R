#' Barcode generator
#'
#' Generate bar codes using QR codes
#'
#' @param text text to convert to QR bar code
#' @param color Bar code color
#' @param alpha Intensity of the bar code color
#'
#' @return plot
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' barcode_qr("inkaverse")
#'
#' }
#'

barcode_qr <- function(text
                          , color = "black"
                          , alpha = 1
                       ) {

  qrmt <- qrcode::qrcode_gen(text
                             , plotQRcode = F
                             , dataOutput = T) %>%
    as.data.frame()

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
