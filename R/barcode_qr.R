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
#' @importFrom qrcode qrcode_gen
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

  as_factor <- NULL

  qrmt <- qrcode::qrcode_gen(text
                             , plotQRcode = F
                             , dataOutput = T) %>%
    as.data.frame()

  dt <- qrmt %>%
    mutate(id = rownames(.)) %>%
    pivot_longer(!.data$id, names_to = "key", values_to = "value") %>%
    mutate(across(c(.data$key, .data$id), ~forcats::as_factor(.)))

  ggplot(dt, aes(x = .data$id, y = .data$key)) +
    geom_tile(aes(fill = .data$value), alpha = alpha) +
    scale_fill_gradient(low = "white", high = color) +
    theme_void() +
    theme(legend.position = 'none')
}
