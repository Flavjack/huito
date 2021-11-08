#' Barcode layer
#'
#' Insert barcode in label
#'
#' @param label label output
#' @param type type of entry: dynamic or static
#' @param units units for the label options
#' @param size image size
#' @param position position coordinate
#' @param color image color
#' @param value column or path
#'
#' @return data frame
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(gsheet)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'        , "1q0EZmZBt52ca-0VbididjJy2jXTwf06laJpzvkQJWvc/edit#gid=107939497")
#' fb <- gsheet2tbl(url)
#'
#' label <- label(data = fb
#'                , size = c(10, 2.5)
#'                ) %>%
#'          include_image(
#'                value = "https://inkaverse.com/reference/figures/logo.png"
#'                , size = c(2,2)
#'                ) %>%
#'          include_barcode(
#'                value = "qr-code"
#'                , size = c(2,2)
#'                )
#'
#' }
#'

include_barcode <- function(label
                  , value
                  , size
                  , position = NA
                  , type = "static"
                  , color = "auto"
                  , units = "cm"
                  ) {

  # test --------------------------------------------------------------------

  if (FALSE) {

    value = "https://inkaverse.com/reference/figures/logo.png"
    size = c(2, 2)
    position = NA
    type = "static"
    color = "auto"
    units = "cm"

  }

  # param -------------------------------------------------------------------

  value <- if(any(is.null(value)) || any(is.na(value)) || any(value == "")) {
    "https://inkaverse.com"
  } else {value}

  type <- if(value %in% names(label$data)) {
    "dynamic"
  } else {"static"}

  size <- if(any(is.null(size)) || any(is.na(size)) || any(size == "")) {
    rep(2.5, times = 2)
  } else if(is.character(size)) {
    size %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else {size}

  position <- if(any(is.null(position)) || any(is.na(position)) || any(position == "")) {
    c(0, 0)
  } else if(is.character(position)) {
    position %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else {position}

  # -------------------------------------------------------------------------

  color <- if(any(is.null(color)) || any(is.na(color)) || any(color == "")) {
    "black"
  } else {color}

# options -----------------------------------------------------------------

  opt <- list(value = value
              , size = size %>% paste0(collapse = "*")
              , position = position %>% paste0(collapse = "*")
              , color = color
              , units = units
              ) %>%
    tibble::enframe(name = "option") %>%
    dplyr::mutate(element = "barcode") %>%
    dplyr::mutate(type = type) %>%
    dplyr::select(.data$element, type, dplyr::everything()) %>%
    dplyr::bind_rows(label$opts, .) %>%
    dplyr::mutate(nlayer = dplyr::case_when(
      is.na(nlayer) ~ length(unique(nlayer)) - 1
      , TRUE ~ nlayer
    ))

  # result ------------------------------------------------------------------

  result <- list(opts = opt
                 , data = label$data)

}
