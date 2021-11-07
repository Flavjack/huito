#' Label layout
#'
#' Generate labels options
#'
#' @param data data frame to build the labels
#' @param units units for the label options
#' @param size label size
#' @param background background color
#' @param border_color border color
#' @param border_width border width
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
#' label <- label_layout(data = fb
#'                      , size = c(10, 2.5)
#'                      )
#'
#' }
#'

label_layout <- function(data = NA
                        , size
                        , background = "white"
                        , border_color = "black"
                        , border_width = 0.5
                        , units = "cm"
                        ) {

# test --------------------------------------------------------------------

if (FALSE) {

  data <- NA
  size <- c(10, 2.5)
  background = NA
  border_color = NA
  border_width = NA
  units = "cm"

}

# param -------------------------------------------------------------------

size <- if(any(is.null(size)) || any(is.na(size)) || any(size == "")) {
  NA
} else if(is.character(size)) {
  size %>%
    gsub("[[:space:]]", "", .) %>%
    strsplit(., "[*]") %>%
    unlist() %>% as.numeric()
} else {size}

border_width <- if(any(is.null(border_width)) || any(is.na(border_width)) || any(border_width == "")) {
  0.5
} else if(is.character(border_width)) {
  border_width %>% as.numeric()
} else {border_width}

background <- if(any(is.null(background)) || any(is.na(background)) || any(background == "")) {
  "white"
} else {background}

border_color <- if(any(is.null(border_color)) || any(is.na(border_color)) || any(border_color == "")) {
  "black"
} else {border_color}

# options -----------------------------------------------------------------

opt <- list(size = size %>% paste0(collapse = "*")
            , color = background
            , border_color = border_color
            , border_width = border_width
            , units = units
            ) %>%
  tibble::enframe(name = "option") %>%
  dplyr::mutate(element = "label") %>%
  dplyr::mutate(type = "template") %>%
  dplyr::select(.data$element, .data$type, dplyr::everything()) %>%
  dplyr::mutate(nlayer = 0)

# result ------------------------------------------------------------------

result <- list(opts = opt
               , data = data)

}
