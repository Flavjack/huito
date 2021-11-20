#' Text layer
#'
#' Insert text in label
#'
#' @param label label output
#' @param type type of entry: dynamic or static
#' @param font font type
#' @param size text size
#' @param position position coordinate
#' @param color image color
#' @param value column or string
#' @param angle angle of the text
#' @param opts list arguments from draw_label()
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
#'                , size = c(10, 2.5)
#'                ) %>%
#'          include_image(value = "https://inkaverse.com/reference/figures/logo.png"
#'                , size = c(2,2)
#'                ) %>%
#'          include_text(value = "plots", position = "5*1") %>% 
#'          label_print()
#'
#' }
#'

include_text <- function(label
                         , value
                         , position = NA
                         , size = 11
                         , font = NA
                         , type = "static"
                         , color = NA
                         , angle = 0
                         , opts = NA
                         ) {

  # test --------------------------------------------------------------------

  if (FALSE) {

    label = label
    value = "Inkaverse"
    position = NA
    size = 11
    font = "Permanent Marker"
    type = "static"
    color = "black"
    angle = 0
    
    opts = list(hjust = 0.5
               , vjust = 0.5)
    
    
    ts <- do.call(paste, opts)

  }

  # param -------------------------------------------------------------------

  value <- if(any(is.null(value)) || any(is.na(value)) || any(value == "")) {
    "INKAVERSE"
  } else {value}

  type <- if(value %in% names(label$data)) {
    "dynamic"
  } else {"static"}

  font <- if(any(is.null(font)) || any(is.na(font)) || any(font == "")) {
    "Permanent Marker"
  } else {font}

  size <- if(any(is.null(size)) || any(is.na(size)) || any(size == "")) {
    11
  } else {size}

  angle <- if(any(is.null(angle)) || any(is.na(angle)) || any(angle == "")) {
    0
  } else {angle}

  color <- if(any(is.null(color)) || any(is.na(color)) || any(color == "")) {
    "black"
  } else {color}

  position <- if(any(is.null(position)) || any(is.na(position)) || any(position == "")) {
    c(0, 0)
  } else if(is.character(position)) {
    position %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else {position}

# options -----------------------------------------------------------------

  opt <- list(value = value
              , size = size
              , font = font
              , position = position %>% paste0(collapse = "*")
              , color = color
              , angle = angle
              , opts = opts 
              ) %>%
    tibble::enframe(name = "option") %>%
    dplyr::mutate(element = "text") %>%
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
