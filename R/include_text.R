#' Text layer
#'
#' Insert text in label
#'
#' @param label label output
#' @param type type of entry `[character: dynamic or static]`
#' @param prefix Prefix for the dynamic text
#' @param font font type
#' @param fontface font type `[character: plain, bold, italic]`
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

include_text <- function(label
                         , value
                         , prefix = ""
                         , position = NA
                         , fontface = "plain"
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
    prefix = "Test: "
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
  
  fontface <- if(any(is.null(value)) || any(is.na(value)) || any(value == "")) {
    ""
  } else {fontface}
  
  prefix <- if(any(is.null(value)) || any(is.na(value)) || any(value == "")) {
    ""
  } else {prefix}
  

  type <- if(value %in% names(label$data)) {
    "dynamic"
  } else {"static"}

  font <- if(any(is.null(font)) || any(is.na(font)) || any(font == "")) {
    "sans"
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
              , prefix = prefix
              , size = size
              , font = font
              , fontface = fontface
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
