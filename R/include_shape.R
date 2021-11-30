#' Shape layer
#'
#' Insert shape in label
#'
#' @param label label output
#' @param value type of shape
#' @param units units for shape
#' @param size image size
#' @param position position coordinate
#' @param border_color image color
#' @param border_width shape line width
#' @param background background color
#' @param margin = shape margin
#' @param value column or path
#' @param panel_color panel color
#' @param panel_size panel size
#'
#' @return data frame
#'
#' @export
#'
#' @examples
#'
#'
#' label <- label_layout(data = NA
#'                , size = c(10, 2.5)
#'                ) %>%
#'          include_shape(
#'                value = "hexagon"
#'                , position = c(1.2, 1.25)
#'                , size = 3
#'                )
#'                
#' label %>% label_print("sample")
#' 
#' ts <- label$opts
#'                
#'

include_shape <- function(label
                  , value = "hexagon"
                  , size = 4.325
                  , position = NA
                  , border_color = "black"
                  , border_width = 2
                  , background = NA
                  , margin = 0
                  , units = "cm"
                  , panel_color = "green"
                  , panel_size = 5.08
                  ) {
  
# param -------------------------------------------------------------------

  size <- if(any(is.null(size)) || any(is.na(size)) || any(size == "")) {
    4.325
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

  border_width <- if(any(is.null(border_width)) || any(is.na(border_width)) || any(border_width == "")) {
    0
  } else if(is.character(border_width)) {
    border_width %>% as.numeric()
  } else {border_width}
  
  background <- if(any(is.null(background)) || any(is.na(background)) || any(background == "")) {
    "transparent"
  } else {background}
  
  border_color <- if(any(is.null(border_color)) || any(is.na(border_color)) || any(border_color == "")) {
    "transparent"
  } else {border_color}
  
  margin <- if(any(is.null(margin)) || any(is.na(margin)) || 
               any(margin == "") ) {
    rep(0, times = 4)
  } else if(is.character(margin)) {
    margin %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else if (length(margin) == 1 && is.numeric(margin)) {
    rep(margin, times = 4)
  } else {margin}
  
  
  panel_color <- if(any(is.null(panel_color)) || any(is.na(panel_color)) || any(panel_color == "")) {
    "transparent"
  } else {panel_color}
  
  panel_size <- if(any(is.null(panel_size)) || any(is.na(panel_size)) || any(panel_size == "")) {
    5.08
  } else if(is.character(panel_size)) {
    panel_size %>% as.numeric()
  } else {panel_size}

# options -----------------------------------------------------------------

  opt <- list(value = value
              , size = size %>% paste0(collapse = "*")
              , position = position %>% paste0(collapse = "*")
              , color = background
              , border_color = border_color
              , border_width = border_width
              , margin = margin %>% paste0(collapse = "*")
              , units = units
              , panel_color = panel_color
              , panel_size = panel_size
              ) %>%
    tibble::enframe(name = "option") %>%
    dplyr::mutate("element" := "shape") %>%
    dplyr::mutate("type" := "static") %>%
    dplyr::select(.data$element, .data$type, dplyr::everything()) %>%
    dplyr::bind_rows(label$opts, .) %>%
    dplyr::mutate(nlayer = dplyr::case_when(
      is.na(nlayer) ~ length(unique(nlayer)) - 1
      , TRUE ~ nlayer
    ))

  # result ------------------------------------------------------------------

  result <- list(opts = opt
                 , data = label$data)

}
