#' Shape layer
#'
#' Insert shape in label
#'
#' @param label label output (table)
#' @param value type of shape (string: "hexagon")
#' @param size shape size (numeric: 5.08)
#' @param position position coordinate (numeric: NA)
#' @param border_color image color (string: "black")
#' @param border_width shape line width (numeric: 1)
#' @param background background color (string: "red")
#' @param units units for shape (string: "cm")
#' @param panel_color panel color (string: NA)
#' @param panel_size panel size (numeric: NA)
#'
#' @return data frame
#'
#' @export
#'
#' @examples
#' 
#' library(huito)
#'
#' label <- label_layout(data = NA
#'                , size = c(10, 2.5)
#'                , background = "yellow"
#'                ) %>%
#'          include_shape(
#'                value = "hexagon"
#'                , position = c(1.2, 1.25)
#'                , background = "red"
#'                , border_width = 1
#'                , size = 2.4
#'                #, panel_size = 2.4*1.157175
#'                )
#'                
#' label %>% label_print("sample")
#' 
#' ts <- label$opts
#'                

include_shape <- function(label
                  , value = "hexagon"
                  , size = 5.08
                  , position = NA
                  , border_color = "black"
                  , border_width = 1
                  , background = NA
                  , units = "cm"
                  , panel_color = NA
                  , panel_size = NA
                  ) {
  
  if (FALSE) {
    
    value = "hexagon"
    size = 2.5
    position = NA
    border_color = "black"
    border_width = 1
    background = NA
    units = "cm"
    panel_color = NA
    panel_size = 5.08
    
  }
  
# param -------------------------------------------------------------------
  
  panel_size <- if(any(is.null(panel_size)) || any(is.na(panel_size)) || any(panel_size == "")) {
    size*1.157175
  } else if(is.character(panel_size)) {
    panel_size %>% as.numeric()
  } else { panel_size }
  
  size <- if(any(is.null(size)) || any(is.na(size)) || any(size == "")) {
    stop("include hexagon size")
  } else if(is.character(size)) {
    size %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else { size }
  
  if(size > panel_size) "Panel size should be bigger than the hexagon size"
  
# -------------------------------------------------------------------------

  position <- if(any(is.null(position)) || any(is.na(position)) || any(position == "")) {
    c(0, 0)
  } else if(is.character(position)) {
    position %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else {position}
  
# -------------------------------------------------------------------------
  
  background <- if(any(is.null(background)) || any(is.na(background)) || any(background == "")) {
    "transparent"
  } else {background}
  
  panel_color <- if(any(is.null(panel_color)) || any(is.na(panel_color)) || any(panel_color == "")) {
    "transparent"
  } else {panel_color}
  
  border_width <- if(any(is.null(border_width)) || any(is.na(border_width)) || any(border_width == "")) {
    0
  } else if(is.character(border_width)) {
    border_width %>% as.numeric()
  } else {border_width}
  
  border_color <- if(any(is.null(border_color)) || any(is.na(border_color)) || any(border_color == "")) {
    "transparent"
  } else {border_color}
  
# options -----------------------------------------------------------------

  opt <- list(value = value
              , size = size %>% paste0(collapse = "*")
              , position = position %>% paste0(collapse = "*")
              , color = background
              , border_color = border_color
              , border_width = border_width
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
