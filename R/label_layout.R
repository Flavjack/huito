#' Label layout
#'
#' Generate labels options
#'
#' @param data data frame to build the labels
#' @param size label size (numeric: c(10, 2.5))
#' @param border_width border width (numeric: 0.5)
#' @param border_color border color (string: "transparent")
#' @param background background color (string: "transparent")
#' @param units units for the label options (string: "cm")
#'
#' @return data frame
#'
#' @export
#'
#' @examples
#'
#' label <- label_layout(size = c(10, 2.5)
#'                    , border_color = "red"
#'                    , border_width = 1
#'                    ) %>% 
#'                  label_print()
#'

label_layout <- function(data = NA
                        , size
                        , border_width = NA
                        , border_color = NA
                        , background = NA
                        , units = "cm"
                        ) {

# param -------------------------------------------------------------------
  
data <- if(is.data.frame(data)) {
  data %>% tibble::tibble() 
  } else { data <- NA }
  
size <- if(any(is.null(size)) || any(is.na(size)) || any(size == "")) {
  c(10, 2.5)
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
  "transparent"
} else {background}

border_color <- if(any(is.null(border_color)) || any(is.na(border_color)) || any(border_color == "")) {
  "transparent"
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
