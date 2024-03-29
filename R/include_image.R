#' Image layer
#'
#' Insert image in label
#'
#' @param label label output
#' @param type type of entry: dynamic or static
#' @param units units for the label options
#' @param size image size
#' @param position position coordinate
#' @param value column or path
#' @param opts R magick funtions
#'
#' @return data frame
#'
#' @export
#'

include_image <- function(label
                  , value
                  , size
                  , position = NA
                  , type = "static"
                  , units = "cm"
                  , opts = NA
                  ) {

  # test --------------------------------------------------------------------

  if (FALSE) {

    label = label
    value = "https://inkaverse.com/reference/figures/logo.png"
    size = c(2, 2)
    position = NA
    type = "static"
    units = "cm"
    
    opts = list(
      "image_rotate(0)"
      , "image_flip()"
      , "image_charcoal()"
    )
    

  }

  # param -------------------------------------------------------------------

  value <- if(any(is.null(value)) || any(is.na(value)) || any(value == "")) {
    "https://inkaverse.com/reference/figures/logo.png"
  } else {value}

  type <- if(value %in% names(label$data)) {
    "dynamic"
  } else {"static"}

  size <- if(any(is.null(size)) || any(is.na(size)) || any(size == "")) {
    rep(2, times = 2)
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

  opts <- if(any(is.null(opts)) || any(is.na(opts)) || any(opts == "") || any(opts == "NA")) {
    NA
  } else if(is.character(opts)) {
    opts %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() 
  } 

# options -----------------------------------------------------------------

  opt <- list(value = value
              , size = size %>% paste0(collapse = "*")
              , position = position %>% paste0(collapse = "*")
              , opts = opts %>% unlist() %>% paste0(collapse = "*")
              , units = units
              ) %>%
    tibble::enframe(name = "option") %>%
    dplyr::mutate(element = "image") %>%
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
