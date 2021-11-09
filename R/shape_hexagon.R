#' Shape hexagon
#'
#' Hexagon geom shape for ggplot2
#'
#' @param border_width line width
#' @param background background color 
#' @param border_color border color
#' @param margin margin 
#' @param units units for shape
#' @param transparent transparent borders
#' 
#' @return geom
#'
#' @export
#' 
#' @examples
#' 
#' shape_hexagon(border_width = 5, background = "red",  border_color = "blue")
#'

shape_hexagon <- function (border_width = 2
                          , background = NA
                          , border_color = "black"
                          , margin = 0
                          , units = "cm"
                          , transparent = TRUE
                          )  {
  

# -------------------------------------------------------------------------

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
  
  background <- if(any(is.null(background)) || any(is.na(background)) || any(background == "")) {
    "transparent"
  } else {background}
  
  border_color <- if(any(is.null(border_color)) || any(is.na(border_color)) || any(border_color == "")) {
    "transparent"
  } else {border_color}
  
# -------------------------------------------------------------------------

  hexd <- data.frame(x = 1 + c(rep(-sqrt(3) / 2, 2), 0
                               , rep(sqrt(3) / 2, 2)
                               , 0), y = 1 + c(0.5, -0.5, -1, -0.5, 0.5, 1))
  
  hexd <- rbind(hexd, hexd[1, ])
  
  ggplot2::ggplot() + 
  ggplot2::geom_polygon(aes_(x = ~x, y = ~y)
                        , data = hexd
                        , size = border_width
                        , fill = background
                        , color = border_color 
                        ) +
    ggplot2::theme(panel.background = element_rect(fill = background)
          , plot.background = element_rect(fill = background, color = NA)
          , line = element_blank()
          , rect = element_blank()
          , axis.text = element_blank()
          , axis.title = element_blank()
          , axis.ticks.length = unit(0, "pt")
          , axis.ticks.length.x = NULL
          , axis.ticks.length.x.top = NULL
          , axis.ticks.length.x.bottom = NULL
          , axis.ticks.length.y = NULL
          , axis.ticks.length.y.left = NULL
          , axis.ticks.length.y.right = NULL
          , legend.box = NULL
          , plot.margin = unit(margin, units)
          , complete = TRUE
          )

}
