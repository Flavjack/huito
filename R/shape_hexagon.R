#' Shape hexagon
#'
#' Hexagon geom shape for ggplot2
#' 
#' @param size hexagon size
#' @param border_width line width
#' @param background background color 
#' @param border_color border color
#' @param margin margin 
#' @param units units for shape
#' @param panel_color panel color
#' @param panel_size panel size
#' 
#' @return geom
#'
#' @export
#' 
#' @examples
#' 
#' shape_hexagon(border_width = 3)
#'

shape_hexagon <- function (size = 4.39
                          , border_width = 2
                          , background = "transparent"
                          , border_color = "black"
                          , margin = 0
                          , units = "cm"
                          , panel_color = "green"
                          , panel_size = 5.08
                          )  {
  

# -------------------------------------------------------------------------

#> https://stackoverflow.com/questions/41261785/clip-cut-everything-outside-of-polygon-or-fill-the-outside-with-white
  
if(FALSE) {
  
  size = 4.39
  border_width = 2
  background = NA
  border_color = "black"
  margin = 0
  units = "cm"
  panel_color = "red"
  panel_size = 5.08

}
  
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
  
  panel_color <- if(any(is.null(panel_color)) || any(is.na(panel_color)) || any(panel_color == "")) {
    "transparent"
  } else {panel_color}
  
  panel_size <- if(any(is.null(panel_size)) || any(is.na(panel_size)) || any(panel_size == "")) {
    5.08
  } else if(is.character(panel_size)) {
    panel_size %>% as.numeric()
  } else {panel_size}
  
# -------------------------------------------------------------------------
  
  hexbin <- function(dx, dy = NULL, n = 1, sep = NULL) {
    stopifnot(length(dx) == 1)
    if (is.null(dy)) {
      dy <- dx / sqrt(3)
    }
    if (is.null(sep)) {
      list(x = rep.int(c(dx, dx, 0, -dx, -dx, 0), n), y = rep.int(c(
        dy,
        -dy, -2 * dy, -dy, dy, 2 * dy
      ), n), no.sep = TRUE)
    } else {
      list(
        x = rep.int(c(dx, dx, 0, -dx, -dx, 0, sep), n),
        y = rep.int(
          c(dy, -dy, -2 * dy, -dy, dy, 2 * dy, sep),
          n
        ), no.sep = FALSE
      )
    }
  }

  hex <- hexbin(dx = size) %>% 
    dplyr::as_tibble() %>% 
    dplyr::select(.data$x, .data$y) 
  hex <- rbind(hex, hex[1, ])
  
# -------------------------------------------------------------------------
  sq <- panel_size
  
  inv  <- tibble::tribble(
    ~x,  ~y, 
   -sq,  sq,
   -sq, -sq,
    sq, -sq,
    sq,  sq, 
    )
  inv <- rbind(inv, inv[1, ])

# -------------------------------------------------------------------------
  
  inversion <- rbind(inv, hex) %>% as.data.frame()
  names(inversion) <- c("x","y")
  
  hexbin <- ggplot2::ggplot() +
    
    ggplot2::geom_polygon(aes_(x = ~x, y = ~y)
                          , data = inversion
                          , fill = panel_color
                          ) +
    
    ggplot2::geom_polygon(aes_(x = ~x, y = ~y)
                          , data = hex
                          , size = border_width
                          , fill = background
                          , color = border_color
                          ) +
    ggplot2::theme(panel.background = element_rect(fill = background, color = NA)
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
                   , panel.border = element_blank()
                   ) 
    

  hexbin

}
