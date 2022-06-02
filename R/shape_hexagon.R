#' Shape hexagon
#'
#' Hexagon geom shape for ggplot2
#' 
#' @param size hexagon size (numeric: 5.08)
#' @param border_width line width (numeric: 1)
#' @param background background color (string: "transparent")
#' @param border_color border color (string: "black")
#' @param units units for shape (string: "cm")
#' @param panel_color panel color (string: "green")
#' @param panel_size panel size (numeric: NA)
#' 
#' @return geom
#'
#' @export
#' 
#' @examples
#' 
#' library(huito)
#' 
#' shape_hexagon(border_width = 1
#'               , background = "red"
#'               #, panel_size = 5.08
#'               )
#'

shape_hexagon <- function (size = 5.08
                          , border_width = NA
                          , background = NA
                          , border_color = "black"
                          , units = "cm"
                          , panel_color = "green"
                          , panel_size = NA
                          ) {
  

# -------------------------------------------------------------------------

#> https://stackoverflow.com/questions/41261785/clip-cut-everything-outside-of-polygon-or-fill-the-outside-with-white
  
if(FALSE) {
  
  size = 5.08
  border_width = 3
  background = NA
  border_color = "black"
  units = "cm"
  panel_color = "red"
  panel_size = NA

}
  
# -------------------------------------------------------------------------

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
    sq, -sq,
    sq,  sq, 
    -sq,  sq,
    -sq, -sq,
  )
  inv <- rbind(inv, inv[1, ])
  
  # -------------------------------------------------------------------------
  
  inversion <- rbind(inv, hex) %>% as.data.frame()
  names(inversion) <- c("x","y")
  
  plot <- ggplot2::ggplot() +
    scale_x_continuous(expand = c(0, border_width/11)) +
    scale_y_continuous(expand = c(0, border_width/11)) +
    ggplot2::theme(panel.background = element_rect(colour = panel_color, size = border_width*2, fill = NA)
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
                   , plot.margin = unit(rep(0, 4), units)
                   , panel.border = element_blank()
                   ) +
    ggplot2::geom_polygon(aes_(x = ~x, y = ~y)
                          , data = inversion
                          , fill = panel_color
                          ) +
    ggplot2::geom_polygon(aes_(x = ~x, y = ~y)
                          , data = hex
                          , size = border_width
                          , color = border_color
                          , fill = background
                          ) 
  
  plot

  # splot <- plot %>%
  #   ggsave2(filename = tempfile(fileext = ".pdf")
  #           , width = panel_size, height = panel_size
  #           , units = units)
  # 
  # browseURL(splot)

  # hexbin <- image_read_pdf(splot) %>%
  #   grid::rasterGrob()
  # 
  # hexbin

}
