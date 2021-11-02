#' Label print
#'
#' Generate labels based in a data frame
#'
#' @param label data frame to build the labels
#' @param mode label in sample or complete mode
#' @param filename path of the final file
#' @param dpi images resolution
#'
#'
#' @return png or pdf
#'
#' @importFrom sysfonts font_add_google
#' @importFrom cowplot draw_plot
#' @importFrom grid grid.raster
#' @importFrom magick image_read
#' @importFrom showtext showtext_auto
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(gsheet)
#'
#' url <- "https://docs.google.com/spreadsheets/d/1q0EZmZBt52ca-0VbididjJy2jXTwf06laJpzvkQJWvc/edit#gid=107939497"
#' fb <- gsheet2tbl(url)
#'
#' label <- label(data = fb
#'                , size = c(10, 2.5)
#'                ) %>%
#'          include_image(
#'                value = "https://inkaverse.com/reference/figures/logo.png"
#'                , size = c(2.2,2.2)
#'                , position = c(0.2, 0.15)
#'                ) %>%
#'          include_barcode(
#'                value = "qr-code"
#'                , size = c(2.2, 2.2)
#'                , position = c(7, 0.15)
#'                ) %>%
#'          include_text(value = "plots", position = c(9.7, 1.25), angle = 90) %>%
#'          label_print()
#'
#' }
#'

label_print <- function(label
                        , filename = "label"
                        , mode = "sample"
                        , margin = NA
                        , paper = NA
                        , units = "cm"
                        , dpi = 100
                         ) {


# test --------------------------------------------------------------------

if(FALSE) {

  label = label
  mode = "sample"
  margin = NA
  paper = NA
  units = "cm"
  dpi = 100
  file = "label"

}

# args ------------------------------------------------------------------

  paper <- if(any(is.null(paper)) || any(is.na(paper)) || any(paper == "")) {
    c(21.0, 29.7)
  } else if(is.character(paper)) {
    paper %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else {paper}

  margin <- if(any(is.null(margin)) || any(is.na(margin)) || any(margin == "")) {
    rep(0.05, times = 4)
  } else if(is.character(margin)) {
    margin %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else {margin}

  data <- label$data

# parameters --------------------------------------------------------------

  info <- label$data %>%
    mutate(nlabel = row.names(.)) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(!.data$nlabel
                 , names_to = "value"
                 , values_to = "info")

  template <- label$opts %>%
    filter(element %in% "label") %>%
    select(option, value) %>%
    deframe()

  options <- label$opts %>%
    pivot_wider(names_from = option, values_from = value) %>%
    mutate(across(everything(), as.character)) %>%
    mutate(nlayer = row.names(.))

# -------------------------------------------------------------------------

  label_dimension <- template$size %>%
    strsplit(split = "[*]") %>%
    unlist() %>%
    as.numeric()

  label_margin <- margin[1] %>% as.numeric()

# fonts -------------------------------------------------------------------

  fonts <- options %>%
    select(.data$font) %>%
    na_if("NULL") %>%
    unique() %>%
    drop_na() %>%
    mutate(fun = paste0("sysfonts::font_add_google(name = ", "'" , .data$font, "'",")")) %>%
    select(.data$fun) %>%
    purrr::as_vector() %>%
    paste0(., collapse = "; ")

  eval(parse(text = fonts))

  showtext::showtext_auto()

# unite-data --------------------------------------------------------------

  tolabel <- merge(options, info
                   , all.x = TRUE
                   , by = "value") %>%
    separate(.data$position, c("X", "Y"), remove = F, sep = "[*]") %>%
    separate(.data$size, c("W", "H"), remove = F, sep = "[*]") %>%
    mutate(layer = case_when(
      element %in% "label" ~ paste0("cowplot::ggdraw(xlim = c(", 0,",", W,")", ", ylim = c(", 0, "," , H,"))")
      , element %in% "text" & type %in% "dynamic" ~ paste0("cowplot::draw_label(", "'", info, "'"
                                                          , ", x =", X, ", y =", Y
                                                          , ", size =", size, ", angle =", angle
                                                          , ", fontfamily =", "'", font, "'"
                                                          , ", color =", "'", color, "'", ")")
      , element %in% "text" & type %in% "static" ~ paste0("cowplot::draw_label(", "'", value, "'"
                                                          , ", x =", X, ", y =", Y
                                                          , ", size =", size, ", angle =", angle
                                                          , ", fontfamily =", "'", font, "'"
                                                          , ", color =", "'", color, "'", ")")
      , element %in% "barcode" & type %in% "dynamic" ~ paste0("cowplot::draw_plot(barcode_qr(",  "'", info , "'", ")"
                                                             , ", x =", X, ", y =", Y
                                                             , ", width =", W, ", height =", H, ")")
      , element %in% "barcode" & type %in% "static" ~ paste0("cowplot::draw_plot(barcode_qr(",  "'", value , "'", ")"
                                                             , ", x =", X, ", y =", Y
                                                             , ", width =", W, ", height =", H, ")")
      , element %in% "image" & type %in% "dynamic" ~ paste0("cowplot::draw_plot(", "grid::rasterGrob(magick::image_read(", "'", info, "'", ")", ")"
                                                            , ", x =", X, ", y =", Y
                                                            , ", width =", W, ", height =", H, ")")
      , element %in% "image" & type %in% "static" ~ paste0("cowplot::draw_plot(", "grid::rasterGrob(magick::image_read(", "'", value, "'", ")", ")"
                                                            , ", x =", X, ", y =", Y
                                                            , ", width =", W, ", height =", H, ")")
    )) %>%
    drop_na("layer") %>%
    select(.data$nlayer, .data$nlabel, .data$value, .data$layer) %>%
    mutate(across(c(.data$nlayer, .data$nlabel), as.numeric)) %>%
    arrange(.data$nlayer, .data$nlabel, .by_group = T)

# frame -------------------------------------------------------------------

  frame <- theme(panel.background = element_rect(fill = template$label_color, colour = NA)
                 , panel.border = element_rect(fill = NA, colour = template$label_border)
                 , plot.margin = unit(margin, template$units)
                 , complete = TRUE)

# -------------------------------------------------------------------------

  layers <- tolabel %>%
    filter(.data$nlabel %in% c(NA, 1)) %>%
    select(.data$layer) %>%
    deframe() %>%
    paste0(., collapse = " + ")

  label <- eval(parse(text = paste(layers))) + frame

  cowplot::ggsave2(filename = file %>% sub("\\..*", "", .) %>% paste0(., ".png")
                   , plot = label
                   , units = template$units
                   , width = label_dimension[1] + label_margin*2
                   , height = label_dimension[2] + label_margin*2
                   , dpi = dpi
                   , limitsize = FALSE)

  # -------------------------------------------------------------------------

  if (mode("complete")) {

    lblist <- 1:nrow(data) %>% purrr::map(
      function(x) {

        layers <- tolabel %>%
          filter(.data$nlabel %in% c(NA, tolabel[,2][x+2] )) %>%
          select(.data$layer) %>%
          deframe() %>%
          paste0(., collapse = " + ")

        eval(parse(text = paste(layers))) + frame

      }
    )

    labels <- cowplot::plot_grid(plotlist = lblist, ncol = ncol)

    cowplot::ggsave2(filename = file
                     , plot = labels
                     , units = template$label_unit
                     , width = (label_dimension[1] + label_margin*2)*ncol
                     , height = (label_dimension[2] + label_margin*2)*ceiling(nrow(data)/ncol)
                     , dpi = dpi
                     , limitsize = FALSE)

  }

  "Your label is Ready!" %>% print()

}
