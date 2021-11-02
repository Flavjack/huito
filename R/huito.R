#' Label generator
#'
#' Generate labels based in a data frame
#'
#' @param data data frame to build the labels
#' @param options data frame. See details
#' @param complete logical value for build all labels
#' @param ncol Number of column for the label in complete mode
#' @param file path of the final file
#' @param dpi images resolution
#'
#' @details
#'
#' `options` is build with the function: `huito_label_options()`
#'
#' The Labels are build by layers
#'
#' `elements` = "label" | "barcode" | "figure" | "text"
#' `type` = "template" | "column" | "manual"
#' `value` = 'column' name from data frame or 'manual' for write personal info
#' `dimension` = dimension for the labels and images
#' `position` = values represented by the unit used for position(x, y) inside the label
#' `color` = color by the name or code
#' `margin` = space between labels
#' `unit` = units used for the design and image dimension
#' `border` = color border for the label
#' `size` = font size
#' `angle` = angle for the text
#' `font` = font type from Google fonts
#'
#'  For more details, see example
#'
#' @return image or pdf
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
#' url_fb <- paste0("https://docs.google.com/spreadsheets/d/"
#'          , "1q0EZmZBt52ca-0VbididjJy2jXTwf06laJpzvkQJWvc/edit#gid=107939497")
#'
#' fb <- gsheet2tbl(url_fb)
#'
#' lyr_temp <- list(element = "label"
#'                  , type = "template"
#'                  , dimension = "10*2.5"
#'                  , color = "white"
#'                  , margin = 0.04
#'                  , unit = "cm"
#'                  , border = "red")
#'
#' lyr_qr <- list(element = "barcode"
#'                , type = "column"
#'                , value = "qr-code"
#'                , position = "7*0.1"
#'                , dimension = "2.3*2.3"
#'                , unit = "cm")
#'
#' lyr_pic <- list(element = "figure"
#'                 , type = "column"
#'                 , value = "image"
#'                 , position = "2.3*0.5"
#'                 , dimension = "1.5*1.5"
#'                 , unit = "cm")
#'
#' lyr_wf <- list(element = "figure"
#'                , type = "manual"
#'                , value = paste0("https://upload.wikimedia.org/"
#'                                 , "wikipedia/commons/a/a9/Unalm_logo.png")
#'                , position = "0.1*0.2"
#'                , dimension = "2*2"
#'                , unit = "cm")
#'
#' txt5 <- list(element = "text"
#'              , type = "column"
#'              , value = "variedad"
#'              , size = 11
#'              , angle = 0
#'              , font = "Permanent Marker"
#'              , color = "black"
#'              , position = "5.5*1.6")
#'
#' txt6 <- list(element = "text"
#'              , type = "column"
#'              , value = "temperatura"
#'              , size = 11
#'              , angle = 0
#'              , font = "Permanent Marker"
#'              , color = "black"
#'              , position = "5.5*0.5")
#'
#' txt7 <- list(element = "text"
#'              , type = "column"
#'              , value = "plots"
#'              , size = 11
#'              , angle = 90
#'              , font = "Permanent Marker"
#'              , color = "blue"
#'              , position = "9.7*1.3")
#'
#' txt8 <- list(element = "text"
#'              , type = "manual"
#'              , value = "Variedad"
#'              , size = 12
#'              , angle = 0
#'              , font = "Permanent Marker"
#'              , color = "red"
#'              , position = "5.5*2.1")
#'
#' txt9 <- list(element = "text"
#'              , type = "manual"
#'              , value = "Temperature"
#'              , size = 12
#'              , angle = 0
#'              , font = "Permanent Marker"
#'              , color = "red"
#'              , position = "5.5*1")
#'
#' lyr_bg <- list(element = "figure"
#'                , type = "manual"
#'                , value = paste0("https://imborrable.com/wp-content/"
#'                     , "uploads/2021/01/psicologia-del-color-significados.png")
#'                , position = "0*0"
#'                , dimension = "10*5"
#'                , unit = "cm")
#'
#' rm(lyr_bg)
#'
#' labels <- huito_label_options(c("lyr", "txt")) %>%
#'   huito_labels(data = fb, options = ., complete = F)
#'
#' }
#'

huito_labels <- function(data
                         , options
                         , ncol = 2
                         , complete = FALSE
                         , file = "labels.pdf"
                         , dpi = 100
                         ) {

  # Parameters --------------------------------------------------------------

  info <- data %>%
    mutate(nlabel = row.names(.)) %>%
    mutate(across(everything(), as.character)) %>%
    pivot_longer(!.data$nlabel
                 , names_to = "value"
                 , values_to = "info")

  opts <- options %>%
    mutate(active = case_when(
      type %in% "column" & value %in% names(data) ~ "yes"
      , type %in% "column" & !value %in% names(data) ~ "no"
      , TRUE ~ "yes"
    )) %>%
    filter(.data$active %in% "yes") %>%
    mutate(across(everything(), as.character)) %>%
    mutate(nlayer = row.names(.))

  template <- opts %>%
    filter(.data$type %in% "template") %>%
    select(!c(.data$type, .data$value, .data$nlayer)) %>%
    pivot_longer(!c(.data$element), names_to = "variable", values_to = "value") %>%
    drop_na(.data$value) %>%
    unite("opts", c(.data$element, .data$variable)) %>%
    deframe() %>%
    as.list()

  columns <- opts %>%
    filter(!.data$type %in% "template")

  label_dimension <- template$label_dimension %>%
    strsplit(split = "[*]") %>% unlist() %>% as.numeric()

  label_margin <- template$label_margin %>% as.numeric()

  # fonts -------------------------------------------------------------------

  fonts <- columns %>%
    select(.data$font) %>%
    unique() %>%
    drop_na() %>%
    mutate(fun = paste0("sysfonts::font_add_google(name = ", "'" , .data$font, "'",")")) %>%
    select(.data$fun) %>%
    purrr::as_vector() %>%
    paste0(., collapse = "; ")

  eval(parse(text = fonts))

  showtext::showtext_auto()

  # unite-data --------------------------------------------------------------

  tolabel <- merge(opts, info
                   , all.x = TRUE
                   , by = "value") %>%
    separate(.data$position, c("X", "Y"), remove = F, sep = "[*]") %>%
    separate(.data$dimension, c("W", "H"), remove = F, sep = "[*]") %>%
    mutate(layer = case_when(
      element %in% "label" ~ paste0("cowplot::ggdraw(xlim = c(", 0,",", W,")", ", ylim = c(", 0, "," , H,"))")
      , element %in% "text" & type %in% "column" ~ paste0("cowplot::draw_label(", "'", info, "'"
                                                          , ", x =", X, ", y =", Y
                                                          , ", size =", size, ", angle =", angle
                                                          , ", fontfamily =", "'", font, "'"
                                                          , ", color =", "'", color, "'", ")")
      , element %in% "text" & type %in% "manual" ~ paste0("cowplot::draw_label(", "'", value, "'"
                                                          , ", x =", X, ", y =", Y
                                                          , ", size =", size, ", angle =", angle
                                                          , ", fontfamily =", "'", font, "'"
                                                          , ", color =", "'", color, "'", ")")
      , element %in% "barcode" & type %in% "column" ~ paste0("cowplot::draw_plot(huito_barcode(",  "'", info , "'", ")"
                                                             , ", x =", X, ", y =", Y
                                                             , ", width =", W, ", height =", H, ")")
      , element %in% "barcode" & type %in% "manual" ~ paste0("cowplot::draw_plot(huito_barcode(",  "'", value , "'", ")"
                                                             , ", x =", X, ", y =", Y
                                                             , ", width =", W, ", height =", H, ")")
      , element %in% "figure" & type %in% "column" ~ paste0("cowplot::draw_plot(", "grid::rasterGrob(magick::image_read(", "'", info, "'", ")", ")"
                                                            , ", x =", X, ", y =", Y
                                                            , ", width =", W, ", height =", H, ")")
      , element %in% "figure" & type %in% "manual" ~ paste0("cowplot::draw_plot(", "grid::rasterGrob(magick::image_read(", "'", value, "'", ")", ")"
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
                 , plot.margin = unit(rep(template$label_margin, 4), template$label_unit)
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
                   , units = template$label_unit
                   , width = label_dimension[1] + label_margin*2
                   , height = label_dimension[2] + label_margin*2
                   , dpi = dpi
                   , limitsize = FALSE)

  # -------------------------------------------------------------------------

  if (isTRUE(complete)) {

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
