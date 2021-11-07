#' Label print
#'
#' Generate labels based in a data frame
#'
#' @param label data frame to build the labels
#' @param mode label in "sample" or "complete" mode
#' @param filename labels file name
#' @param margin labels margins. margin(t = 0, r = 0, b = 0, l = 0)
#' @param paper paper size
#' @param units units for the label options
#' @param fonts For add new fonts from google fonts. Only active when you import
#'   new fonts
#' @param viewer show the sample in the "Plots" or "Viewer" panel
#'
#' @return pdf
#'
#' @import ggplot2
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
#' library(huito)
#' library(gsheet)
#'
#' url <- paste0("https://docs.google.com/spreadsheets/d/"
#'        , "1q0EZmZBt52ca-0VbididjJy2jXTwf06laJpzvkQJWvc/edit#gid=107939497")
#' fb <- gsheet2tbl(url)
#'
#' label <- label_layout(data = fb
#'                      , size = c(10, 2.5)
#'                      ) %>%
#'          include_image(
#'                value = "https://flavjack.github.io/inti/img/inkaverse.png"
#'                , size = c(2.4, 2.4)
#'                , position = c(0.1, 0.05)
#'                ) %>%
#'          include_barcode(
#'                value = "qr-code"
#'                , size = c(2.2, 2.2)
#'                , position = c(7.2, 0.15)
#'                ) %>%
#'          include_text(value = "plots", position = c(9.7, 1.25), angle = 90, size = 15) %>%
#'          include_text(value = "Inkaverse", position = c(4.5, 1.25), size = 18) %>%
#'          label_print("sample")
#'
#' }
#' 

label_print <- function(label
                        , mode = "sample"
                        , filename = "labels"
                        , margin = 0.04
                        , paper = c(21.0, 29.7)
                        , units = "cm"
                        , fonts = FALSE
                        , viewer = FALSE
                         ) {

# test --------------------------------------------------------------------

if(FALSE) {
  
  mode = "s"
  margin = 0.04
  paper = c(21.0, 29.7)
  units = "cm"
  filename = "label"
  fonts = F
  viewer = T
}

# args ------------------------------------------------------------------
  
  mode <- match.arg(mode, c("complete", "sample"))
  
  paper <- if(any(is.null(paper)) || any(is.na(paper)) || any(paper == "")) {
    c(21.0, 29.7)
  } else if(is.character(paper)) {
    paper %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else {paper}
  
  margin <- if(any(is.null(margin)) || any(is.na(margin)) || 
               any(margin == "") ) {
    rep(0.04, times = 4)
  } else if(is.character(margin)) {
    margin %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else if (length(margin) == 1 && is.numeric(margin)) {
    rep(margin, times = 4)
  } else {margin}
  
# -------------------------------------------------------------------------
  
  if (!tibble::is_tibble(label$data)) {
  
  label$data <- list(huito = NA) %>% 
    tibble::enframe()
  
  } 
  
# -------------------------------------------------------------------------

  fb <- if(mode == "sample") {
    
    label$data %>% dplyr::slice_sample(n = 1)
    
  } else if (mode == "complete") {
    
    label$data 
    
  } 

# parameters --------------------------------------------------------------

  info <- fb %>%
    dplyr::mutate(nlabel = row.names(.)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::pivot_longer(!.data$nlabel
                 , names_to = "value"
                 , values_to = "info")
  
  template <- label$opts %>%
    dplyr::filter(.data$element %in% "label") %>%
    dplyr::select(.data$option, .data$value) %>%
    tibble::deframe()

  cols <- c(value = NA_real_
            , angle = NA_real_
            , position = NA_real_
            , size = NA_real_
            , font = NA_real_
            )
  
  options <- label$opts %>%
    tidyr::pivot_wider(names_from = .data$option, values_from = .data$value) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>% 
    tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
    dplyr::na_if("NULL") 

# -------------------------------------------------------------------------

  label_dimension <- template$size %>%
    strsplit(split = "[*]") %>%
    unlist() %>%
    as.numeric()

# fonts -------------------------------------------------------------------
  
  if (isTRUE(fonts)) {
    
    fonts <- options %>%
      dplyr::select(.data$font) %>%
      dplyr::na_if("NULL") %>%
      unique() %>%
      tidyr::drop_na() %>%
      dplyr::mutate(fun = paste0("sysfonts::font_add_google(name = ", "'" , .data$font, "'",")")) %>%
      dplyr::select(.data$fun) %>%
      purrr::as_vector() %>%
      paste0(., collapse = "; ")

    eval(parse(text = fonts))
    
  }

  showtext::showtext_auto()

# unite-data --------------------------------------------------------------
  
  tolabel <- merge(options, info
                   , all.x = TRUE
                   , by = "value"
                   ) %>%
    tidyr::separate(.data$position, c("X", "Y"), remove = F, sep = "[*]", fill = 'right') %>%
    tidyr::separate(.data$size, c("W", "H"), remove = F, sep = "[*]", fill = 'right') %>% 
    dplyr::mutate(layer = dplyr::case_when(
      .data$element %in% "label" ~ paste0("cowplot::ggdraw(xlim = c(", 0,",", W,")", ", ylim = c(", 0, "," , H,"))")
      , .data$element %in% "text" & .data$type %in% "dynamic" ~ paste0("cowplot::draw_label(", "'", info, "'"
                                                          , ", x =", X, ", y =", Y
                                                          , ", size =", size, ", angle =", angle
                                                          , ", fontfamily =", "'", font, "'"
                                                          , ", color =", "'", color, "'", ")")
      , .data$element %in% "text" & .data$type %in% "static" ~ paste0("cowplot::draw_label(", "'", value, "'"
                                                          , ", x =", X, ", y =", Y
                                                          , ", size =", size, ", angle =", angle
                                                          , ", fontfamily =", "'", font, "'"
                                                          , ", color =", "'", color, "'", ")")
      , .data$element %in% "barcode" & .data$type %in% "dynamic" ~ paste0("cowplot::draw_plot(barcode_qr(",  "'", info , "'", ")"
                                                             , ", x =", X, ", y =", Y
                                                             , ", width =", W, ", height =", H, ")")
      , .data$element %in% "barcode" & .data$type %in% "static" ~ paste0("cowplot::draw_plot(barcode_qr(",  "'", value , "'", ")"
                                                             , ", x =", X, ", y =", Y
                                                             , ", width =", W, ", height =", H, ")")
      , .data$element %in% "image" & .data$type %in% "dynamic" ~ paste0("cowplot::draw_plot(", "grid::rasterGrob(magick::image_read(", "'", info, "'", ")", ")"
                                                            , ", x =", X, ", y =", Y
                                                            , ", width =", W, ", height =", H, ")")
      , .data$element %in% "image" & .data$type %in% "static" ~ paste0("cowplot::draw_plot(", "grid::rasterGrob(magick::image_read(", "'", value, "'", ")", ")"
                                                            , ", x =", X, ", y =", Y
                                                            , ", width =", W, ", height =", H, ")")
      
      , .data$element %in% "shape" & .data$type %in% "static" ~   paste0("cowplot::draw_plot(huito::shape_"
                                                                         , value
                                                                         , "(border_width = ", border_width
                                                                         , ", background = '", color
                                                                         , "', border_color = '", border_color
                                                                         , "', margin = ", margin
                                                                         , ", units = '", units
                                                                         , "')"
                                                                         , ", width = ", W
                                                                         , ", height = ", H
                                                                         , ", x = ", X
                                                                         , ", y = ", Y
                                                                         , ", halign = 0, valign = 0"
                                                                         , ")"
                                                                         )
      )) %>%
    dplyr::select(.data$nlayer, .data$nlabel, .data$layer) %>%
    dplyr::mutate(dplyr::across(c(.data$nlayer, .data$nlabel), as.numeric)) %>%
    dplyr::arrange(.data$nlayer, .data$nlabel, .by_group = T) %>% 
    dplyr::select(!.data$nlayer) %>% 
    replace(is.na(.), 0)
  
# frame -------------------------------------------------------------------

  frame <- theme(
    panel.background = element_rect(fill = template$color, colour = NA)
    , panel.border = element_rect(fill = NA
                                  , colour = template$border_color
                                  , size = template$border_width
                                  )
    , plot.margin = unit(margin, template$units)
    , complete = TRUE
    )

# -------------------------------------------------------------------------

if (mode =="sample") {

  layers <- tolabel %>%
    dplyr::filter(.data$nlabel %in% c(0, 1)) %>%
    dplyr::select(.data$layer) %>%
    tibble::deframe() %>%
    paste0(., collapse = " + ")
  
  label_print <- eval(parse(text = paste(layers))) + frame 
  
  label_sample <- file.path(
    tempdir()
    , "sample.pdf"
    )
  
  cowplot::ggsave2(
    filename = label_sample
    , plot = label_print
    , units = template$units
    , width = (margin[4] + label_dimension[1] + margin[2])
    , height = (margin[1] + label_dimension[2] + margin[3])
    , limitsize = FALSE
    )  
  
  if(isFALSE(viewer)) {
    
    label_sample %>% 
      magick::image_read_pdf(density = 200) %>% 
      plot(grDevices::as.raster(.))
    
  } else {
    
    label_sample %>% 
      magick::image_read_pdf(density = 200) %>% 
      print()
  }

}

# -------------------------------------------------------------------------

  if (mode =="complete") {
    
    label_width <- (margin[4] + label_dimension[1] + margin[2])
    label_height <- (margin[1] + label_dimension[2] + margin[3])

    ncol <- (paper[1]/label_width) %>% trunc()
    nrow <- (paper[2]/label_height) %>% trunc()
    pages <- ceiling((nrow(fb)/(ncol*nrow)))

    label_list <- 1:nrow(fb) %>%
      purrr::map(function(x) {

            layers <- tolabel %>%
              dplyr::filter(.data$nlabel %in% c(0, x)) %>%
              dplyr::select(.data$layer) %>%
              tibble::deframe() %>%
              paste0(., collapse = " + ")

            eval(parse(text = paste(layers))) + frame

        })

# -------------------------------------------------------------------------

    grids <- seq(from = 0, to = nrow(fb), by = ncol*nrow)

    pdf <-1:length(grids) %>%
      purrr::map(function(x) {

        ini <- grids[x] + 1
        fin <- grids[x] + ncol*nrow

        plotlabs <- label_list[c(ini:fin)]

        labels <- cowplot::plot_grid(
          plotlist = plotlabs
          , ncol = ncol
          )

        pdf_file <- file.path(
          tempdir()
          , filename %>% sub("\\..*", "", .) %>% paste0(., x,".pdf")
          )
        
        cowplot::ggsave2(
          filename = pdf_file
           , plot = labels
           , units = template$units
           , width = ncol*label_width
           , height = nrow*label_height
           , limitsize = FALSE
           )
        
        }) %>%
      qpdf::pdf_combine(
        input = .
        , output = filename %>% sub("\\..*", "", .) %>% paste0(., ".pdf")
        ) 
    
    path <- filename %>% sub("\\..*", "", .) %>% paste0(., ".pdf")
    
  }

}

