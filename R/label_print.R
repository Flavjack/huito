#' Label print
#'
#' Generate labels based in a data frame
#'
#' @param label Data frame to build the labels or n repeated labels
#' @param mode Label in "sample/preview" or "complete" mode
#' @param filename Labels file name
#' @param margin Labels margins. margin(t = 0, r = 0, b = 0, l = 0)
#' @param paper Paper size. Default A4 (21.0 x 29.7)
#' @param units Units for the label options
#' @param viewer Show the sample in the "Plots" or "Viewer" panel
#' @param smpres Sample resolution
#' @param nlabels Number of labels to generate
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
#' library(huito)
#' 
#' fb <- fieldbook
#' 
#' label <- fb %>% 
#' label_layout(size = c(10, 2.5)
#'              , border_color = "blue"
#'              ) %>%
#'   include_image(
#'     value = "https://flavjack.github.io/inti/img/inkaverse.png"
#'     , size = c(2.4, 2.4)
#'     , position = c(1.2, 1.25)
#'     ) %>%
#'   include_barcode(
#'     value = "barcode"
#'     , size = c(2.5, 2.5)
#'     , position = c(8.2, 1.25)
#'     ) %>%
#'   include_text(value = "plots"
#'                , position = c(9.7, 1.25)
#'                , angle = 90
#'                , size = 15
#'                , color = "red"
#'                ) %>%
#'   include_text(value = "Inkaverse"
#'                , position = c(4.6, 2)
#'                , size = 30
#'                , color = "brown"
#'                ) %>%
#'   include_text(value = "condition"
#'                , position = c(4.6, 1.2)
#'                , size = 13
#'                , color = "orange"
#'                ) %>%
#'   include_text(value = "genotypes"
#'                , position = c(4.6, 0.5)
#'                , size = 13
#'                , color = "#009966"
#'                ) %>% 
#'                label_print(mode = "sample")
#'   

label_print <- function(label
                        , mode = "sample"
                        , filename = "labels"
                        , margin = 0.04
                        , paper = c(21.0, 29.7)
                        , units = "cm"
                        , viewer = FALSE
                        , smpres = 200
                        , nlabels = NA
                        ) {
  
# args ------------------------------------------------------------------
  
  mode <- match.arg(mode, c("complete", "sample", "preview"))
  
  if (!tibble::is_tibble(label$data)) {
      
      label$data <- list(huito = NA) %>% 
        tibble::enframe()
      
    } 
# -------------------------------------------------------------------------
  
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
    rep(0, times = 4)
  } else if(is.character(margin)) {
    margin %>%
      gsub("[[:space:]]", "", .) %>%
      strsplit(., "[*]") %>%
      unlist() %>% as.numeric()
  } else if (length(margin) == 1 && is.numeric(margin)) {
    rep(margin, times = 4)
  } else {margin}
  
  # -------------------------------------------------------------------------
  
  fb <- if(mode == "sample" | mode == "preview") {
    
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
            , margin = NA_real_
            , panel_color = NA_real_
            , panel_size = NA_real_
            , opts = NA_real_
            )
  
  options <- label$opts %>%
    tidyr::pivot_wider(names_from = .data$option, values_from = .data$value) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>% 
    tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
    dplyr::na_if("NULL") %>% 
    dplyr::na_if("NA")
  
  # -------------------------------------------------------------------------
  
  label_dimension <- template$size %>%
    strsplit(split = "[*]") %>%
    unlist() %>%
    as.numeric()
  
  # unite-data --------------------------------------------------------------
  
  tolabel <- merge(options, info
                   , all.x = TRUE
                   , by = "value"
  ) %>%
    tidyr::separate(.data$position, c("X", "Y"), remove = F, sep = "[*]", fill = 'right') %>%
    tidyr::separate(.data$size, c("W", "H"), remove = F, sep = "[*]", fill = 'right') %>% 
    dplyr::mutate(layer = dplyr::case_when(
      .data$element %in% "label" ~ paste0("cowplot::ggdraw(xlim = c(", 0,",", W,")", ", ylim = c(", 0, ",", H, "), clip = 'on')")
      
      , .data$element %in% "text" & .data$type %in% "dynamic" ~ paste0("do.call(cowplot::draw_label"
                                                                      , ", list(label = '", info, "'"
                                                                      , ", x = ", X
                                                                      , ", y = ", Y
                                                                      , ", size = ", size
                                                                      , ", angle = ", angle
                                                                      , ", fontfamily = '", font, "'"
                                                                      , ", color = '", color, "'"
                                                                      , ", ", opts
                                                                      , "))")
      
      , .data$element %in% "text" & .data$type %in% "static" ~ paste0("do.call(cowplot::draw_label"
                                                                      , ", list(label = '", value, "'"
                                                                      , ", x = ", X
                                                                      , ", y = ", Y
                                                                      , ", size = ", size
                                                                      , ", angle = ", angle
                                                                      , ", fontfamily = '", font, "'"
                                                                      , ", color = '", color, "'"
                                                                      , ", ", opts
                                                                      , "))")
      
      , .data$element %in% "barcode" & .data$type %in% "dynamic" ~ paste0("cowplot::draw_plot(barcode_qr(",  "'", info , "'", ")"
                                                                          , ", x =", X, ", y =", Y
                                                                          , ", width =", W, ", height =", H
                                                                          , ", halign = 0.5, valign = 0.5"
                                                                          , ", hjust = 0.5, vjust = 0.5"
                                                                          , ")")

      , .data$element %in% "barcode" & .data$type %in% "static" ~ paste0("cowplot::draw_plot(barcode_qr(",  "'", value , "'", ")"
                                                                         , ", x =", X, ", y =", Y
                                                                         , ", width =", W, ", height =", H
                                                                         , ", halign = 0.5, valign = 0.5"
                                                                         , ", hjust = 0.5, vjust = 0.5"
                                                                         , ")")
      
      , .data$element %in% "image" & .data$type %in% "dynamic" ~ paste0("cowplot::draw_plot("
                                                                        , "grid::rasterGrob(image_import("
                                                                        , "'", info, "'"
                                                                        , ", '", opts, "'"
                                                                        , ")", ")"
                                                                        , ", x =", X, ", y =", Y
                                                                        , ", width =", W, ", height =", H
                                                                        , ", halign = 0.5, valign = 0.5"
                                                                        , ", hjust = 0.5, vjust = 0.5"
                                                                        , ")")
      
      , .data$element %in% "image" & .data$type %in% "static" ~ paste0("cowplot::draw_plot("
                                                                       , "grid::rasterGrob(image_import("
                                                                       , "'", value, "'"
                                                                       , ", '", opts, "'"
                                                                       , ")", ")"
                                                                       , ", x =", X, ", y =", Y
                                                                       , ", width =", W, ", height =", H
                                                                       , ", halign = 0.5, valign = 0.5"
                                                                       , ", hjust = 0.5, vjust = 0.5"
                                                                       , ")")
      
      , "shape" %in% .data$element & "static" %in% .data$type ~   paste0("cowplot::draw_plot(huito::shape_"
                                                                         , value
                                                                         , "(size = ", size
                                                                         , ", border_width = ", border_width
                                                                         , ", background = '", color
                                                                         , "', border_color = '", border_color
                                                                         , "' , margin = '", margin
                                                                         , "' , panel_color = '", panel_color
                                                                         , "' , panel_size = ", panel_size
                                                                         , ")"
                                                                         , ", width = ", size
                                                                         , ", height = ", size
                                                                         , ", x = ", X, ", y = ", Y
                                                                         , ", halign = 0.5, valign = 0.5"
                                                                         , ", hjust = 0.5, vjust = 0.5"
                                                                         , ")")
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
  
  showtext::showtext_auto(enable = TRUE)
  
  if (mode =="sample"| mode == "preview") {
    
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
    
    # open 
    
    ancho <- (margin[4] + label_dimension[1] + margin[2])
    alto <- (margin[1] + label_dimension[2] + margin[3])
    
    ggplot2::ggsave(
      filename = label_sample
      , plot = label_print
      , units = template$units
      , width = ancho
      , height = alto
      , limitsize = FALSE
    )
    
    if(isFALSE(viewer)) {
      
      label_sample %>% 
        magick::image_read_pdf(density = smpres) %>% 
        plot(grDevices::as.raster(.))
      
    } else {
      
      label_sample %>% 
        magick::image_read_pdf(density = smpres) %>% 
        print()
    }
    
  }
  
  # -------------------------------------------------------------------------
  
  nlabels <- if(is.na(nlabels)) { nrow(fb) } else { nlabels }
  
  if (mode =="complete") {
    
    label_width <- (margin[4] + label_dimension[1] + margin[2])
    label_height <- (margin[1] + label_dimension[2] + margin[3])
    
    ncol <- (paper[1]/label_width) %>% trunc()
    nrow <- (paper[2]/label_height) %>% trunc()
    pages <- ceiling((nlabels/(ncol*nrow)))
    
    label_list <- 1:nlabels %>%
      purrr::map(function(x) {
        
        layers <- tolabel %>%
          dplyr::filter(.data$nlabel %in% c(0, x)) %>%
          dplyr::select(.data$layer) %>%
          tibble::deframe() %>%
          paste0(., collapse = " + ")
        
        eval(parse(text = paste(layers))) + frame
        
      })
    
    # -------------------------------------------------------------------------
    
    grids <- if(nlabels == 1) { 0 } else {
      
      seq(from = 0, to = nlabels, by = ncol*nrow)
      
    }
    
    file_output <- filename %>% sub("\\..*", "", .) %>% paste0(., ".pdf")
    
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
          , "label" %>% paste0(., x,".pdf")
        )
        
        ancho <- ncol*label_width
        alto <- nrow*label_height
        
        cowplot::ggsave2(
          filename = pdf_file
          , plot = labels
          , units = template$units
          , width = ancho
          , height = alto
          , limitsize = FALSE
        )
        
      }) %>%
      pdftools::pdf_combine(
        input = .
        , output = file_output
      ) 
    
    showtext::showtext_auto(enable = FALSE)
    
    path <- file_output
    
  }
  
}