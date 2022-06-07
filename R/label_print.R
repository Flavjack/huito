#' Label print
#'
#' Generate labels based in a data frame
#'
#' @param label Data frame to build the labels or n repeated labels (table/numeric)
#' @param mode Label generation (string: "sample/preview", "complete") 
#' @param filename Labels file name (string: "labels")
#' @param margin Labels margins. margin(numeric vector: t = 0, r = 0, b = 0, l = 0)
#' @param paper Paper size. Default A4 (numeric vector: 21.0 x 29.7)
#' @param units Units for the label options (string: "cm")
#' @param viewer Visualization of the label (logial: FALSE)
#' @param smpres Sample resolution if viewer = TRUE (numeric: 200)
#' @param nlabels Number of labels to generate (numeric: NA)
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
  
  if (FALSE) {
    
    mode = "c"
    filename = "labels"
    margin = 0
    paper = c(21.0, 29.7)
    units = "cm"
    viewer = FALSE
    smpres = 200
    nlabels = NA
    
  }
  
  
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
  
  margin <- if(any(is.null(margin)) || any(is.na(margin)) || any(margin == "") ) {
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
    
    label$data %>% 
     { if(!is.na(nlabels)) { head(x = ., nlabels) } else {.} }
    
  } 
  
# parameters --------------------------------------------------------------
  
  info <- fb %>%
    dplyr::mutate(nlabel = row.names(.)) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::pivot_longer(!.data$nlabel
                        , names_to = "row"
                        , values_to = "info") 
  
  opts <- label$opts %>% 
    mutate(nlayer = case_when(
      .data$element %in% "label" ~ "0-99"
      , TRUE ~ as.character(.data$nlayer)
    )) %>%  
    tidyr::separate_rows(nlayer, sep = "-") %>% 
    dplyr::rename(class = element) %>% 
    dplyr::mutate(type = case_when(
      .data$value %in% info$row ~ "dynamic"
      , TRUE ~ "static"
    )) %>%
    dplyr::mutate(row = case_when(
      .data$option %in% "value" ~ as.character(.data$value)
      , .data$class %in% "label" ~ "template"
    )) %>% 
    tidyr::fill(.data$row)
  
  dynamic <- opts %>% 
    dplyr::filter(.data$type %in% "dynamic") %>% 
    dplyr::select(!.data$row) %>% 
    dplyr::rename(row = value) %>% 
    merge(info, ., by = "row") %>% 
    dplyr::arrange(.data$nlabel, .data$nlayer) %>% 
    dplyr::select(.data$nlabel, .data$nlayer, .data$class, .data$option, .data$info) %>% 
    tidyr::pivot_wider(names_from = .data$option, values_from = .data$info) %>% 
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) 
  
  static <- opts %>% 
    dplyr::filter(.data$type %in% "static") %>% 
    dplyr::select(.data$class, .data$option, .data$value, .data$nlayer) %>% 
    tidyr::pivot_wider(names_from = .data$option, values_from = .data$value) %>% 
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) 
  
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
  
  dt2label <- merge(dynamic, static
                    , by = c("class", "nlayer"), all = T) %>% 
    dplyr::select(.data$class, .data$nlabel, .data$nlayer, .data$value, everything()) %>% 
    dplyr::arrange(.data$nlabel, .data$nlayer) %>% 
    dplyr::na_if("NULL") %>% 
    pivot_longer(!c(.data$class:.data$nlayer)) %>% 
    drop_na(value) %>% 
    mutate(name = gsub(".x|.y", "", .data$name)) %>% 
    dplyr::mutate(nlabel = replace_na(.data$nlabel, "0")) %>% 
    pivot_wider() %>% 
    dplyr::arrange(.data$nlabel, .data$nlayer) %>% 
    tibble::add_column(!!!cols[!names(cols) %in% names(.)]) %>% 
    #> add conditional opts
    tidyr::separate(.data$position, c("X", "Y"), remove = F, sep = "[*]", fill = 'right') %>%
    tidyr::separate(.data$size, c("W", "H"), remove = F, sep = "[*]", fill = 'right') %>% 
    dplyr::mutate(border_width = case_when(
      border_width %in% 0 ~ "element_blank()"
      , border_width > 0 ~ paste0("element_rect(fill = NA, colour = , '", border_color ,"'"
                                  , ", size =", border_width, ")")
    )) %>% 
    dplyr::mutate(margin = case_when(
      .data$class %in% "label" ~ paste0("c(", paste0({{margin}}, collapse = ","), ")")
      , TRUE ~ as.character(.data$margin)
    ))
    
# unite-data --------------------------------------------------------------
  
  tolabel <- dt2label %>%
    dplyr::mutate(layer = dplyr::case_when(
      
      .data$class %in% "label" & nlayer %in% 0 ~ paste0("cowplot::ggdraw(xlim = c(0,",  W ,")"
                                                        , ", ylim = c(0,", H, ")"
                                                        , ", clip = 'on')"
                                                        )
      
      , .data$class %in% "label" & nlayer %in% 99 ~ paste0("theme(panel.background = element_rect(fill = '" , .data$color, "'"
                                            , ", colour = NA)"
                                            , ", panel.border = ", .data$border_width
                                            , ", plot.margin = unit(", .data$margin, ", '", .data$units, "')"
                                            , ", complete = TRUE)"
                                            )
      
      , .data$class %in% "text" ~ paste0("do.call(cowplot::draw_label"
                                          , ", list(label = '", value, "'"
                                          , ", x = ", X
                                          , ", y = ", Y
                                          , ", size = ", size
                                          , ", angle = ", angle
                                          , ", fontfamily = '", font, "'"
                                          , ", color = '", color, "'"
                                          , ", ", opts
                                          , "))")

      , .data$class %in% "barcode" ~ paste0("cowplot::draw_plot(barcode_qr(",  "'", value , "'", ")"
                                           , ", x =", X, ", y =", Y
                                           , ", width =", W, ", height =", H
                                           , ", halign = 0.5, valign = 0.5"
                                           , ", hjust = 0.5, vjust = 0.5"
                                           , ")")
      
      , .data$class %in% "image" ~ paste0("cowplot::draw_plot("
                                           , "grid::rasterGrob(image_import("
                                           , "'", .data$value, "'"
                                           , ", '", .data$opts, "'"
                                           , ")", ")"
                                           , ", x =", .data$X, ", y =", .data$Y
                                           , ", width =", .data$W, ", height =", .data$H
                                           , ", halign = 0.5, valign = 0.5"
                                           , ", hjust = 0.5, vjust = 0.5"
                                           , ")")
      
      , .data$class %in% "shape" ~ paste0("cowplot::draw_plot(huito::shape_"
                                           , .data$value
                                           , "(size = ", .data$size
                                           , ", border_width = ", .data$border_width
                                           , ", background = '", .data$color
                                           , "', border_color = '", .data$border_color
                                           , "', panel_color = '", .data$panel_color
                                           , "', panel_size = ", .data$panel_size
                                           , ")"
                                           , ", width = ", .data$size
                                           , ", height = ", .data$size
                                           , ", x = ", .data$X, ", y = ", .data$Y
                                           , ", halign = 0.5, valign = 0.5"
                                           , ", hjust = 0.5, vjust = 0.5"
                                           , ")")
      )) %>%
    dplyr::select(.data$nlayer, .data$nlabel, .data$layer) %>%
    dplyr::mutate(dplyr::across(c(.data$nlayer, .data$nlabel), as.numeric)) %>%
    dplyr::arrange(.data$nlayer, .data$nlabel, .by_group = T) %>% 
    dplyr::select(!.data$nlayer) %>% 
    replace(is.na(.), 0)
  
# -------------------------------------------------------------------------
  
  label_opts <- opts %>% 
    dplyr::filter(nlayer %in% 0) %>% 
    dplyr::select(.data$option, .data$value) %>% 
    tibble::deframe()
  
  label_dimension <- label_opts$size %>% 
    strsplit(split = "[*]") %>% 
    unlist() %>% 
    as.numeric()
  
# -------------------------------------------------------------------------
  
  showtext::showtext_auto(enable = TRUE)
  
  if (mode =="sample"| mode == "preview") {
    
    layers <- tolabel %>%
      dplyr::filter(.data$nlabel %in% c(0, 1)) %>%
      dplyr::select(.data$layer) %>%
      tibble::deframe() %>%
      paste0(., collapse = " + ")
    
    label_print <- eval(parse(text = paste(layers))) 
    
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
      , units = label_opts$units
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
  
  nlabels <- nrow(fb)
  
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
        
        eval(parse(text = paste(layers)))
        
      })
    
    # -------------------------------------------------------------------------
    
    grids <- seq(from = 0, to = nlabels, by = ncol*nrow) %>% 
      head(., pages)
    
    file_output <- paste0(filename, ".pdf")
    
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
          , units = label_opts$units
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