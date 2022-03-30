library(huito)
source("http://inkaverse.com/setup.r")

# import scale ------------------------------------------------------------

scale <- image_read_pdf("pkgdown/favicon/img/scale.pdf", density = 600) %>% 
  # image_rotate(-90) %>% 
  image_write("pkgdown/favicon/img/scale.png")

# phenotyping-blue --------------------------------------------------------
# -------------------------------------------------------------------------

label <- label_layout(size = c(100, 100)
                      , border_color = "black"
                      , border_width = 10
                      , background = "#0099FF"
                      ) 

label %>% 
  label_print(mode = "c"
              , paper = c(110, 110)
              , filename = "pkgdown/favicon/img/phenotyping_blue")

# phenotyping-bw ----------------------------------------------------------
# -------------------------------------------------------------------------

label <- label_layout(size = c(100, 100)
                      , border_color = "black"
                      , border_width = 10
                      , background = NA
                      ) 

label %>% 
  label_print(mode = "c"
              , paper = c(110, 110)
              , filename = "pkgdown/favicon/img/phenotyping_bw")

# scales ------------------------------------------------------------------
# -------------------------------------------------------------------------

label <- label_layout(size = c(12, 6)
                      , border_color = NA
                      , border_width = 0
                      ) %>% 
  include_image(value = scale
                , size = c(11, 5)
                , position = c(6, 3)
                ) 

label %>% 
  label_print(mode = "c"
              , paper = c(100, 40)
              , filename = "pkgdown/favicon/img/scale-print"
              , nlabels = 48
              )

# labels ------------------------------------------------------------------
# -------------------------------------------------------------------------

url <- "https://docs.google.com/spreadsheets/d/1ywk2jhCKRKy_9-8pH9LPjBN8jmfMr_6ofBB7NY0WY5Y/edit#gid=204727071"

gs <- url %>% 
  as_sheets_id()

fb <- gs %>% 
  range_read("phenomquinoa")


# huito_fonts("Rubik")

label <- fb %>% 
  label_layout(size = c(10, 2.5)
               , border_color = "blue"
  ) %>%
  include_image(
    value = "man/figures/logo.png"
    , size = c(2.2, 2.2)
    , position = c(1.2, 1.25)
  ) %>%
  include_barcode(
    value = "barcode"
    , size = c(2.5, 2.5)
    , position = c(8.2, 1.25)
  ) %>%
  include_text(value = "id"
               , position = c(9.7, 1.25)
               , angle = 90
               , size = 15
               , color = "red"
               , font = "Rubik"
  ) %>%
  include_text(value = "PhenomQuinoa"
               , position = c(4.6, 1.8)
               , size = 18
               , color = "brown"
               , font = 'Rubik'
  ) %>%
  include_text(value = "barcode"
               , position = c(4.6, 0.8)
               , size = 13
               , color = "black"
               , font = 'Rubik'
  ) 


label %>% 
  label_print(mode = "c"
              , filename = "pkgdown/favicon/img/labels"
              , paper = c(100, 100)
              )




