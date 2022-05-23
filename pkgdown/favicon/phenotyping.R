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
                      ) %>% 
  include_image(value = "https://inkaverse.com/img/inkaverse.png"
                , size = c(10, 10)
                , position = c(5, 94)
                ) %>% 
  include_image(value = "man/figures/logo.png"
                , size = c(10, 10)
                , position = c(95, 94)
                )

label %>% label_print()

label %>% 
  label_print(mode = "c"
              , paper = c(110, 110)
              , filename = "pkgdown/favicon/img/phenobg")

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

label %>% label_print()

label %>% label_print(mode = "c"
                      , paper = c(100, 50)
                      , margin = 0
                      , filename = "pkgdown/favicon/img/scales2print"
                      , nlabels = 8*8
                      ) 

# labels ------------------------------------------------------------------
# -------------------------------------------------------------------------

url <- "https://docs.google.com/spreadsheets/d/1jcXXUzoMsXEMeccfs2AeQLDvC_3_B9QjzeFvnx6AT3Y/edit#gid=1266812467"

gs <- url %>% 
  as_sheets_id()

fb <- gs %>% 
  range_read("fb") 

font <- "Rubik"

huito_fonts(font)

label <- fb %>% 
  label_layout(size = c(12, 5)
               , border_color = "red"
               , border_width = 2
               ) %>%
  include_barcode(
    value = "barcode"
    , size = c(4.5, 4.5)
    , position = c(9, 2.5)
    ) %>% 
  include_text(value = "id"
               , position = c(11.5, 2.5)
               , angle = 90
               , size = 20
               , font[1]
               ) %>% 
  include_image(
    value = "man/figures/logo.png"
    , size = c(3, 3)
    , position = c(4, 2.3)
    ) %>% 
  include_text(value = "PhenomQuinoa"
               , position = c(4, 4.5)
               , size = 20
               , color = "brown"
               , font[1]
               ) %>% 
  include_text(value = "barcode"
               , position = c(0.5, 2.5)
               , angle = 90
               , size = 15
               , font[1]
               ) %>% 
  include_text(value = "12x5 cm"
               , position = c(4, 0.3)
               , size = 10
               , color = "brown"
               , font[1]
               )

label %>% label_print()
  
label %>% 
  label_print(mode = "c"
              , filename = "pkgdown/favicon/labels"
              , paper = c(100, 100)
              )

