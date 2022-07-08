# -------------------------------------------------------------------------
# hutio ---------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

font <- "Permanent Marker"

huito_fonts(font)

label <- label_layout(size = c(5.08, 5.08)
                      , border_width = 0
                      ) %>% 
  include_image(value = "https://flavjack.github.io/huito/img/shipibo.png"
                , size = c(7, 7)
                , position = c(2.55, 2.52)
                # , opts = "image_scale(600)"
                ) %>%
  include_text(value = "H", size = 45 , position = c(1.15, 2.7), color = "#00a85a", font) %>%
  include_text(value = "u", size = 45 , position = c(2.07, 2.7), color = "#f58735", font) %>%
  include_text(value = "i", size = 45 , position = c(2.73, 2.7), color = "#4774b8", font) %>%
  include_image(value = "https://flavjack.github.io/huito/img/huito_fruit.png"
                , size = c(1.32, 1.32) 
                , position = c(4.04, 2.65)
                ) %>%
  include_text(value = "t", size = 45 , position = c(3.32, 2.7), color = "#a9518b", font) %>%
  include_shape(size = 5.08
                , border_width = 3
                , border_color = "black"
                , position = c(2.54, 2.54)
                , panel_color = "blue"
                ) %>%
  include_text(value = "inkaverse.com"
               , size = 6
               , position = c(3.6, 0.75)
               , angle = 30
               , color = "white"
               , font
               ) 

label %>% 
  label_print(mode = "preview")

logo <- label %>% 
  label_print(filename = tempfile()
              , margin = 0
              , paper = c(5.5, 5.5)
              , mode = "complete"
              )

# transparent -------------------------------------------------------------
# -------------------------------------------------------------------------

stiker <- logo %>% 
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent("blue") %>%
  image_write("pkgdown/favicon/img/huito.png")

file.copy("pkgdown/favicon/img/huito.png"
          , "man/figures/logo.png"
          , overwrite = T)

# -------------------------------------------------------------------------
# Flavio Lozano Isla ------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

font <- c("Fredericka the Great", "Permanent Marker")

huito_fonts(font)

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      , background = "white"
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/shipibo.png"
                , size = c(7, 7)
                , position = c(2.55, 2.52)
                , opts = 'image_transparent("white")'
                ) %>%
  include_shape(size = 5.08
                , border_width = 2.2
                , border_color = "black"
                , position = c(2.54, 2.54)
                , panel_color = "blue" 
                ) %>%
  include_shape(size = 4
                , border_width = 0.1
                , border_color = "black"
                , position = c(2.54, 2.54)
                , panel_color = NA
                , background = "white"
                ) %>%
  include_text(value = "Flavio"
               , position = "2.65*3.4"
               , size = 27
               , font = font[1]
               ) %>% 
  include_text(value = "Lozano"
               , position = "2.55*2.55"
               , size = 27
               , font = font[1]
               ) %>% 
  include_text(value = "Isla"
               , position = "2.6*1.65"
               , size = 27
               , font = font[1]
               ) %>%
  include_text(value = "lozanoisla.com"
               , size = 5
               , position = c(3.6, 0.72)
               , angle = 30
               , font = font[2]
               , color = "white") 

label %>% label_print()
  
logo <- label %>% 
  label_print(filename = tempfile()
              , margin = 0
              , paper = c(5.5, 5.5)
              , mode = "complete"
              )

# transparent -------------------------------------------------------------
# -------------------------------------------------------------------------

sticker <- logo %>% 
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent("blue") %>%  
  image_write("pkgdown/favicon/img/flozano.png")

