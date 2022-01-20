# -------------------------------------------------------------------------
# hutio ---------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

#> https://github.com/terinjokes/StickersStandard

huito_fonts()

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/shipibo.png"
                , size = c(7, 7)
                , position = c(2.55, 2.52)
                ) %>%
  include_text(value = "H", size = 45 , position = c(1.15, 2.7), color = "#00a85a") %>%
  include_text(value = "u", size = 45 , position = c(2.07, 2.7), color = "#f58735") %>%
  include_text(value = "i", size = 45 , position = c(2.73, 2.7), color = "#4774b8") %>%
  include_image(value = "pkgdown/favicon/img/huito_fruit.png"
                , size = c(1.35, 1.35) 
                , position = c(4.03, 2.65)
                ) %>%
  include_text(value = "t", size = 45 , position = c(3.33, 2.7), color = "#a9518b") %>%
  include_shape(size = 4.1
                , border_width = 3
                , border_color = "black"
                , margin = -0.8
                , position = c(2.54, 2.54)
                , panel_color = "blue"
                , panel_size = 5.08
                ) %>%
  include_text(value = "inkaverse.com"
               , size = 6
               , position = c(3.6, 0.75)
               , angle = 30
               , color = "white") %>%
  label_print(filename = "pkgdown/favicon/img/huito"
              , margin = 0
              , paper = c(5.5, 5.5)
              , viewer = T
              , smpres = 300
              , mode = "s"
              )

# transparent -------------------------------------------------------------
# -------------------------------------------------------------------------

label %>% 
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent("blue") %>% 
  image_write("man/figures/logo.png")


# -------------------------------------------------------------------------
# Flavio Lozano Isla ------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

font <- "Fredericka the Great"

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
  include_shape(size = 4.1
                , border_width = 2.5
                , border_color = "black"
                , margin = -0.8
                , position = c(2.54, 2.54)
                , panel_size = 5.08
                , panel_color = "blue" 
                ) %>%
  include_shape(size = 3.7
                , border_width = 0.1
                , border_color = "black"
                , margin = -0.8
                , position = c(2.54, 2.54)
                , panel_size = 5.08
                , panel_color = NA
                , background = "white"
                ) %>%
  include_text(value = "Flavio"
               , position = "2.65*3.4"
               , size = 27
               , font = font
               ) %>% 
  include_text(value = "Lozano"
               , position = "2.55*2.55"
               , size = 27
               , font = font
               ) %>% 
  include_text(value = "Isla"
               , position = "2.6*1.65"
               , size = 27
               , font = font
               ) %>%
  include_text(value = "lozanoisla.com"
               , size = 5
               , position = c(3.6, 0.75)
               , angle = 30
               , font = font, color = "white") %>%
  label_print(filename = "pkgdown/favicon/img/flozano"
              , margin = 0
              , paper = c(5.5, 5.5)
              , viewer = T
              , smpres = 300
              , mode = "c"
              )

# transparent -------------------------------------------------------------
# -------------------------------------------------------------------------

label %>% 
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent("blue") %>%  
  image_write("pkgdown/favicon/img/flozano.png")



# -------------------------------------------------------------------------
# Sandy -------------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

font <- "Fredericka the Great"

huito_fonts(font)

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      , background = "pink"
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/shipibo.png"
                , size = c(7, 7)
                , position = c(2.55, 2.52)
                , opts = 'image_transparent("white")'
                ) %>%
  include_shape(size = 4.1
                , border_width = 2.5
                , border_color = "black"
                , margin = -0.8
                , position = c(2.54, 2.54)
                , panel_size = 5.08
                , panel_color = "red"
                ) %>%
  include_shape(size = 3.7
                , border_width = 0.1
                , border_color = "black"
                , margin = -0.8
                , position = c(2.54, 2.54)
                , panel_size = 5.08
                , panel_color = "transparent"
                , background = "white"
                ) %>%
  include_text(value = "Sandy"
               , position = "2.55*3.4"
               , size = 20
               , font = font
               ) %>% 
  include_text(value = "Ordoñez"
               , position = "2.55*2.65"
               , size = 19
               , font = font
               ) %>% 
  include_text(value = "Del Aguila"
               , position = "2.6*1.9"
               , size = 19
               , font = font
               ) %>%
  label_print(filename = "pkgdown/favicon/img/sandy"
              , margin = 0
              , paper = c(5.5, 5.5)
              , viewer = T
              , smpres = 300
              , mode = "s"
              )

# transparent -------------------------------------------------------------
# -------------------------------------------------------------------------

label %>% 
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent("red") %>% 
  image_write("pkgdown/favicon/img/sandy.png")


# -------------------------------------------------------------------------
# Omar Estefano -----------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)

font <- "Fredericka the Great"

huito_fonts(font)

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
) %>% 
  include_image(value = "pkgdown/favicon/img/shipibo.png"
                , size = c(7, 7)
                , position = c(2.55, 2.52)
  ) %>%
  include_shape(size = 4.1
                , border_width = 2.5
                , border_color = "black"
                , margin = -0.8
                , position = c(2.54, 2.54)
                , panel_color = "#abc621"
                , panel_size = 5.08
  ) %>%
  include_shape(size = 3.7
                , border_width = 0.1
                , border_color = "black"
                , margin = -0.8
                , position = c(2.54, 2.54)
                , panel_size = 5.08
                , panel_color = NA
                , background = "white"
  ) %>%
  include_text(value = "Omar"
               , position = "2.55*3.1"
               , size = 28
               , font = font
  ) %>% 
  include_text(value = "Estefano"
               , position = "2.55*2"
               , size = 22
               , font = font
  ) %>% 
  label_print(filename = "pkgdown/favicon/img/omar"
              , margin = 0
              , paper = c(5.5, 5.5)
              , viewer = T
              , smpres = 300
              , mode = "c"
              )

# transparent -------------------------------------------------------------
# -------------------------------------------------------------------------

label %>% 
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>% 
  image_crop(geometry = "560x600-40") %>% 
  image_transparent("#abc621") %>% 
  image_write("pkgdown/favicon/img/omar.png")
