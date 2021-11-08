# -------------------------------------------------------------------------
# hutio ---------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)
library(tidyverse)

# huito_fonts()

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/patron1.png"
                , size = c(7, 7)
                , position = c(2.54, 2.54)
                ) %>%
  include_text(value = "H", size = 45 , position = c(1.15, 2.7), color = "#00a85a") %>%
  include_text(value = "u", size = 45 , position = c(2.07, 2.7), color = "#f58735") %>%
  include_text(value = "i", size = 45 , position = c(2.73, 2.7), color = "#4774b8") %>%
  include_image(value = "pkgdown/favicon/img/huito_fruit.png"
                , size = c(1.3, 1.3) 
                , position = c(4.06, 2.6)
                ) %>%
  include_text(value = "t", size = 45 , position = c(3.33, 2.7), color = "#a9518b") %>%
  include_shape(size = c(4.4196, 5.08)
                , border_width = 3
                , border_color = "black"
                , margin = -0.11
                , position = c(2.54, 2.54)
                ) %>%
  include_text(value = "inkaverse.com"
               , size = 6
               , position = c(3.6, 0.75)
               , angle = 30
               , color = "white") %>%
  label_print(filename = "pkgdown/favicon/img/huito"
              , margin = 0
              , paper = c(5.08, 5.08)
              , viewer = T
              , mode = "c"
              )

# transparent -------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "huito.pdf") %>%
  image_read_pdf()  %>% 
  image_transparent('white') %>% 
  image_write("pkgdown/favicon/img/huito.png")

# fruit -------------------------------------------------------------------

imgs <- list.files("pkgdown/favicon/img"
                   , full.names = T
) %>% 
  enframe(name = "img", value = "path") %>% 
  select(path)

# imgs %>% 
#   filter(str_detect(path, "fruit")) %>% 
#   unlist() %>% 
#   image_read()  %>% 
#   image_transparent('white') %>% 
#   image_write("pkgdown/favicon/img/huito_fruit.png")

