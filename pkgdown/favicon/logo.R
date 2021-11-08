# -------------------------------------------------------------------------
# hutio ---------------------------------------------------------------
# -------------------------------------------------------------------------

library(huito)
library(tidyverse)

# open https://fonts.google.com/
# font poma

# -------------------------------------------------------------------------

label <- label_layout(size = c(4.4196, 5.08)
                      , border_color = "white"
                      , border_width = 0
                      ) %>% 
  include_image(value = "pkgdown/favicon/img/patron1.png"
                , size = c(7, 7)
                , position = c(-1.29, -1)
                ) %>%
  include_text(value = "H", size = 45 , position = c(0.82, 2.7), color = "#00a85a") %>%
  include_text(value = "u", size = 45 , position = c(1.75, 2.7), color = "#f58735") %>%
  include_text(value = "i", size = 45 , position = c(2.38, 2.7), color = "#4774b8") %>%
  include_image(value = "pkgdown/favicon/img/huito_fruit.png"
                , size = c(1.3, 1.3) , position = c(3.06, 1.94)) %>%
  include_text(value = "t", size = 45 , position = c(2.95, 2.7), color = "#a9518b") %>%
  include_shape(size = c(4.4196, 5.08)
                , border_width = 3
                , border_color = "black"
                ) %>%
  include_text(value = "inkaverse.com"
               , size = 6
               , position = c(3.3147, 0.8)
               , angle = 30
               , color = "white") %>%
  label_print(fonts = F
              , filename = "huito"
              , margin = 0
              , paper = c(6, 6)
              , viewer = T
              , mode = "s"
              )

# transparent -------------------------------------------------------------
# -------------------------------------------------------------------------

logo <- list.files("pkgdown/favicon/img"
                   , full.names = T
                   , pattern = "huito.png") %>%
  image_read()  %>% 
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

