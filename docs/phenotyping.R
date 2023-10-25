source("http://inkaverse.com/setup.r")

# phenotyping-blue --------------------------------------------------------
# -------------------------------------------------------------------------

label <- label_layout(size = c(50, 30)
                      , border_color = "black"
                      , border_width = 5
                      , background = "#0099FF"
                      ) %>% 
  include_image(value = "https://inkaverse.com/img/inkaverse.png"
                , size = c(2.5, 2.5)
                , position = c(2, 28)
                ) %>% 
  include_image(value = "https://inkaverse.com/img/inkaverse.png"
                , size = c(2.5, 2.5)
                , position = c(48, 28)
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/logo.png"
                , size = c(2.5, 2.5)
                , position = c(2, 2)
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/logo.png"
                , size = c(2.5, 2.5)
                , position = c(48, 2)
                ) %>% 
  #> top
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(25, 28)
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(12.5, 28)
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(37.5, 28)
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(18.75, 28)
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(31.25, 28)
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(6.25, 28)
                ) %>%  
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(43.75, 28)
                ) %>%  
  #> bottom
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(25, 2)
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(12.5, 2)
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(37.5, 2)
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(18.75, 2)
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(31.25, 2)
                ) %>%  
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(6.25, 2)
                ) %>%  
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(5, 1)
                , position = c(43.75, 2)
                ) %>%  
  #> left 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(1, 5)
                , position = c(2, 6*1)
                , opts = "image_rotate(-90)"
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(1, 5)
                , position = c(2, 6*2)
                , opts = "image_rotate(-90)"
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(1, 5)
                , position = c(2, 6*3)
                , opts = "image_rotate(-90)"
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(1, 5)
                , position = c(2, 6*4)
                , opts = "image_rotate(-90)"
                ) %>% 
  # right
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(1, 5)
                , position = c(48, 6*1)
                , opts = "image_rotate(-90)"
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(1, 5)
                , position = c(48, 6*2)
                , opts = "image_rotate(-90)"
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(1, 5)
                , position = c(48, 6*3)
                , opts = "image_rotate(-90)"
                ) %>% 
  include_image(value = "https://huito.inkaverse.com/img/scale.pdf"
                , size = c(1, 5)
                , position = c(48, 6*4)
                , opts = "image_rotate(-90)"
                )

label %>% label_print()

label %>% 
  label_print(mode = "c"
              , paper = c(55, 35)
              , filename = "pkgdown/favicon/img/phenoboard")


