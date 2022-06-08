library(tidyverse)
library(huito)

fb <- readxl::read_excel("vignettes/test/phenomquinoa.xlsx")

pictures <- "vignettes/test/" %>%  
  list.files(full.names = T, recursive = T, pattern = ".jpg") %>% 
  enframe(name = "id", value = "path") %>% 
  mutate(img = basename(path)) %>% 
  mutate(info = image_read(path) %>% image_info()) %>% 
  unnest(cols = c(info))

fbdt <- fb %>% 
  pivot_longer(!c(id:`panicle-shape`), names_to = "bg", values_to = "img") %>% 
  mutate(bg = gsub("-background", "", bg)) %>% 
  mutate(shape = case_when(
    `panicle-shape` %in% c("1", "2") ~ "glomerulate"
    , `panicle-shape` %in% "3" ~ "amaranth"
  ))

dtfb <- merge(fbdt, pictures, by = "img") %>% 
  mutate(rotate = case_when(
    width > height ~ "image_scale(300)*image_rotate(90)"
    , width < height ~ "image_scale(300)"
  )) %>% 
  mutate(bgc = case_when(
    shape %in% "glomerulate" ~ "red"
    , shape %in% "amaranth" ~ "green"
  ))
  
label <- dtfb %>% 
  label_layout(size = "10*10"
               , background = "bgc"
               ) %>% 
  include_image(value = "path"
                , size = c(8, 8)
                , position = c(5, 5)
                , opts = "rotate"
                ) %>% 
  include_text(value = "shape", position = c(5, 0.5), size = 16) %>% 
  include_text(value = "img", position = c(5, 9.5)) %>% 
  include_text(value = "id.y", position = c(1, 5), size = 18)

label %>% label_print()

# label %>% label_print(filename = "vignettes/test/quinoa", mode = "c")


