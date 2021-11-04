source("https://inkaverse.com/setup.r")
library(huito)

url <- "https://docs.google.com/spreadsheets/d/1rhIK4kqgp3UZFOAUGuSKQ45W7MpY98IxpQZSRb67LUY/edit#gid=334632135"
gs <- as_sheets_id(url)
# browseURL(url)

fb <- gs %>% 
  range_read("fb") %>% 
  select(1:tratamientos) %>% 
  mutate(bloque = paste("bloque:\n", block)) %>% 
  mutate(treat = paste("Tratamiento:\n", tratamientos)) 

label <- fb %>% 
  label_layout(size = c(10, 2.5)
               , border_color = "red"
               # , border_width = 0.5
               ) %>% 
  include_barcode(value = "barcode"
                  , size = c(2.5, 2.5)
                  , position = c(7, 0)
                  ) %>%
  include_text(value = "plots"
               , position = c(9.7, 1.25)
               , angle = 90
               , size = 14
               , color = "red"
               ) %>%
  include_image(
    value = "https://upload.wikimedia.org/wikipedia/commons/a/a9/Unalm_logo.png"
    , size = c(2, 2)
    , position = c(1, 0.25)
  ) %>%
  include_text(value = "treat"
               , position = c(5, 1.9)
               , size = 12
               , color = "blue"
               ) %>%
  include_text(value = "bloque"
               , position = c(5, 0.9)
               , size = 12
               ) %>%
  include_text(value = "HECTOR FRANCISCO MELGAR CHIA"
               , position = c(5, 0.2)
               , size = 6
               , color = "#339966"
               ) %>% 
  label_print("c"
              , fonts = F
              # , margin = 0.03
              )

# -------------------------------------------------------------------------

library(huito)
library(gsheet)

url <- paste0("https://docs.google.com/spreadsheets/d/"
       , "1q0EZmZBt52ca-0VbididjJy2jXTwf06laJpzvkQJWvc/edit#gid=107939497")
fb <- gsheet2tbl(url)
# browseURL(url)

label <- fb %>% 
  mutate(temp = paste0("Temperatura: ", temperatura)) %>% 
  mutate(var = paste0("Variedad: ", variedad)) %>% 
  label_layout(size = c(10, 2.5)
               , border_color = "blue"
               ) %>%
  include_image(
    value = "https://flavjack.github.io/inti/img/inkaverse.png"
    , size = c(2.4, 2.4)
    , position = c(0, 0.05)
    ) %>%
  include_barcode(
     value = "qr-code"
     , size = c(2.5, 2.5)
     , position = c(6.9, 0)
     ) %>%
  include_text(value = "plots"
               , position = c(9.7, 1.25)
               , angle = 90
               , size = 15
               , color = "red"
               ) %>%
  include_text(value = "Inkaverse"
               , position = c(4.5, 2.1)
               , size = 18
               , color = "brown"
               ) %>%
  include_text(value = "temp"
               , position = c(4.5, 1.2)
               , size = 12
               , color = "orange"
               ) %>%
  include_text(value = "var"
               , position = c(4.5, 0.5)
               , size = 12
               , color = "#009966"
               ) %>%
  label_print("c", fonts = F)





