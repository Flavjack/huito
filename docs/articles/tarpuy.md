# Tarpuy

More information from Tarpuy project:
<https://inkaverse.shinyapps.io/tarpuy/>

## Sticker design

In the layer `include_image(opts = "magick package")` you can add
different arguments and combine them using asterisk (`*`) or
`list("magick function")`.

Options available in [magick
package](https://CRAN.R-project.org/package=magick)

> Select different panel color for your sticker
> (i.e. `include_shape(panel_color = "color")`).

``` r

library(huito)
font <- c("Permanent Marker")

huito_fonts(font)

label <- label_layout(size = c(5.08, 5.08)
                      , border_color = NA
                      , border_width = 0
                      , background = "#ffe701"
                      ) %>% 
  include_image(value = "logo_tarpuy.jpg" 
                , size = c(3.95, 3.95)
                , position = c(2.57, 2.21)
                ) %>%
  include_shape(size = 5.08
                , border_width = 3
                , border_color = "#505456"
                , position = c(2.54, 2.54)
                , panel_color = "blue"
                ) %>%
  include_text(value = "inkaverse.com"
               , size = 6
               , position = c(3.6, 0.75)
               , angle = 30
               , color = "white"
               , font = font[1]
               ) 
```

### Preview mode

``` r

label %>% 
  label_print(mode = "preview")
```

![](tarpuy_files/figure-html/unnamed-chunk-2-1.png)

### Complete mode

The final file is exported in `pdf` format.

``` r

sticker <- label %>%
  label_print(filename = "tarpuy"
              , margin = 0
              , paper = c(5.5, 5.5)
              , mode = "complete"
              )
```

## Transparent logo

Import the image in pdf format and cut the border and make the
`panel_color` transparent.

``` r

sticker %>% 
  image_read_pdf()  %>% 
  image_crop(geometry = "600x600+40") %>%
  image_crop(geometry = "560x600-40") %>%
  image_transparent('blue') %>% 
  image_write("tarpuy.png")
```

### Final sticker result

![](tarpuy.png)
