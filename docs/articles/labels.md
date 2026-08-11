# Labels

## Create fieldbook

The field-book experimental design was deployed with `inti` package
<https://inkaverse.com/articles/apps.html>

``` r

library(inti)

treats <- data.frame(condition = c("irrigated", "drought")
                     , genotypes = c("choclito", "salcedo", "pandela", "puno"))

fb <- tarpuy_design(data = treats
                    , nfactors = 2
                    , type = "rcbd"
                    , rep = 3
                    ) 

fb %>% web_table()
```

## Label design

The layer options can change dynamically based in a column. In the
following example the color of the text will change based in the
treatments: “blue” for `irrigated` and “red” for `drought`.

> You can find more fonts in <https://fonts.google.com/>

``` r

library(huito)

font <- c("Permanent Marker", "Tillana", "Courgette")

huito_fonts(font)

label <- fb %>% 
  mutate(color = case_when(
    condition %in% "irrigated" ~ "blue"
    , condition %in% "drought" ~ "red"
  )) %>% 
  label_layout(size = c(10, 2.5)
               , border_color = "blue"
               ) %>%
  include_image(
    value = "https://flavjack.github.io/inti/img/inkaverse.png"
    , size = c(2.1, 2.4)
    , position = c(1.2, 1.25)
    , opts = list("image_scale(200)", "image_noise()")
    ) %>%
  include_barcode(
     value = "barcode"
     , size = c(2.5, 2.5)
     , position = c(8.2, 1.25)
     ) %>%
  include_text(value = "Inkaverse"
               , position = c(4.6, 2)
               , size = 25
               , color = "brown"
               , font = font[1]
               , fontface = "bold"
               ) %>%
  include_text(value = "condition"
               , position = c(2.5, 1.2)
               , size = 18
               , color = "color" 
               , font = font[2]
               , opts = list(hjust = 0.0, vjust = 0.0) 
               , fontface = "bold"
               ) %>%
  include_text(value = "genotypes"
               , position = c(2.5, 0.5)
               , size = 15
               , color = "#009966"
               , font = font[3]
               , opts = list(hjust = 0.0, vjust = 0.0)
               , prefix = "Genotipo: "
               ) %>% 
  include_text(value = "plots"
               , position = c(9.7, 1.25)
               , angle = 90
               , size = 12
               , color = "red"
               , font = font[1]
               , prefix = "Plot: "
               ) 
```

You should comment
[`huito_fonts()`](http://huito.inkaverse.com/reference/huito_fonts.md)
after the first running as we already imported the fonts.

### Preview mode

The preview mode `label_print(mode = "preview")` generate a example of
the label design from a random row of the data set.

``` r

label %>% 
  label_print(mode = "preview")
```

![](labels_files/figure-html/unnamed-chunk-3-1.png)

### Complete mode

If you want generate the complete labels list, change:
`label_print(mode = "complete")`.

``` r

label %>% 
  label_print(mode = "complete", filename = "etiquetas", nlabels = 10, dpi= 1200, rasterize= T)
```
