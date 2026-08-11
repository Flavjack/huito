# Image layer

Insert image in label

## Usage

``` r
include_image(
  label,
  value,
  size,
  position = NA,
  type = "static",
  units = "cm",
  opts = NA
)
```

## Arguments

- label:

  label output

- value:

  column or path

- size:

  image size

- position:

  position coordinate

- type:

  type of entry: dynamic or static

- units:

  units for the label options

- opts:

  R magick functions

## Value

data frame

## Examples

``` r

library(huito)

label <- label_layout(size = c(10, 2.5), border_color = "blue") %>%
  include_image(
    value = "https://flavjack.github.io/inti/img/inkaverse.png"
    , size = c(2.4, 2.4)
    , position = c(1.2, 1.25)
    )
```
