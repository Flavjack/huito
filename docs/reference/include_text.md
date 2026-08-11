# Text layer

Insert text in label

## Usage

``` r
include_text(
  label,
  value,
  prefix = "",
  position = NA,
  fontface = "plain",
  size = 11,
  font = NA,
  type = "static",
  color = NA,
  angle = 0,
  opts = NA
)
```

## Arguments

- label:

  label output

- value:

  column or string

- prefix:

  Prefix for the dynamic text

- position:

  position coordinate

- fontface:

  font type `[character: plain, bold, italic]`

- size:

  text size

- font:

  font type

- type:

  type of entry `[character: dynamic or static]`

- color:

  image color

- angle:

  angle of the text

- opts:

  list arguments from draw_label()

## Value

data frame
