# Shape hexagon

Hexagon geom shape for ggplot2

## Usage

``` r
shape_hexagon(
  size = 5.08,
  border_width = NA,
  background = NA,
  border_color = "black",
  units = "cm",
  panel_color = "green",
  panel_size = NA
)
```

## Arguments

- size:

  hexagon size (numeric: 5.08)

- border_width:

  line width (numeric: 1)

- background:

  background color (string: "transparent")

- border_color:

  border color (string: "black")

- units:

  units for shape (string: "cm")

- panel_color:

  panel color (string: "green")

- panel_size:

  panel size (numeric: NA)

## Value

geom

## Examples

``` r

library(huito)

shape_hexagon(border_width = 1
              , background = "red"
              #, panel_size = 5.08
              )

```
