# Label layout

Generate labels options

## Usage

``` r
label_layout(
  data = NA,
  size,
  border_width = NA,
  border_color = "black",
  background = NA,
  units = "cm"
)
```

## Arguments

- data:

  data frame to build the labels

- size:

  label size (numeric: c(10, 2.5))

- border_width:

  border width (numeric: 0.5)

- border_color:

  border color (string: "transparent")

- background:

  background color (string: "transparent")

- units:

  units for the label options (string: "cm")

## Value

data frame

## Examples

``` r

library(huito)

label <- label_layout(size = c(10, 2.5)
                   , border_color = "red"
                   , border_width = 1
                   ) 
                   
label %>% label_print()

```
