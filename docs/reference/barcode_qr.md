# Barcode generator

Generate bar codes using QR codes

## Usage

``` r
barcode_qr(text, color = "black", alpha = 1, ecl = "H")
```

## Arguments

- text:

  text to convert to QR bar code

- color:

  Bar code color

- alpha:

  Intensity of the bar code color

- ecl:

  Error correction level (percentage). "L" (7), "M" (15), "Q" (25) and
  "H" (30). Defaults to "H"

## Value

plot

## Examples

``` r

library(huito)

barcode_qr("LIMA-2021-11-03_15_3_4")


```
