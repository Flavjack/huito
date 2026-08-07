# Changelog

## huito 0.2.7

- Added `dpi` and `rasterize` arguments to
  [`label_print()`](http://huito.inkaverse.com/reference/label_print.md)
  for improved export quality.
- Updated graphical layer functions to allow independent control of
  width and height through `size`.
- Added a new vignette for field labels.
- Updated label design documentation.

## huito 0.2.6

CRAN release: 2025-10-18

- Update to new ggplot2 version
- Include image options by list
- Update vignettes
- [`include_text()`](http://huito.inkaverse.com/reference/include_text.md)
  updated to use `prefix` and
  `[fontface - character: plain, bold, italic]`

## huito 0.2.5

CRAN release: 2024-09-05

- CRAN checks

## huito 0.2.4

CRAN release: 2023-10-25

- Avoid quality lost when rotate image from pdf

## huito 0.2.3

CRAN release: 2023-07-04

- Include image from pdf

## huito 0.2.2

CRAN release: 2023-01-24

- Update vignettes: labels

## huito 0.2.1

CRAN release: 2022-08-11

- Change package title
- logo update
- Connect to Zenodo
- Vignettes updates
- Fix CRAN comments

## huito 0.2.0

CRAN release: 2022-06-24

- Update
  [`include_shape()`](http://huito.inkaverse.com/reference/include_shape.md)
  - real size hexagon in stickers
- Update vignettes
- Update
  [`label_print()`](http://huito.inkaverse.com/reference/label_print.md)
  - Argument `width = 0` avoid borders
  - “Dynamic” arguments base in columns

## huito 0.1.4

CRAN release: 2022-05-26

- Improve code in
  [`label_print()`](http://huito.inkaverse.com/reference/label_print.md)
- Print one page if the label length is lees than 1 page

## huito 0.1.3

CRAN release: 2022-04-01

- Update vignettes
- Not load any font by default (i.e Permanent Marker)
- [`label_layout()`](http://huito.inkaverse.com/reference/label_layout.md)
  use tibble as default
- Include a dataset: `fieldbook`
- Update examples in functions
- Avoid [`tempdir()`](https://rdrr.io/r/base/tempfile.html) when use
  [`label_print()`](http://huito.inkaverse.com/reference/label_print.md)

## huito 0.1.2

CRAN release: 2022-01-20

- Fix background argument in
  [`shape_hexagon()`](http://huito.inkaverse.com/reference/shape_hexagon.md)
- Close text device after the output in
  [`label_print()`](http://huito.inkaverse.com/reference/label_print.md)

## huito 0.1.1

CRAN release: 2021-12-14

- Update pkgdown
- Update vignettes.
- Include example from GerminaR sticker
- Match argument for `label_print(mode)` to “preview” and “sample”.
- Set the number of label to print: `label_print(nlabels)`

## huito 0.1.0

CRAN release: 2021-12-01

- Added a `NEWS.md` file to track changes to the package.
- First release of huito :)
