# vertical

## Create the experimental field book

The field book experimental design was deployed with `inti` package
<https://inkaverse.com/articles/apps.html>

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`inti`](https://inkaverse.com/)`)`\
\
`treats`` ``<-`` `[`data.frame`](https://rdrr.io/r/base/data.frame.html)`(``condition ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"irrigated"``, ``"drought"``)`\
`                     , genotypes ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"choclito"``, ``"salcedo"``, ``"pandela"``, ``"puno"``)``)`\
\
`fb`` ``<-`` `[`tarpuy_design`](https://inkaverse.com/reference/tarpuy_design.html)`(``data ``=`` ``treats`\
`                    , nfactors ``=`` ``2`\
`                    , type ``=`` ``"rcbd"`\
`                    , rep ``=`` ``3`\
`                    , project ``=`` ``"inkaverse"`\
`                    ``)`` `\
\
`fb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `[`web_table`](https://inkaverse.com/reference/web_table.html)`(``)`

## Customize the label layout

The label layout can be customized by combining text, images and QR
codes. Each layer can use values from the experimental field book,
allowing automatic generation of labels for every experimental plot.

Load package and import fonts.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`huito`](https://huito.inkaverse.com/)`)`\
\
`font`` ``<-`` `[`c`](https://rdrr.io/r/base/c.html)`(``"Permanent Marker"``, ``"Tillana"``, ``"Courgette"``)`\
\
[`huito_fonts`](http://huito.inkaverse.com/reference/huito_fonts.md)`(``font``)`

> You can find more fonts in <https://fonts.google.com/>

## Label design

\
`label`` ``<-`` ``fb`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`label_layout`](http://huito.inkaverse.com/reference/label_layout.md)`(`\
`    size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``5.2``, ``10``)`\
`    ,`\
`    border_color ``=`` ``"#5C0000"`\
`    ,`\
`    border_width ``=`` ``1.5`\
`  ``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_image`](http://huito.inkaverse.com/reference/include_image.md)`(`\
`    value ``=`` ``"https://inkaverse.com/img/inkaverse.png"`\
`    ,`\
`    size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``1.3``, ``1.5``)`\
`    ,`\
`    position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.8``, ``9.1``)`\
`  ``)``  `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_text`](http://huito.inkaverse.com/reference/include_text.md)`(`\
`    value ``=`` ``"plots"`\
`    ,`\
`    position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``4.2``, ``9.1``)`\
`    ,`\
`    size ``=`` ``20`\
`    ,`\
`    color ``=`` ``"black"`\
`    ,`\
`    fontface ``=`` ``"bold"`\
`    ,`\
`    font ``=`` ``font``[``1``]`\
`  ``)``  `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_image`](http://huito.inkaverse.com/reference/include_image.md)`(``value ``=`` ``"https://huito.inkaverse.com/img/scale.pdf"`\
`                ,`\
`                size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``1``)`\
`                ,`\
`                position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.6``, ``7.7``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_barcode`](http://huito.inkaverse.com/reference/include_barcode.md)`(``value ``=`` ``"qrcode"`\
`                  ,`\
`                  size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``5``)`\
`                  ,`\
`                  position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.6``, ``4.7``)``)`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_text`](http://huito.inkaverse.com/reference/include_text.md)`(`\
`    value ``=`` ``"condition"`\
`    ,`\
`    position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.6``, ``2``)`\
`    ,`\
`    size ``=`` ``12`\
`    ,`\
`    prefix ``=`` ``"Condition: "`\
`    ,`\
`    font ``=`` ``font``[``3``]`\
`  ``)``  `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_text`](http://huito.inkaverse.com/reference/include_text.md)`(`\
`    value ``=`` ``"genotypes"`\
`    ,`\
`    position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.6``, ``1.5``)`\
`    ,`\
`    size ``=`` ``12`\
`    ,`\
`    prefix ``=`` ``"Genotypes: "`\
`    ,`\
`    font ``=`` ``font``[``2``]`\
`  ``)``  `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)\
`  `[`include_image`](http://huito.inkaverse.com/reference/include_image.md)`(``value ``=`` ``"https://huito.inkaverse.com/img/scale.pdf"`\
`                ,`\
`                size ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``5``, ``1``)`\
`                ,`\
`                position ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``2.6``, ``0.6``)``)`` `

### Label preview

The preview mode `label_print(mode = "preview")` generate a example of
the label design from a random row of the data set.

\
`label`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`label_print`](http://huito.inkaverse.com/reference/label_print.md)`(``mode ``=`` ``"preview"``)`

![](vertical_files/figure-html/unnamed-chunk-4-1.png)

### Generate the complete labels

If you want generate the complete labels list, change:
`label_print(mode = "complete")`.

\
`label`` `[`%>%`](https://magrittr.tidyverse.org/reference/pipe.html)` `\
`  `[`label_print`](http://huito.inkaverse.com/reference/label_print.md)`(``mode ``=`` ``"complete"`\
`              , filename ``=`` ``"vertical"`\
`              , nlabels ``=`` ``12``)`
