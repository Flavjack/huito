#> https://stackoverflow.com/questions/41261785/clip-cut-everything-outside-of-polygon-or-fill-the-outside-with-white
#> https://stackoverflow.com/questions/23888037/subtracting-two-columns-to-give-a-new-column-in-r

hexd <- data.frame(x = 1 + c(rep(-sqrt(3) / 2, 2), 0
                             , rep(sqrt(3) / 2, 2)
                             , 0), y = 1 + c(0.5, -0.5, -1, -0.5, 0.5, 1))

hexd <- rbind(hexd, hexd[1, ])

rim  <- tibble(x = c(0, 0, 2, 2, 0)
               , y = c(0, 2, 2, 0, 0))

datapolyM <- rbind(rim, hexd) %>% as.data.frame()
names(datapolyM) <- c("x","y")
datapolyM$id <- "a"

ggplot(datapolyM, aes(x=x, y=y)) + 
  geom_polygon(fill="red")


