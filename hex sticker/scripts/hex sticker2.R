
library(tidyverse)
library(magick)
library(hexSticker)
library(here)
library(showtext)

## use the ggplot2 example


#read in image
img <- image_read(here::here("./hex sticker/pics/image.png"))

img_text <- image_scale(img, "x200")
img_text <- image_background(img_text, "#003478", flatten = TRUE)

img_scale <- image_scale(img, "300")

img_scale

img_crop <- image_crop(img_scale, "175x300+75+10")

img_crop

img_crop <- image_background(img_crop
                             , "#003478")

## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Gochi Hand", "gochi")
## Automatically use showtext to render text for future devices
showtext_auto()

new <- sticker(img_crop, package = ""
               , p_size=20
               , p_color = "#C84E00"
               , s_x=1
               , s_y=1.02
               , s_width=1.3
               , s_height = 1.3
               , p_family = "gochi"
               , h_fill = "#003478"
               , h_color = "#C84E00"
               , h_size = 2#"#007C92"
               , filename="./hex sticker/stickers/new_logo.png")

new
