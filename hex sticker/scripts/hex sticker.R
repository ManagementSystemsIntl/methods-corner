
library(hexSticker)

source("hex sticker/scripts/prep.r")

# test ---- 

s <- sticker(~plot(cars, cex=.5, cex.axis=.5, mgp=c(0,.3,0), xlab="", ylab=""),
             package="hexSticker", p_size=20, s_x=.8, s_y=.6, s_width=1.4, s_height=1.2,
             filename="inst/figures/baseplot.png")


s <- sticker(~plot(cars, cex=.5, cex.axis=.5, mgp=c(0,.3,0), xlab="", ylab=""),
             package="hexSticker", p_size=20, s_x=.8, s_y=.6, s_width=1.4, s_height=1.2,
             filename="baseplot.png")

s

?sticker


p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p

p <- p + theme_void() + theme_transparent()

outfile <- tempfile(fileext=".png")

sticker(p, 
        package="MSI Data Science",
        p_color="#9E0000",
        h_color="#5482AB",
        h_fill="#009CA6",
        filename="sticker test.png")



# attempts ---- 
sticker("Y:/Private/dan.killian/Report templates/MSI-TT report templates/MSI logo.jpg",
        package="MSI Data Science",
        filename="grade sticker.png")



sticker("C:/Users/dan.killian/Documents/Afghan education/viz/wave 3/household/average grade, by region sex age no Capital.png",
        package="MSI Data Science",
        filename="grade sticker 2.png")

grd <- "C:/Users/dan.killian/Documents/Afghan education/viz/wave 3/household/average grade, by region sex age no Capital.png"


# v1 ---- 

s <- sticker("hex sticker/pics/MSI viz collage 5.jpg",
             s_x=1,
             s_y=.9,
             s_width=.8,
             s_height=.5,
             package="MSI Data Science",
             p_family="sans",
             p_x=1,
             p_y=1.45,
             p_size=14,
             p_color="#009CA6",
             #h_fill="#5482AB",
             h_fill="#133469",
             h_color="#009CA6",
             url="msiworldwide.com",
             u_size=4,
             u_color="#009CA6",
             filename="hex sticker/stickers/MSI data science hex sticker v1.png")


p <- ggplot(aes(x = mpg, y = wt), data = mtcars) + geom_point()
p
p <- p + theme_void() + theme_transparent()
outfile <- tempfile(fileext=".png")
sticker(p, package="hexSticker", filename=outfile)




# v2 ---- 

s <- sticker("hex sticker/pics/MSI viz collage 6.jpg",
             s_x=1,
             s_y=.9,
             s_width=.8,
             s_height=.5,
             package="MSI Data Science",
             p_family="sans",
             p_x=1,
             p_y=1.45,
             p_size=14,
             p_color="#009CA6",
             #h_fill="#5482AB",
             h_fill="#133469",
             h_color="#009CA6",
             url="msiworldwide.com",
             u_size=4,
             u_color="#009CA6",
             filename="hex sticker/stickers/MSI data science hex sticker v2.png")


# v3 ---- 

sticker("hex sticker/pics/MSI dynamic stability.jpg",
        s_x=1,
        s_y=.9,
        s_width=.8,
        s_height=.5,
        package="MSI Data Science",
        p_family="sans",
        p_x=1,
        p_y=1.45,
        p_size=14,
        p_color="#009CA6",
        #h_fill="#5482AB",
        h_fill="#133469",
        h_color="#009CA6",
        url="msiworldwide.com",
        u_size=4,
        u_color="#009CA6",
        filename="hex sticker/stickers/MSI data science hex sticker v3.png")

# v4 ---- 

sticker("hex sticker/pics/MSI familiarity.jpg",
        s_x=1,
        s_y=.85,
        s_width=.75,
        s_height=.5,
        package="MSI Analytics",
        p_family="sans",
        p_x=1,
        p_y=1.35,
        p_size=14,
        p_color="#009CA6",
        #h_fill="#5482AB",
        h_fill="#133469",
        h_color="#009CA6",
        url="msiworldwide.com",
        u_size=4,
        u_color="#009CA6",
        filename="hex sticker/stickers/MSI data science hex sticker v4.png")

# v5 ---- 

sticker("hex sticker/pics/data-science.png",
        s_x=1,
        s_y=.8,
        s_width=.5,
        s_height=.3,
        package="MSI Data Science",
        p_family="sans",
        p_x=1,
        p_y=1.5,
        p_size=14,
        p_color="#009CA6",
        #h_fill="#5482AB",
        h_fill="#133469",
        h_color="#009CA6",
        url="msiworldwide.com",
        u_size=4,
        u_color="#5482AB",
        filename="hex sticker/stickers/MSI data science hex sticker v5.png")


# v6 ---- 

?sticker

sticker("hex sticker/pics/data-science.png",
        s_x=1,
        s_y=.8,
        s_width=.5,
        s_height=.3,
        package="MSI Data Science",
        p_family="merienda",
        #p_fontface="merienda",
        p_x=1,
        p_y=1.5,
        p_size=14,
        p_color="#009CA6",
        #h_fill="#5482AB",
        h_fill="#133469",
        h_color="#009CA6",
        url="msiworldwide.com",
        u_size=4,
        u_color="#5482AB",
        filename="hex sticker/stickers/MSI data science hex sticker v6.png")

getwd()





