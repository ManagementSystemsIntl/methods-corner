packages <- c("acled.api", "av", "colorspace",  "cowplot", "DT", "extrafont", "extrafontdb", "haven", "janitor", "lubridate"
              , "gganimate", "ggdist", "ggforce", "gghalves", "gghighlight", "ggrepel", "ggridges", "ggthemes", "gt", "gtsummary", "htmltools"
              , "htmlwidgets", "knitr", "leaflet", "leaflet.extras", "leaflet.extras2", "plotly", "raster", "rasterVis", "rcartocolor", "readxl", "readr"
              , "rgeoboundaries", "rnaturalearth", "rnaturalearthdata", "rworldmap", "sf", "mapsf",  "spatstat", "sysfonts", "systemfonts", "terra", "tidyr", "tidyverse", "viridis", "writexl")


lapply(packages, library, character.only=T)


#My font
#font_add_google(name = "Open Sans"
               # , family = "Open Sans")
showtext_auto()
## custom colors
my_pal <- rcartocolor::carto_pal(name = "Safe")

carto_pal(12, "Safe")

#theme for ggplot2 objects
theme.plot <- function(){
  require(extrafont)
  require(extrafontdb)
  require(ggplot2)
  list(  
    ggplot2::theme(legend.position = "top"
                   , legend.background = ggplot2::element_blank()
                   , legend.title = ggplot2::element_blank()
                   , legend.key = ggplot2::element_blank() 
                   , axis.ticks = ggplot2::element_blank()
                   , axis.line = ggplot2::element_blank()
                   , panel.grid.minor = ggplot2::element_blank()
                   , panel.grid.major.y = ggplot2::element_line(color = "#CFCDC9") 
                   , panel.grid.major.x = ggplot2::element_blank()
                   , panel.background = ggplot2::element_blank()
                   , plot.title.position = "plot" #Pushes the title to the very left of the plot window
                   , plot.title = element_text(size = 26, family = "Corbel", color = "#000000")
                   , plot.subtitle = element_text(size = 18, family = "Corbel", color = "#CFCDC9")
                   , strip.background = ggplot2::element_rect(fill = "white") 
                   , axis.text = ggplot2::element_text(size = 12, family = "Corbel", hjust = 0, color = "#000000")
                   , plot.caption = ggplot2::element_text(size = 8, family = "Corbel", color = "#000000"))
    #The colors below are the cartocolor "Safe" palette plus 3 additional colors pulled from cartocolor.
    , ggplot2::scale_fill_manual(values = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288",
                                            "#AA4499", "#44AA99", "#999933", "#882255", "#661100",
                                            "#6699CC", "#888888", "#764E9F", "#ED645A", "#edd9a3"))
    , ggplot2::scale_color_manual(values = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288",
                                             "#AA4499", "#44AA99", "#999933", "#882255", "#661100",
                                             "#6699CC", "#888888", "#764E9F", "#ED645A", "#edd9a3"))
  )}
