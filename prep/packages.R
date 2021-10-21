packages <- c("acled.api", "av", "colorspace",  "cowplot", "DT", "extrafont", "extrafontdb", "haven", "janitor", "lubridate"
              , "gganimate", "ggdist", "ggforce", "gghalves", "gghighlight", "ggrepel", "ggridges", "ggthemes", "gt", "gtsummary", "htmltools"
              , "htmlwidgets", "knitr", "leaflet", "leaflet.extras", "leaflet.extras2", "plotly", "raster", "rasterVis", "rcartocolor", "readxl", "readr"
              , "rgeoboundaries", "rnaturalearth", "rnaturalearthdata", "rworldmap", "sf", "mapsf",  "spatstat", "sysfonts", "systemfonts", "terra", "tidyr", "tidyverse", "viridis", "writexl")


lapply(packages, library, character.only=T)


#My font
font_add_google(name = "Open Sans"
                , family = "Open Sans")

## custom colors
my_pal <- rcartocolor::carto_pal(name = "Safe")


  