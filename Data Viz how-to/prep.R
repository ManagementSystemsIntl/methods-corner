packages <- c("DT", "extrafont", "extrafontdb", "flextable", "lubridate"
              , "ggforce", "gghighlight", "ggtext", "knitr", "rcartocolor"
              , "readxl", "tidyverse", "sf", "showtext", "terra", "tmap")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
lapply(packages, library, character.only = TRUE) |>
  invisible()

## custom colors
my_pal <- carto_pal(12, "Bold") 


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
                   , plot.subtitle = element_text(size = 18, family = "Corbel", color = "#A9A9A9")
                   , strip.background = ggplot2::element_rect(fill = "white") 
                   , axis.text = ggplot2::element_text(size = 14, family = "Corbel", hjust = 0, color = "#000000")
                   , plot.caption = ggplot2::element_text(size = 10, family = "Corbel", color = "#000000"))
    #The colors below are from the cartocolor "Safe" palette plus 3 additional colors pulled from cartocolor.
    , ggplot2::scale_fill_manual(values = c("#7F3C8D", "#11A579", "#3969AC", "#F2B701", "#E73F74", "#80BA5A", "#E68310",
                                            "#008695", "#CF1C90", "#f97b72", "#4b4b8f", "#A5AA99"))
    , ggplot2::scale_color_manual(values = c("#7F3C8D", "#11A579", "#3969AC", "#F2B701", "#E73F74", "#80BA5A", "#E68310",
                                             "#008695", "#CF1C90", "#f97b72", "#4b4b8f", "#A5AA99"))
  )}



#create dates to add to the dataframe

# Define the initial date
initial_date <- as.Date("2021-01-01")

# Define the number of repetitions for each date
repetitions_per_date <- 5

# Define the total number of rows in your dataframe
total_rows <- 50  # Adjust as needed

# Calculate the total number of dates needed
num_dates <- ceiling(total_rows / repetitions_per_date)

# Generate a sequence of dates
dates <- seq(initial_date, by = "quarter", length.out = num_dates)




