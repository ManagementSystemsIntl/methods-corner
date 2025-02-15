#An example of a spider chart

#example code from 
#https://r-graph-gallery.com/web-radar-chart-with-R.html


#libraries

#remotes::install_github("ricardo-bion/ggradar")
#install.packages("palmerpenguins")

library(ggradar)
library(palmerpenguins)
library(tidyverse)
library(scales)
library(showtext)

font_add_google("Lobster Two", "lobstertwo")
font_add_google("Roboto", "roboto")

showtext_auto()

#load the penguins data
data("penguins", package = "palmerpenguins")
head(penguins, 3)

#some basic cleaning and scaling of data
penguins_radar <- penguins %>%
  drop_na() %>%
  group_by(species) %>%
  summarise(
    avg_bill_length = mean(bill_length_mm),
    avg_bill_dept = mean(bill_depth_mm),
    avg_flipper_length = mean(flipper_length_mm),
    avg_body_mass = mean(body_mass_g)
  ) %>%
  ungroup() %>%
  mutate_at(vars(-species), rescale)


plot <- penguins_radar %>%
  ggradar::ggradar(
    font.radar = "roboto",
    grid.label.size = 13,  # Affects the grid annotations (0%, 50%, etc.)
    axis.label.size = 8.5, # Afftects the names of the variables
    group.point.size = 3)   # Simply the size of the point 

ggsave(filename = "plot1.png"
       , path = "Spider chart demo/viz")


# 1. Set the position legend to bottom-right
# 2. Bottom-right justification
# 3. Customize text size and family
# 4. Remove background and border color for the keys
# 5. Remove legend background
plot <- plot + 
  theme(
    legend.position = c(1, 0),  
    legend.justification = c(1, 0),
    legend.text = element_text(size = 28, family = "roboto"),
    legend.key = element_rect(fill = NA, color = NA),
    legend.background = element_blank()
  ) +
  labs(title = "Radar plot of penguins species") + 
  theme(
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.title.position = "plot", # slightly different from default
    plot.title = element_text(
      family = "lobstertwo", 
      size = 62,
      face = "bold", 
      color = "#2a475e"
    )
  )

ggsave(filename = "plot2.png"
       , path = "Spider chart demo/viz")


#now making it interactive

#devtools::install_github("ropensci/plotly")
library(plotly)
library(htmlwidgets)


p <- ggplotly(plot)

saveWidget(p, "viz/plotlyplot.html")

### Making my mock object

dat <- tibble(role = c("Role 1"
                       , "Role 2"
                       , "Role 3"
                       , "Role 4")) %>% 
              mutate(lfa = case_when(grepl("Role 1", role)  ~ runif(4)
                                     , grepl("Role 3", role) ~ runif(4)
                                     , TRUE ~ 0)
                     , cam = case_when(grepl("Role 2", role) ~ runif(4)
                                       , TRUE ~ 0)
                     , ic = case_when(grepl("Role 3", role) ~ runif(4)
                                      , grepl("Role 2", role) ~ runif(4)
                                      , grepl("Role 1", role) ~ runif(4)
                                      , TRUE ~ 0)
                     , cms = case_when(grepl("Role 4", role) ~ runif(4)
                                       , grepl("Role 2", role) ~ runif(4)
                                       , grepl("Role 3", role) ~ runif(4)
                                       , TRUE ~ 0))

col_list = c("lfa", "cam", "ic", "cms")

var_width = 15

#avg. scores
dat_sum <- dat %>%
  group_by(role) %>% 
  rowwise() %>% 
  mutate(score = round(mean(c_across(all_of(col_list)), na.rm = T), digits = 2)) %>% 
  ungroup() %>% 
  mutate_at(vars(-role), rescale) %>% 
  select(-score) %>% 
  rename("Land Force Application" = lfa
         , !!str_wrap("Counter Air & Missile", width=15) := cam
         , "Imagery Collection" = ic
         , !!str_wrap("Corp. Mgmt & Support", width = 15) := cms) 
  
spider <- dat_sum %>%
  ggradar::ggradar(
    font.radar = "roboto",
    grid.label.size = 13,  # Affects the grid annotations (0%, 50%, etc.)
    axis.label.size = 8.5, # Afftects the names of the variables
    group.point.size = 3)   # Simply the size of the point 

ggsave(filename = "plot_test.png"
       , width = 6
       , height = 6
       , units = "in"
       , path = "Spider chart demo/viz")


# 1. Set the position legend to bottom-right
# 2. Bottom-right justification
# 3. Customize text size and family
# 4. Remove background and border color for the keys
# 5. Remove legend background
spider <- spider + 
  theme(
    legend.position = c(1, 0),  
    legend.justification = c(1, 0),
    legend.text = element_text(size = 16, family = "roboto"),
    legend.key = element_rect(fill = NA, color = NA),
    legend.background = element_blank()
  ) +
  labs(title = "Radar plot of S2C capabilities, by Role") + 
  theme(
    plot.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    panel.background = element_rect(fill = "#fbf9f4", color = "#fbf9f4"),
    plot.title.position = "plot", # slightly different from default
    plot.title = element_text(
      family = "Roboto", 
      size = 32,
      face = "bold", 
      color = "#2a475e"
    )
  )

spider
               

p2 <- ggplotly(spider)

saveWidget(p2, "Spider chart demo/viz/plotly_spider.html")

p2
