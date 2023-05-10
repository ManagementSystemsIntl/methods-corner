



#load libraries
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(mapview)



#Load a dataset 
#This is an ACLED datset
events <- read_xlsx("SouthSudan/data/SSudan.xlsx") 

#There's one issue with the spelling on one state
events$admin1 <- events$admin1 %>% 
  recode("Western Bahr El Ghazal" = "Western Bahr el Ghazal")

#To plot this data, we need to know how it stores the geo coordinates
head(events)

#This version has a geometry column added to it
#This creates a list column of the combined longitude and latitude
events_sf <- st_as_sf(events, coords = c("longitude", "latitude"), crs = 4326)

#Now we can manipulate the events_sf object as a normal dataframe


#Quick interactive view
mapview(events_sf)

#Quick static view
plot(events_sf[,1])

#But there's no polygons for South Sudan
states <- geoboundaries(country = "South Sudan"
                        , adm_lvl = "adm1")

#As a note geoboundaries is not maintained well, but it is easy. 
#I use www.gadm.org and download geopackage files if I want to make 
#sure I use something of high quality
#What did we get?
mapview(states)


##here's a more prepared ggplot static map
ggplot(states) +
  geom_sf() +
  geom_sf(data = events_sf, aes(color = event_type)
          , size=.8,
          alpha = .6) 

+
  theme.plot()

#sampling


mapview(df1)

mapview(st_sample(events_sf, 500
                  , type = "Fibonnacci"))
mapview(st_sample(events_sf, 100
                  , type = "hexagonal"))



