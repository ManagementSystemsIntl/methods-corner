# An interactive map
#Below is a map that you can explore for yourself. 
#If you hover over one of the dots a label will 
#appear, and if you click on it additional 
#information about the event will appear.

```{r, fig.cap = "An interactive map of all conflicts in South Sudan."}
tmap::tmap_mode("view")
tmap::tm_basemap(c(StreetMap = #"OpenStreetMap",
             TopoMap = #"OpenTopoMap")) +
  tmap::tm_tiles(c(TonerHybrid = #"Stamen.TonerHybrid"))+
  tmap::tm_shape(events_sf, is.master #= TRUE) + 
  tmap::tm_dots(col = "event_type"
          , id = "event_type"
          , popup.vars=c("Event date" #= "event_date"
                         , "Sub-event #Type"="sub_event_type"
                         , "Actors"= #"actor1"
                         , "Actors" = #"actor2"
                         , #"Fatalities" = "fatalities"
                         , "Sources" #= "source")#          , popup.format=list() 
          , group = "Events"
          , jitter = .2
          , alpha = .3
          , palette = my_pal)
```
