
#My type vector
type <- unique(dat$event_type)


#My function for filtering by type
type_fun <- function(x){
  dat %>% 
  filter(event_type == {{x}}) %>% 
  group_by(country, event_type) %>% 
  summarize(Count = n())
}

#Now run it for real
z <-map(type, ~type_fun(.x))

#Plot the object and organize it by Count. The facet_wrap function generates six charts, one for each of the event types
plot_fun <- function(x) {
ggplot(x
       , aes(Count, reorder(country, Count)
             , color = country
             , fill = country)) +
  geom_point(size = 5
             , alpha = .3) +
  geom_text(aes(label = Count)
            , color = "black") +
  theme.plot() +
  theme(legend.position = "none"
        , axis.text.x = element_blank())+
  ggtitle(x[[2]]) +
  labs(x = ""
       , y = "")
 
}

six_plot <- map(z, ~plot_fun(.x))


plots_all <- cowplot::plot_grid(plotlist = six_plot
                   , nrow = 3
                   , nol = 2)

plots_all


ggtitle("Types of Conflict Events by Country"
        , subtitle = "Some explanatory text") +
labs(caption = "Author: Brian Calhoon\n Source: ACLED, www.acleddata.com") 


ggplot(z[[1]]
       , aes(Count, reorder(country, Count)
             , color = country
             , fill = country)) +
  geom_point(size = 5
             , alpha = .3) +
  geom_text(aes(label = Count)
            , color = "black") +
  theme.plot() +
  theme(legend.position = "none"
        , axis.text.x = element_blank())
