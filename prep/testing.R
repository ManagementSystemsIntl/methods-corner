
#My type vector
type <- unique(dat$event_type)
type

#My function for filtering by type
type_fun <- function(x){
  dat %>% 
  filter(event_type == {{x}}) %>% 
  group_by(country, event_type) %>% 
  summarize(Count = n())
}

#Test the function on one variable
battle <- type_fun("Battles")

#Now run it for real
z <-map(type, ~type_fun(.x))

z

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


test_p <- plot_fun(z[[1]])

test_p


test_plot <- map(z, ~plot_fun(.x))

test_plot[[1]]


#This works
cowplot::plot_grid(plotlist = test_plot)


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
