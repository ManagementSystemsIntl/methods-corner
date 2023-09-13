

#develop random network

df <- data.frame(name = c("Steve", "Steve", "Steve", "Cheryl", "Stephanie", "Stephanie")
                 , friend = c("Chris", "Cheryl", "Stephanie", "Chris", "Paulo", "Cheryl"))

df <- df |>
  mutate(team = case_when(name == "Steve" | name == "Chris" ~ "A", name == "Cheryl" | name == "Paulo" ~ "B", name == "Stephanie" ~ "C"))

g <- graph_from_data_frame(df, directed = TRUE)


g_plot <- ggraph(g, layout = "kk") +
  geom_edge_link(size = 4
                 , color = "#6639B7") +
  geom_node_point(color = "white"
                  , size = 35)+
  geom_node_text(aes(label = name)
                 , color = "#6639B7"
                 , size = 8)+ 
  
  theme_void()

ggsave(plot = g_plot
       , filename  = "friend-plot.png"
       , path = "./network_analysis_sandbox/viz/"
       , height = 4
       , width = 6
       , units = "in")