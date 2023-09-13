

#develop random network

df <- data.frame(name = c("Steve", "Steve", "Steve", "Cheryl", "Stephanie", "Stephanie")
                 , friend = c("Chris", "Cheryl", "Stephanie", "Chris", "Paulo", "Cheryl"))

df <- df |>
  mutate(team = case_when(name == "Steve" | name == "Chris" ~ "A", name == "Cheryl" | name == "Paulo" ~ "B", name == "Stephanie" ~ "C"))

g <- graph_from_data_frame(df, directed = TRUE)


g_plot <- ggraph(g, layout = "stress") +
  geom_edge_link(width = 1
                 , color = "#6639B7") +
  geom_node_point(color = "white"
                  , size = 20)+
  geom_node_text(aes(label = name)
                 , color = "#6639B7"
                 , size = 4)+ 
  theme_graph(plot_margin = unit(c(1, 0, 1, 0), "cm"))

g_plot
ggsave(plot = g_plot
       , filename  = "friend-plot.png"
       , path = "./network_analysis_sandbox/viz/"
       , height = 4
       , width = 5
       , units = "in")
