source(here::here("./network_analysis_sandbox/prep/prep.R"))

#set seed
set.seed(1374)

#develop random network

df <- data.frame(name = c("Steve", "Steve", "Steve", "Cheryl", "Stephanie", "Stephanie")
                 , friend = c("Chris", "Cheryl", "Stephanie", "Chris", "Paulo", "Cheryl"))

df <- df |>
  mutate(team = case_when(name == "Steve" | name == "Chris" ~ "A", name == "Cheryl" | name == "Paulo" ~ "B", name == "Stephanie" ~ "C"))

g <- graph_from_data_frame(df, directed = TRUE)


g_plot <- ggraph(g, layout = "kk") +
  geom_edge_link(width = 1
                 , color = "grey"
                 , alpha = .7) +
  geom_node_point(color = "white"
                  , size = 16)+
  geom_node_text(aes(label = name)
                 , color = "#6639B7"
                 , size = 5.5)+ 
  theme_graph(plot_margin = unit(c(1, 0, 1, 0), "cm"))

g_plot
ggsave(plot = g_plot
       , filename  = "friend-plot.png"
       , path = "./network_analysis_sandbox/viz/"
       , height = 4
       , width = 6
       , units = "in")


# an org chart type graph
#adapted from https://r-graph-gallery.com/334-basic-dendrogram-with-ggraph.html

# create a data frame 
data <- data.frame(
  level1="CEO",
  level2=c( rep("They",4), rep("Them",4)),
  level3=c("Steve", "Brian", "Steph", "Barb", "Lindsay", "Ingrid", "Lynn", "Chris"))

# transform it to a edge list!
edges_level1_2 <- data %>% select(level1, level2) %>% unique %>% rename(from=level1, to=level2)
edges_level2_3 <- data %>% select(level2, level3) %>% unique %>% rename(from=level2, to=level3)
edge_list=rbind(edges_level1_2, edges_level2_3)

# Now we can plot that
mygraph <- graph_from_data_frame( edge_list )
ggraph(mygraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal(width = 1
                     , color = "grey"
                     , alpha = .7) +
  geom_node_point(color = "white"
                  , size = 16) +
  geom_node_text(aes(label = name)
                 , color = "#6639B7"
                 , size = 5)+
  theme_graph()


ggsave(plot = last_plot()
       , filename  = "hierarchy.png"
       , path = "./network_analysis_sandbox/viz/"
       , height = 4
       , width = 6
       , units = "in")

## Another example to include in the presentation

#load network data from files
edges_mentor <- readxl::read_xlsx(here::here("./network_analysis_sandbox/data/edges_mentor.xlsx"))
nodes_mentor <- readxl::read_xlsx(here::here("./network_analysis_sandbox/data/nodes_mentor.xlsx"))
edges_tech <- readxl::read_xlsx(here::here("./network_analysis_sandbox/data/edges_tech.xlsx"))
nodes_tech <- readxl::read_xlsx(here::here("./network_analysis_sandbox/data/nodes_tech.xlsx"))

nodes_mentor <- nodes_mentor |>
  mutate(R_user = case_when(grepl("R", software) ~ "R"
                            , TRUE ~ "Other"))

###create graph object
graph_ment <- graph_from_data_frame(edges_mentor
                                    , directed = FALSE
                                    , vertices = nodes_mentor)
graph_tech <- graph_from_data_frame(edges_tech
                                    , directed = FALSE
                                    , vertices = nodes_tech)


g_ment <- ggraph(graph_ment, layout = "with_kk") +
  geom_edge_link(color = my_pal[[5]]
                 , alpha = .3) +
  geom_node_point(aes(color = V(graph_ment)$R_user)#my_pal[[5]]
                  , size = 8) +
  geom_node_text(aes(label = name)
                 , color = "white")+
  theme.graph()
  

g_ment

ggsave(plot = g_ment
       , filename  = "mentor_R.png"
       , path = "./network_analysis_sandbox/viz/"
       , height = 5
       , width = 7
       , units = "in")

#testing out community finding algorithm
fun <- fastgreedy.community(graph_ment)

fun2 <- edge.betweenness.community(graph_ment)

fun3 <- cluster_fast_greedy(graph_ment)

png("./network_analysis_sandbox/viz/community.png", width=8, height=6, units="in", res=300)
par(mfrow = c(2, 2))
plot(fun, graph_ment, main = "Communities Option 1")
plot(fun2, graph_ment, main = "Communities Option 2")
plot(fun3, graph_ment, main = "Communities Option 3")
dev.off()


#Interactive version
# Set a vertex attribute called 'color' to 'dodgerblue' 
ment_int <- set_vertex_attr(graph_ment, "color", value = "dodgerblue")

# Redraw the graph and make the vertex size 1
graphjs(ment_int, vertex.size = 1)

# Create numerical vector of vertex eigenvector centralities 
ec <- as.numeric(eigen_centrality(graph_ment)$vector)

# Create new vector 'v' that is equal to the square-root of 'ec' multiplied by 5
v <- 5*sqrt(ec)

# Plot threejs plot of graph setting vertex size to v
graphjs(graph_ment, vertex.size = v)



