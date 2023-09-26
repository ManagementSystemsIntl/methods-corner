source(here::here("./network_analysis_sandbox/prep/prep.R"))

#set seed
set.seed(1374)

#develop random network

df <- data.frame(name = c("Steve", "Steve", "Steve", "Cheryl", "Stephanie", "Stephanie")
                 , friend = c("Chris", "Cheryl", "Stephanie", "Chris", "Paulo", "Cheryl"))

df_nodes <- data.frame(name = c("Steve"
                                , "Cheryl"
                                , "Stephanie"
                                , "Chris"
                                , "Paulo")) 
df_nodes <- df_nodes |>
  mutate(team = case_when(name == "Steve" | name == "Chris" ~ "A", name == "Cheryl" | name == "Stephanie" ~ "B", name == "Paulo" ~ "C"))

g <- graph_from_data_frame(df
                           , directed = TRUE
                           , vertices = df_nodes)


g_plot <- ggraph(g, layout = "kk") +
  geom_edge_diagonal(width = 1
                 , color = "grey"
                 , alpha = .7) +
  geom_node_point(aes(color = team)
                  , size = 16)+
  geom_node_text(aes(label = name)
                 #, color = "#6639B7"
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
  geom_edge_link(color = my_pal[[2]]
                 , alpha = .3) +
  geom_node_point(aes(color = V(graph_ment)$R_user)
                  , size = 8) +
  geom_node_text(aes(label = name)
                 , color = "white")+
  labs(title = "SEA Data Science Team Mentoring Network"
       , subtitle = "Highlighting R users")+
  theme.graph()+
  theme(plot.subtitle = element_text(color = "#CC6576")
        , legend.position = "none")
  


g_ment

ggsave(plot = g_ment
       , filename  = "mentor_R.png"
       , path = "./network_analysis_sandbox/viz/"
       , height = 5
       , width = 7
       , units = "in")

#Ego graph from person 1
#make the ego graph
ego <- make_ego_graph(graph_ment
                      , diameter(graph_ment)
                      , nodes = 1
                      , mode = c("all"))[[1]]

#create a vector of distances from person 1
dists <- distances(ego, "1")

#add color to the graph object
V(ego)$color <- my_pal[dists+1]

#Ego graph from person 6
#make the ego graph
ego6 <- make_ego_graph(graph_ment
                      , diameter(graph_ment)
                      , nodes = 6
                      , mode = c("all"))[[1]]

#create a vector of distances from person 6
dists1 <- distances(ego, "6")

#add color to the graph object
V(ego6)$color <- my_pal[dists1+1]


# Visualize the network based on geodesic distance from vertex 184 (patient zero).
png("./network_analysis_sandbox/viz/distances.png"
    , width=8, height=6, units="in", res=300)
par(mfrow = c(1,2)
    , mar = (c(1,1,1,1)))
plot(ego, 
     vertex.label = dists, 
     vertex.label.color = "white",
     vertex.label.cex = 1.5,
     edge.color = 'black',
     vertex.size = 16,
     main = "Geodesic Distances from Person 1"
)
plot(ego6, 
     vertex.label = dists1, 
     vertex.label.color = "white",
     vertex.label.cex = 1.5,
     edge.color = 'black',
     vertex.size = 16,
     main = "Geodesic Distances from Person 6"
)
dev.off()

##Shortest-path example
ment.path <- shortest_paths(graph_ment, 
                            from = V(graph_ment)[name=="17"], 
                            to  = V(graph_ment)[name=="15"],
                            output = "both") # both path nodes and edges

# Generate edge color variable to plot the path:
ecol <- rep("gray80", ecount(graph_ment))
ecol[unlist(ment.path$epath)] <- my_pal[[7]]

# Generate edge width variable to plot the path:
ew <- rep(2, ecount(graph_ment))
ew[unlist(ment.path$epath)] <- 4

# Generate node color variable to plot the path:
vcol <- rep("gray", vcount(graph_ment))
vcol[unlist(ment.path$vpath)] <- "red"


png("./network_analysis_sandbox/viz/shortest.png"
    , width=8, height=6, units="in", res=300)
short <- plot(graph_ment, vertex.color=vcol
     , edge.color=ecol, 
     edge.width=ew, edge.arrow.mode=0
     , main = "Shortest path between nodes")
dev.off()



#identify the largest cliques in the mentor graph
largest_cliques(graph_ment)

# Determine all maximal cliques in the network and assign to object 'clq'
clq <- max_cliques(graph_ment)

# Calculate the size of each maximal clique.
table(unlist(lapply(clq, length)))

# Assign largest cliques output to object 'lc'
lc <- largest_cliques(graph_ment)

# Create two new undirected subgraphs, each containing only the vertices of each largest clique.
gs1 <- as.undirected(subgraph(graph_ment, lc[[1]]))
gs2 <- as.undirected(subgraph(graph_ment, lc[[2]]))
gs3 <- as.undirected(subgraph(graph_ment, lc[[3]]))

# Plot the two largest cliques side-by-side

par(mfrow=c(1,3)) # To plot two plots side-by-side

plot(gs1,
     vertex.label.color = "black", 
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Largest Clique 1",
     layout = layout.circle(gs1)
)

plot(gs2,
     vertex.label.color = "black", 
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Largest Clique 2",
     layout = layout.circle(gs2)
)
plot(gs3,
     vertex.label.color = "black", 
     vertex.label.cex = 0.9,
     vertex.size = 0,
     edge.color = 'gray28',
     main = "Largest Clique 3",
     layout = layout.circle(gs1)
)
dev.off()

#testing out community finding algorithm
fun <- fastgreedy.community(graph_ment)

fun2 <- edge.betweenness.community(graph_ment)

fun3 <- cluster_fast_greedy(graph_ment)

fun4 <- infomap.community(graph_ment)

#plot all the community network charts together
png("./network_analysis_sandbox/viz/community.png"
    , width=8, height=6, units="in", res=300)
par(mfrow = c(2, 2)
    , mar = c(1, 1, 1, 1))
plot(fun, graph_ment, main = "Communities Option 1")
plot(fun2, graph_ment, main = "Communities Option 2")
plot(fun3, graph_ment, main = "Communities Option 3")
plot(fun4, graph_ment, main = "Communities Option 4")
dev.off()

## Create an object 'i' containing the memberships of the fast-greedy community detection
i <-  membership(fun)

# Check the number of different communities
sizes(fun)

# Add a color attribute to each vertex, setting the vertex color based on community membership
g_test <- set_vertex_attr(graph_ment, "color", value = my_pal[1:4][i])

# Plot the graph using threejs
graphjs(g_test
        , vertex.label = "name"
        )

i <- membership(fun)

g_ment <- ggraph(graph_ment, layout = "with_kk") +
  geom_edge_link(color = my_pal[[5]]
                 , alpha = .3) +
  geom_node_point(
                   size = 8) +
  geom_node_text(aes(label = name)
                 , color = "white"
                 , size = 6)+
  theme.graph()

g_ment


#Interactive version
# Set a vertex attribute called 'color' to 'dodgerblue' 
#ment_int <- set_vertex_attr(graph_ment, "color", value = "dodgerblue")

# Redraw the graph and make the vertex size 1
#graphjs(ment_int, vertex.size = 1)

# Create numerical vector of vertex eigenvector centralities 
#ec <- as.numeric(eigen_centrality(graph_ment)$vector)

# Create new vector 'v' that is equal to the square-root of 'ec' multiplied by 5
#v <- 5*sqrt(ec)

# Plot threejs plot of graph setting vertex size to v
#graphjs(graph_ment, vertex.size = v)



### Example code
# From previous steps
#static_network <- ggplot(
 # ggnetwork(graph_ment, arrow.gap = 0.01), 
  #aes()) + 
  #geom_edges() + 
  #geom_nodes() + 
  #theme_blank() 


# Print the interactive network
#girafe(code = print(interactive_network)) %>%
  # Set girafe options
 # girafe_options(
    # Set hover options
  #  opts_hover(css = "cursor: pointer; fill: red; stroke: red; r: 5pt"),
    # Set tooltip options; give x-offset of 10 
  #  opts_tooltip(offx = 10)
  #)

# Make a D3 object


#add communities to the object
comm <- membership(fastgreedy.community(graph_ment))

V(graph_ment)$comm <- comm
V(graph_ment)$betw <- betweenness(graph_ment)



V(graph_ment)$group <- comm
nd3 <- igraph_to_networkD3(graph_ment, V(graph_ment)$group)



#Render your D3.js network
d3 <- forceNetwork(
#  Define links from nd3 object
  Links = nd3$links, 
  # Define nodes from nd3 object
  Nodes = nd3$nodes, 
  # Specify the source column
  Source = "source", 
  # Specify the target column
  Target = "target", 
  NodeID = "name",
  Group = "group",  
  legend = FALSE, 
  fontSize = 20,
  opacity = .8
  
)

saveNetwork(d3,
  "./network_analysis_sandbox/viz/d3-object.html")


#Calculate some network statistics - density, mean distance, betweeness, transitivity, and eigenvector centrality
#calculate the network density
density_ment <- edge_density(graph_ment)
#calculate the mean_distance
avg_dist_ment <- mean_distance(graph_ment, directed = FALSE)
#calculate the betweeness
b1 <- igraph::betweenness(graph_ment, directed = FALSE)
between_ment <- mean(b1)
#calculate the transitivity of the network
trans_ment <- transitivity(graph_ment)
#eigenvalues vector
e1 <- eigen_centrality(graph_ment, directed =FALSE)$vector
#calculate avg. eigenvalue of the network
eigen_ment <- mean(e1)
l <- c("Network", "Avg. Density", "Avg. Distance", "Avg. Transitivity", "Avg. Betweenness", "Avg. Eigenvector")
m <- c("Mentorship", density_ment, avg_dist_ment, trans_ment, between_ment, eigen_ment)
dat <- data.frame(l, m)
dat1 <- pivot_wider(data = dat
                    , names_from = l
                    , values_from = m) |>
  mutate(across(2:6, as.numeric))
#graph statistics table
network_table <- flextable(dat1) |>
  set_header_labels(values = dat1) |> 
  align(align = "center", part = "all") |>
  colformat_double(digits = 2)
#network_table

#Calculate some network statistics - density, mean distance, betweeness, transitivity, and eigenvector centrality
#calculate the network density
density_tech <- edge_density(graph_tech)

#calculate the mean_distance
avg_dist_tech <- mean_distance(graph_tech, directed = FALSE)

#calculate the betweeness
b2 <- igraph::betweenness(graph_tech, directed = FALSE)

between_tech <- mean(b2)
#calculate the transitivity of the network
trans_tech <- transitivity(graph_tech)

#eigenvalues vector
e1_tech <- eigen_centrality(graph_tech, directed =FALSE)$vector

#calculate avg. eigenvalue of the network
eigen_tech <- mean(e1_tech)

names <- c("Network", "Avg. Density", "Avg. Distance", "Avg. Transitivity", "Avg. Betweenness", "Avg. Eigenvector")
values <- c("Tech guidance", density_tech, avg_dist_tech, trans_tech, between_tech, eigen_tech)

dat_tech <- data.frame(names, values)

dat1_tech <- pivot_wider(data = dat_tech
                         , names_from = names
                         , values_from = values) |>
  mutate(across(2:6, as.numeric))

dat2 <- bind_rows(dat1, dat1_tech)

network_table1 <- flextable(dat2) |>
  set_header_labels(values = names(dat2)) |>
  align(align = "center", part = "all") |>
  colformat_double(digits = 2)

#network_table1

#simulation of mentorship network 1000x
gl <- vector('list', 1000)

for(i in 1:1000){
  gl[[i]] <- erdos.renyi.game(
    n = gorder(graph_ment)
    , p.or.m = edge_density(graph_ment)
    , type = "gnp"
  )
}

#Then use the gl object to calculate averages for each of the stats presented above on the original two networks.

gl_ment_avg_density <- mean(unlist(lapply(gl, edge_density)))
gl_ment_avg_dist <- mean(unlist(lapply(gl, mean_distance, directed = FALSE)))
gl_ment_avg_between <- mean(unlist(lapply(gl, betweenness)))
gl_ment_avg_trans <- mean(unlist(lapply(gl, transitivity)))

#This one takes a few more steps 
gl_eigen_ment <- do.call(rbind, lapply(gl, eigen_centrality, directed = FALSE)) |>
  #do.call(rbind, .data) |>
  data.frame() 

gl_ment_avg_eigen <- mean(unlist(gl_eigen_ment$vector))


####simulation of tech guidance network 1000x

#this simulates 1000 iterations of a graph with the same number of nodes and edge_density
#set up a for loop 
gl_tech <- vector('list', 1000)

for(i in 1:1000){
  gl_tech[[i]] <- erdos.renyi.game(
    n = gorder(graph_tech)
    , p.or.m = edge_density(graph_tech)
    , type = "gnp"
  )
}

#Then use the gl_tech object to calculate averages for each of the stats presented above on the original two networks.

gl_tech_avg_density <- mean(unlist(lapply(gl_tech, edge_density)))
gl_tech_avg_dist <- mean(unlist(lapply(gl_tech, mean_distance, directed = FALSE)))
gl_tech_avg_between <- mean(unlist(lapply(gl_tech, betweenness)))
gl_tech_avg_trans <- mean(unlist(lapply(gl_tech, transitivity)))

#This one takes a few more steps 
gl_eigen_tech <- do.call(rbind, lapply(gl_tech, eigen_centrality, directed = FALSE)) |>
  #do.call(rbind, .data) |>
  data.frame() 

gl_tech_avg_eigen <- mean(unlist(gl_eigen_tech$vector))

#make a flextable
names <- c("Network", "Avg. Density", "Avg. Distance", "Avg. Transitivity", "Avg. Betweenness", "Avg. Eigenvector")
dat_ment <- c("Mentorship", density_ment, avg_dist_ment, trans_ment, between_ment, eigen_ment)
sim_ment <- c("Simulated Mentorship", gl_ment_avg_density, gl_ment_avg_dist, gl_ment_avg_trans, gl_ment_avg_between, gl_ment_avg_eigen)
dat_tech <- c("Tech Guidance", density_tech, avg_dist_tech, trans_tech, between_tech, eigen_tech)
sim_tech <- c("Simulated Tech Guidance", gl_tech_avg_density, gl_tech_avg_dist, gl_tech_avg_trans, gl_tech_avg_between, gl_tech_avg_eigen)


dat_all <- data.frame(names, dat_ment, sim_ment, dat_tech, sim_tech)

dat_all1 <- t(dat_all) |>
  data.frame()

names(dat_all1) <- dat_all1[1,]

dat_all2 <- dat_all1[2:5,] |>
  mutate(across(2:6, as.numeric))
#, Type = c("Mentorship", "Mentorship", "Technical", "Technical") 
#)

network_table2 <- flextable(dat_all2) |>
  set_header_labels(values = names(dat_all2)) |>
  align(align = "center", part = "all") |>
  colformat_double(digits = 2) |>
  italic(i = c(2,4)) |>
  bold(i = c(1,3))

network_table2

