# Network activity script

#read in the prep script and libraries
source(here::here("./network_analysis_sandbox/prep/prep.R"))

#read in the dataframe
df <- readxl::read_xlsx("./network_analysis_sandbox/data/networking activity data.xlsx")

nodes <- readxl::read_xlsx("./network_analysis_sandbox/data/networking activity data.xlsx"
                                                      , sheet = 2)


#make it a graph object
g <- graph_from_data_frame(df
                           , directed = FALSE)

V(g)$strength <- strength(g)
V(g)$between <- betweenness(g)
V(g)$degree <- degree(g)


#make a plot
gplot <- ggraph(g, layout = "with_kk") +
  geom_edge_link(color = my_pal[[2]]
                 , alpha = .3) +
  geom_node_point(size = 12
                  ,color = "#003478"
                  , alpha = 1) +
  geom_node_text(aes(label = name)
                 , color = "white"
                 , size = 2)+
  labs(title = "MSI's Network of Networking Day"
       , subtitle = "September 22, 2023")+
  theme.graph()+
  theme(legend.position = "none")

gplot

ggsave(gplot
       , filename  = "networkday.png"
       , path = "./network_analysis_sandbox/viz/"
       , height = 5
       , width = 7
       , units = "in")


gplot2 <- ggraph(g, layout = "with_kk") +
  geom_edge_link(color = my_pal[[2]]
                 , alpha = .3) +
  geom_node_point(aes(size = V(g)$between)
                  ,color = "#003478"
                  , alpha = .6) +
  geom_node_text(aes(label = name)
                 , color = "black"
                 , size = 2)+
  labs(title = "MSI's Network of Networking Day"
       , subtitle = "September 22, 2023")+
  theme.graph()+
  theme(legend.position = "none")

gplot2

ggsave(gplot2
       , filename  = "networkday2.png"
       , path = "./network_analysis_sandbox/viz/"
       , height = 5
       , width = 7
       , units = "in")

#communities

g_fun <- edge.betweenness.community(g)

#plot all the community network charts together
png("./network_analysis_sandbox/viz/networkdaycomm.png"
    , width=8, height=6, units="in", res=300)
plot(g, g_fun, main = "Communities within Networking Day")


#add communities to the the member vector
member <- membership(edge.betweenness.community(g))

#add member to the graph object g
V(g)$comm <- member

#add betweenness scores to the graph object g
V(g)$betw <- betweenness(g)


#V(g)$group <- comm #use this if the communities don't join
#on the first try
net3 <- igraph_to_networkD3(g, V(g)$comm)



#Render your D3.js network
interactive <- forceNetwork(
  #  Define links from nd3 object
  Links = net3$links, 
  # Define nodes from nd3 object
  Nodes = net3$nodes, 
  # Specify the source column
  Source = "source", 
  # Specify the target column
  Target = "target", 
  NodeID = "name",
  Group = "group",  
  legend = FALSE, 
  fontSize = 20,
  opacity = 1
  
)

interactive
