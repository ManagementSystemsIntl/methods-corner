
library(tidygraph)

g <- as_tbl_graph(mentor_edges) |>
  activate(nodes) |>
  mutate(degree = centrality_degree()) |>
  activate(edges) |>
  mutate(centrality = centrality_edge_betweenness()) |>
  arrange(centrality)


activate( nodes) |>
  local_size(g)

E(g)

ecount(g)

E(g)$weight
ggraph(g, layout = "with_kk") +
  geom_node_point(aes(size = strength)
                  , color = "red")+
  geom_node_text(aes(label = name)) +
  geom_edge_link(aes(alpha = centrality))

centrality(g)

degree(g)

strength(g)

