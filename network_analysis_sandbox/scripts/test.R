
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

###text work

library(tidytext)
library(tidyverse)

text <- as_tibble_col(df$software)

softwares <- paste(c("R", "Stata", "SPSS", "Nvivo", "Qualtrics", "MaxQDA"
              , "Python", "ArcGIS", "Excel", "SAS", "EpiInfo", "STATA"
              , "Kobo Tool Box", "Dedoose", "SMath Studio", "Tableau"), collapse = "|")

text <- text |>
  mutate(new = str_extract_all(text$value, pattern = softwares))


    