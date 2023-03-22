# Dan Killian
# networks explore

library(igraph)

getwd()

te <- read_excel("network analysis sandbox/data/2-15/staff survey networks.xlsx", 
                sheet="technical-edges") %>%
  right_join(v)

  filter(te_nms$name %in% v$name==T)
te

te_nms$name %in% v$name==T

te_nms <- data.frame(name=c(te$from, te$to)) %>%
  distinct

te_nms

?distinct

te_nms$name %in% v$name

v <- read_excel("network analysis sandbox/data/2-15/staff survey networks.xlsx",
                sheet="key") %>%
  filter(tech==1)
,
         practice=="SEA")

v

ids <- te %>%
  select(1,3) %>%
  set_names(nm=c("from","to")) %>%
  filter()

ids

d <- graph_from_data_frame(d=ids,
                           vertices=v,
                           directed=T)

nms <- te %>%
  select(2,4)

nms

idg <- graph.edgelist(as.matrix(ids),
                     directed=T)

idg

plot(idg)

V(tech)

E(tech)

gorder(tech)
gsize(tech)
