



df <- readxl::read_excel("misc/FY22 and FY23 Ukraine Projects 7-17-2023 consolidated.xlsx",
                         sheet="Combined Funding")

df <- df %>%
  select(activity=2,cn=4,sector=6, amount=12) %>%
  arrange(cn, sector)

act <- df %>%
  select(activity) %>%
  mutate(act_num=0:185)


sec <- df %>%
  select(sector) %>%
  distinct %>%
  na.omit() %>%
  mutate(sec_num=186:197)

sec

cn <- df %>%
  select(cn) %>%
  distinct() %>%
  na.omit() %>%
  mutate(cn_num=198:199)

cn

df2 <- df %>%
  left_join(act) %>%
  left_join(sec) %>%
  left_join(cn) %>%
  na.omit()












df1_2 <- df2 %>% select(activity, cn, amount) %>% unique %>% rename(from=activity, to=cn)

ot <- df2 %>%
  group_by(cn, sector) %>%
  summarise(amount=sum(amount))

df2_3 <- df2 %>% select(cn, sector) %>% unique %>% rename(from=cn, to=sector) %>%
  mutate(amount=ot$amount)


dfedge <- rbind(df1_2, df2_3) %>%
  as.data.frame()

dfnodes <- data.frame(name=c(as.character(dfedge$from),
                             as.character(dfedge$to)) %>% unique())

dfedge <- dfedge %>%
  mutate(IDsource=match(from, dfnodes$name) - 1,
         IDtarget=match(to, dfnodes$name)-1)

library(networkD3)

ds <- sankeyNetwork(Links = dfedge, 
                    Nodes = dfnodes,
                    Source = "IDsource", 
                    Target = "IDtarget",
                    Value = "amount", 
                    NodeID = "name",
                    sinksRight=FALSE,
                    units="$",
                    fontSize=14)
ds

?sankeyNetwork

library(htmlwidgets)
saveWidget(ds, file="misc/sankey.html")
?saveWidget




library(webshot)

#install phantom:
webshot::install_phantomjs()
# Make a webshot in pdf : high quality but can not choose printed zone
webshot("paste_your_html_here.html" , "output.pdf", delay = 0.2)

# Make a webshot in png : Low quality - but you can choose shape
webshot("paste_your_html_here" , "output.png", delay = 0.2 , cliprect = c(440, 0, 1000, 10))







set.seed(1)
data <- matrix(sample( seq(0,40), 49, replace=T ), 7, 7)
data[data < 35] <- 0
colnames(data) = rownames(data) = c("group_A", "group_B", "group_C", "group_D", "group_E", "group_F", "group_G")

data

# Transform it to connection data frame with tidyr from the tidyverse:
links <- data %>% 
  as.data.frame() %>% 
  rownames_to_column(var="source") %>% 
  gather(key="target", value="value", -1) %>%
  filter(value != 0)

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(links$source), as.character(links$target)) %>% 
    unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

# Make the Network
p <- sankeyNetwork(Links = links, Nodes = nodes,
                   Source = "IDsource", Target = "IDtarget",
                   Value = "value", NodeID = "name", 
                   sinksRight=FALSE)

p



















dfgraph <- graph_from_data_frame(dfedge)

plot(dfgraph)

ggraph(dfgraph, layout = 'dendrogram', circular = FALSE) + 
  geom_edge_diagonal() +
  geom_node_point(color="#69b3a2", size=3) +
  theme_void() +
  coord_flip() +
  scale_y_reverse() 

