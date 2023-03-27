
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


###Testing out additional attributes
df_test <- df 

edges_mentor_test <- df_test |>
  mutate(to_mentor2 = strsplit(to_mentor, "[,;&]+")) |>
  unnest("to_mentor2") #|>
#select(ID, Name, to_mentor2)

#had to separately clean the " and " for some reason
edges_mentor_test <- edges_mentor_test |>
  mutate(to_mentor3 = strsplit(to_mentor2, " and ")) |>
  unnest("to_mentor3")

#need to clean up names in to_mentor column
#This allows us to view all unique rows in the column
unique(edges_mentor_test$to_mentor3)

#this fixes the names of the to_mentor3 column
edges_mentor_test$to_mentor3 <- edges_mentor_test$to_mentor3 |>
  str_trim() |> #eliminates extra spaces before and after text
  recode("Tim R" = "Tim Reilly"
         , "Tim R." = "Tim Reilly"
         , "Michelle" = "Michelle Adams-Matson"
         , "Just started here" = "skip"
         , "Dave Hinkle" = "David Hinkle"
         , "past staff" = "skip"
         , "None" = "skip"
         , "NA" = "skip"
         , "N/A" = "skip"
         , "Kazeem"  = "Kazeem Adebayo"
         , "Tom" = "Tom Muga Ogejo"
         , "Tom Muga (CoP NMP)" = "Tom Muga Ogejo"
         , "I don't know" = "skip"
         , "Kinda Jaradat (COP)" = "Kinda Jaradat"
         , "Non for now" = "skip"
         , "Reily Tim on Strategic Information Management" = "Tim Reilly"
         , "Tim Shiflett on visualizations" = "Tim Shifflett"
         , "others on incorporation of ML into my project" = "skip"
         , "Andrew Dicello until he left the company"= "Andrew Dicello"
         , "Since I just joined the MSI team" = "skip"
         , "for now I only know of Dan" = "Dan Killian"
         , "Hallie from Home office" = "Hallie Powell"
         , "Gift from Field Office." = "Gift Sitenge"
         , "Anyone?" = "skip"
         , "Gwen Bevis" = "Gwendolyn Bevis"
         , "Mussarat Arif." = "Mussarat Arif")


edges_mentor_test2 <- edges_mentor_test |>
  filter(!is.na(to_mentor3)) |>
  filter(to_mentor3 != "skip") |>
  filter(to_mentor3 != "") #|>
#  select(ID, Name, to_mentor3)

#all the names mentioned need to be in a df for nodes
nodes_mentor_test <- as_tibble(unique(c(unique(edges_mentor_test2$Name)
                                   , unique(edges_mentor_test2$to_mentor3)))) |>
  select('Name' = value)


###create a dataframe for the tech question
#-repeat what was done above---###
edges_tech_test <- df_test |>
  mutate(to_tech2 = strsplit(to_tech_ques, "[,;&]+")) |>
  unnest("to_tech2") #|>
#  select(Name, to_tech2)

#see what names need to be fixed
unique(edges_tech_test$to_tech2)

#fix those names
edges_tech_test$to_tech2 <- edges_tech_test$to_tech2 |>
  str_trim() |>
  dplyr::recode("No one right now" = "skip"
                , "Dan" = "Dan Killian"
                , "David" = "David Hinkle"
                , "Tim S." = "Tim Schifflet"
                , "Clare Bambrick" = "Clare Bambrick"
                , "Robert Underwood" = "Robert Underwood"
                , "N/A" = "skip"
                , "Depends on the project/assignment" = "skip"
                , "NA" = "skip"
                , "I'm fairly new to MSI but I would go to Aaron Lee to run through ideas :)" = "Aaron Lee"
                , "The other Technical Director in ROLGA" = "skip"
                , "Technical Manager Clare Bambrick" = "Clare Bambrick"
                , "Supervisor Robert Underwood" = "Robert Underwood"
                , "No one right now since I'm not involved in the technical side" = "skip"
                , "Dave Hinkle Brian Calhoon" = "David Hinkle, Brian Calhoon"
                , "N/A" = "skip"
                , "Other TDs" = "skip"
                , "I do not turn to anyone for help on technical questions." = "skip"
                , "Gwen" = "Gwendolyn Bevis"
                , "Gwen Bevis" = "Gwendolyn Bevis"
                , "NA" = "skip"
                , "Ali Hayat;" = "Ali Hayat"
                , "Busayo" = "Busayo Bello"
                , "Kazeem"  = "Kazeem Adebayo"
                , "Tom" = "Tom Muga Ogejo"
                , "Kazeem Adebayo (DCoP NMP)" = "Kazeem Adebayo"
                , "within the field team" = "skip"
                , "Tim Schifflet" = "Tim Shifflett"
                , "My line manager Kinda Jaradat" = "Kinda Jaradat"
                , "Kinda Jaradat (COP)" = "Kinda Jaradat"
                , "Dave Hinkle" = "David Hinkle"
                , "Non for now" = "skip"
                , "Diego" = "Diego Parra Galan"
                , "Oscar Reyes y Diego Parra" = "Oscar Reyes, Diego Parra Galan"
                , "Tim Shiflet" = "Tim Shifflett"
                , "the PMES COP" = "Roger Pipe"
                , "previously Lynn Carter" = "Lynn Carter"
                , "not sure about a third person I \"often\" contact" = "skip"
                , "and  Mohammed Saab" = "Mohammed Saab"
                , "Timm Shifflett" = "Tim Shifflett"
                , "and Jovan Rapic." = "Jovan Rapic") 


#remove the double name ones
edges_tech_test <- edges_tech_test |>
  mutate(to_clean = strsplit(to_tech2, ", ")) |>
  unnest("to_clean") 

#remove the last white space
edges_tech_test$to_clean <- edges_tech_test$to_clean |>
  str_trim()

#remove the the "skip" and the NA
edges_tech_test <- edges_tech_test |>
  filter(!is.na(to_clean)) |>
  filter(to_clean != "skip") #|>
#  select(Name, to_clean)

#all the names mentioned need to be in a df for nodes
nodes_tech_test <- as_tibble(unique(c(unique(edges_tech_test$Name)
                                 , unique(edges_tech_test$to_clean)))) |>
  rename("Name" = "value")

###Populate the master list of nodes and generate ids----

all_nodes_names_test <- as_tibble(unique(c(nodes_mentor_test$Name
                                      , nodes_tech_test$Name)))  
#confirm names are repeated more than once
janitor::get_dupes(all_nodes_names_test)

#create unique ids for each of the names
#The id column in this is the unique id for each name
all_nodes_names_test <-  all_nodes_names_test |>
  mutate(id = 1:nrow(all_nodes_names_test)) |>
  rename('Name' = value)

#The above df contains all the nodes.

#Left join all_nodes_names to nodes_mentor object to get the ids
nodes_mentor_test2 <- nodes_mentor_test |>
  left_join(all_nodes_names_test)

#then to anonymize each df, we select only the 
# id column and write that to the file
nodes_mentor_xlsx_test <- nodes_mentor_test2 #|>
  #select(id)  

#writexl::write_xlsx(nodes_mentor_xlsx
#                    , here::here("network_analysis_sandbox/data/nodes_mentor.xlsx"))

#Then we repeat this for each of the 3 other dfs

#first I renamed some columns before joining
edges_mentor_join_test <- edges_mentor_test2 |>
  left_join(all_nodes_names_test) |>
  rename('resp' = Name, 'from' = id, 'Name' = to_mentor3) #Used the column name "Name" to make the next join easier

edges_mentor_xlsx_test <- edges_mentor_join_test |>
  left_join(all_nodes_names_test) |>
  rename('to' = id)

#writexl::write_xlsx(edges_mentor_xlsx
 #                   , here::here("network_analysis_sandbox/data/edges_mentor.xlsx"))

#Repeat the above joining fun for the tech question 
nodes_tech_xlsx_test <- nodes_tech_test |>
  left_join(all_nodes_names_test) |>
  select(id) 

#writexl::write_xlsx(nodes_tech_xlsx,
 #                   here::here("network_analysis_sandbox/data/nodes_tech.xlsx"))

edges_tech_test2 <- edges_tech_test |>
  left_join(all_nodes_names_test) |>
  rename('resp' = Name, 'from' = id, 'Name' = to_clean)

edges_tech_xlsx_test <- edges_tech_test2 |>
  left_join(all_nodes_names_test) |>
  rename('to' = id)


#writexl::write_xlsx(edges_tech_xlsx,
 #                   here::here("network_analysis_sandbox/data/edges_tech.xlsx"))

###test plots ----
#This is an igraph object of the network
edges_mentor_xlsx_test <- edges_mentor_xlsx_test |>
  relocate(c(from, to), .before = ID) 

edges_tech_xlsx_test <- edges_tech_xlsx_test |>
  relocate(c(from, to), .before = ID)

g_mentor_test <- graph_from_data_frame(edges_mentor_xlsx_test
                                  , directed = FALSE)
                               


g_tech_test <- graph_from_data_frame(edges_tech_xlsx_test
                                , directed = FALSE)

par(mfrow=c(1, 2))
plot(g_tech_test, main = "Tech Assistance")
plot(g_mentor_test, main = "Mentoring")

V(g_mentor_test)

E(g_mentor_test)

library(tidygraph)

ggraph(g_mentor_test, "with_kk") +
  geom_node_point(aes(fill = g_mentor_test$home_field)) +
  theme.graph()

