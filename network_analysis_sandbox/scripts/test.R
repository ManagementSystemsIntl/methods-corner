
library(tidygraph)

g <- as_tbl_graph(edges_mentor_xlsx) |>
  activate(nodes) |>
  mutate(degree = centrality_degree()) |>
  activate(edges) |>
  mutate(centrality = centrality_edge_betweenness()) |>
  arrange(centrality)


activate( nodes) |>
  local_size(g)

V(g)$degree
E(g)$centrality
E(g)

ecount(g)

E(g)$weight
ggraph(g, layout = "with_kk") +
  geom_node_point(aes(size = strength)
                  , color = "red")+
  geom_node_text(aes(label = name)) +
  geom_edge_link(aes(alpha = centrality))


###testing out additional vertice attributes
df <- read_xlsx(here::here("network_analysis_sandbox/data/staff_survey_data_03-21.xlsx")) |>
  select(ID
         , Name 
         , home_field = `Are you home office or field office staff?` 
         , practice_area = `Practice Area` 
         , training = `Training/Credential specific to data analytics` 
         , advanced_analyses = `Any specific advanced analyses you have conducted?`
         , software = `Any specific competencies with statistical analysis software?` 
         , aspirations = `Any specific aspirations for data analytics?`
         , to_mentor = `Who within MSI are you able to turn to for mentorship/career guidance?`
         , to_tech_ques = `Who within MSI do you turn to most often to discuss or get help on technical questions?`) |>
  mutate(respondent = 1) |>
  filter(!is.na(practice_area)) 

df$practice_area <- df$practice_area |>
  recode("Strategy, Evaluation and Analysis" = "SEA"
         , "Education" = "SEA")

#reworking this data set for mentoring
df1 <- df |>
  filter(practice_area == "SEA"
         & home_field == "Home office") |>
  select(-to_tech_ques)

df1 <- df1 |>
  mutate(to_mentor2 = strsplit(to_mentor, "[,;&]+")) |>
  unnest("to_mentor2") 

#had to separately clean the " and " for some reason
df1 <- df1 |>
  mutate(to_mentor3 = strsplit(to_mentor2, " and ")) |>
  unnest("to_mentor3")

#need to clean up names in to_mentor column
#This allows us to view all unique rows in the column
unique(df1$to_mentor3)

#this fixes the names of the to_mentor3 column
df1$to_mentor3 <- df1$to_mentor3 |>
  str_trim() |> #eliminates extra spaces before and after text
  recode("Tim R" = "Tim Reilly"
         , "Michelle" = "Michelle Adams-Matson"
         , "Tim R." = "Tim Reilly"
         , "None" = "skip"
         , "NA" = "skip"
         , "N/A" = "skip") 

df1 <- df1 |>
  relocate(to_mentor3, .before = home_field)

#find the individual softwares
softwares <- paste(c("R", "Stata", "SPSS", "Spss", "Nvivo", "Qualtrics", "MaxQDA"
                     , "Python", "ArcGIS", "Excel", "excel", "SAS"
                     , "EpiInfo", "STATA", "Kobo Tool Box", "Dedoose"
                     , "SMath Studio", "Tableau"), collapse = "|")

#create a new column that extracts the names of software used 
#then create another column that unnests any lists
df2 <- df1 |>
  mutate(new = str_extract_all(software, pattern = softwares))

#Create a ties df
df1_ties <- df2 |>
  select(Name, to_mentor3) |>
  filter(to_mentor3 != "skip")

#Create a nodes df with all attributes
df1_nodes <- as_tibble(unique(c(df2$Name, df2$to_mentor3))) |>
  filter(value != "skip" &
           !is.na(value))

#Create a nodes df with all attributes
#create the list of nodes
df1_nodes <- as_tibble(unique(c(df1_ties$Name, df1_ties$to_mentor3)))

#join it to the attributes
df2_nodes <- df1_nodes |>
  left_join(df2, by = c("value" = "Name"))

#delete any duplicate nodes
df2_vertices <- subset(df2_nodes, !duplicated(value)) 

###create graph object

df_graph_ment <- graph_from_data_frame(df1_ties
                                  , directed = FALSE
                                  , vertices = df2_vertices)

ggraph(df_graph_ment, "with_kk") +
  geom_node_point()+
  geom_edge_link() +
  geom_node_text(aes(label = df2_vertices$value))

#find the individual softwares
softwares <- paste(c("R", "Stata", "SPSS", "Spss", "Nvivo", "Qualtrics", "MaxQDA"
                     , "Python", "ArcGIS", "Excel", "excel", "SAS"
                     , "EpiInfo", "STATA", "Kobo Tool Box", "Dedoose"
                     , "SMath Studio", "Tableau"), collapse = "|")

#create a new column that extracts the names of software used 
#then create another column that unnests any lists
df2 <- df1 |>
  mutate(new = str_extract_all(software, pattern = softwares)) #|>
#  unnest(new) 

#clean up a few software names
df1$new <- df1$new |>
  recode("STATA" = "Stata"
         , "Spss" = "SPSS"
         , "EXCEL" = "Excel") 

df1 <- df1 |>
  mutate(to_mentor2 = strsplit(to_mentor, "[,;&]+")) |>
  unnest("to_mentor2") #|>
#select(ID, Name, to_mentor2)

#had to separately clean the " and " for some reason
df1 <- df1 |>
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
nodes_mentor_test <- as_tibble(unique(c(edges_mentor_test2$Name
                                   , edges_mentor_test2$to_mentor3))) |>
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
nodes_tech_test <- as_tibble(unique(c(edges_tech_test$Name
                                 , edges_tech_test$to_clean))) |>
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
  left_join(all_nodes_names_test) #|>
  #select(id) 

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

edges_mentor_xlsx_test$home_field <- edges_mentor_xlsx_test$home_field |>
  recode("Home office" = "1" 
         , "Field office" = "2" )

edges_tech_xlsx_test <- edges_tech_xlsx_test |>
  relocate(c(from, to), .before = ID)

nodes_test <- 

edges_g <- edges_mentor_xlsx_test |>
  select(1:2)

nodes_test <- nodes_mentor_xlsx_test |>
  select(4) 

####smaller test##-------

#This uses only the SEA staff
df_test$practice_area <- df_test$practice_area |>
  recode("Strategy, Evluation and Analysis" = "SEA"
         , "Education" = "SEA") 



head(df_test1)

df_test1_ties <- df_test1 |>
  select()
  
g_mentor_test <- graph_from_data_frame(edges_g
                                  , directed = FALSE
                                  , vertices = nodes_mentor_test)
                               


g_tech_test <- graph_from_data_frame(edges_tech_xlsx_test
                                , directed = FALSE)

par(mfrow=c(1, 2))
plot(g_tech_test, main = "Tech Assistance")
plot(g_mentor_test, main = "Mentoring")

V(g_mentor_test)

E(g_mentor_test)

V(g_mentor_test)$home_field

E(g_mentor_test)$color <- E(g_mentor_test)$home_field

E(g_mentor_test)$home_field <- gsub("Home office", "red"
                               , E(g_mentor_test)$home_field)
E(g_mentor_test)$color <- gsub("Field office", "blue"
                               , E(g_mentor_test)$color)


ggraph(g_mentor_test, "with_kk") +
  geom_node_label(aes(name)) +
  geom_edge_link(aes(color = home_field))+
  theme.graph()

###--------testing out--------------

library(igraph)

actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David",
                            "Esmeralda"),
                     age=c(48,33,45,34,21),
                     gender=c("F","M","F","M","F"))
relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",
                               "David", "Esmeralda"),
                        to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
                        same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
                        friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))
g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)
print(g, e=TRUE, v=TRUE)

## The opposite operation
as_data_frame(g, what="vertices")
as_data_frame(g, what="edges")