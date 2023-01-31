

#script to clean the raw data and format it for use
# igraph, ggraph, visNetwork 

source("network analysis sandbox/prep/prep.R")

set.seed(13287)

#reading in the nodes - keeping ID and Names column for now
nodes <- read_xlsx("network analysis sandbox/data/DATA SCIENCE AND ADVANCED ANALYTICS(1-13).xlsx") |>
                  select(ID, Name)

#reading in edges and renaming columns to variations of from and to
df <-read_xlsx("network analysis sandbox/data/DATA SCIENCE AND ADVANCED ANALYTICS(1-13).xlsx") |>
  select(ID, Name, to_mentor = `Who within MSI are you able to turn to for mentorship/career guidance?`
         , to_tech_ques = `Who within MSI do you turn to most often to discuss or get help on technical questions?`)

#reformat `to` columns by splitting them 
#and then pivoting_longer

#create a dataframe for the mentor question  
edges_mentor <- df |>
  mutate(to_mentor2 = strsplit(to_mentor, ",")) |>
  unnest("to_mentor2") |>
  select(ID, Name, to_mentor2)
 
#need to clean up names in to_mentor column
#This allows us to view all unique rows in the column
unique(edges_mentor$to_mentor2)

#this fixes the names of the to_mentor2 column
edges_mentor$to_mentor2 <- edges_mentor$to_mentor2 |>
  str_trim() |> #eliminates extra spaces before and after text
  recode("Tim R" = "Tim Reilly"
         , "Tim R." = "Tim Reilly"
         , "Michelle" = "Michelle Adams-Matson")
        
edges_mentor <- edges_mentor |>
  filter(!is.na(to_mentor2)) |>
  select(ID, Name, to_mentor2)

#all the names mentioned need to be in a df for nodes
nodes_mentor <- as_tibble(unique(c(unique(edges_mentor$Name)
                            , unique(edges_mentor$to_mentor2)))) |>
  select('Name' = value)


###create a dataframe for the tech question
#-repeat what was done above---###
edges_tech <- df |>
  mutate(to_tech2 = strsplit(to_tech_ques, ",")) |>
  unnest("to_tech2") |>
  select(Name, to_tech2)

#see what names need to be fixed
unique(edges_tech$to_tech2)

#fix those names
edges_tech$to_tech2 <- edges_tech$to_tech2 |>
  str_trim() |>
  dplyr::recode("No one right now" = "skip"
         , "Dan" = "Dan Killian"
         , "David" = "David Hinkle"
         , "Tim S." = "Tim Schifflet"
         , "Clare Bambrick" = "Clare Bambrick"
         , "Robert Underwood" = "Robert Underwood"
         , "Dave Hinkle" = "David Hinkle"
         , "Brian Calhoon" = "Brian Calhoon"
         , "N/A" = "skip") 

edges_tech <- edges_tech |>
  filter(!is.na(to_tech2)) |>
  filter(to_tech2 != "skip")
  
#all the names mentioned need to be in a df for nodes
nodes_tech <- as_tibble(unique(c(unique(edges_tech$Name)
                                   , unique(edges_tech$to_tech2)))) |>
  rename("Name" = "value")

###Populate the master list of nodes and ----
#make unique ids for each name

mentors <- unique(edges_mentor$to_mentor2)

techs <- unique(edges_tech$to_tech2) 

sources <- unique(nodes$Name)

#This will create our list of master IDs so that nodes are identified 
#by the same number in each network
all_nodes_names <- as_tibble(unique(c(techs, mentors, sources)))
  
#confirm names are repeated more than once
janitor::get_dupes(all_nodes_names)

#create unique ids for each of the names
#The id column in this is the unique id for each name
all_nodes_names <-  all_nodes_names |>
  mutate(id = 1:nrow(all_nodes_names)) |>
  select(id, 'Name' = value)

#The above df contains all the nodes.

#Left join them to the mentor nodes object to get the ids
nodes_mentor2 <- nodes_mentor |>
  left_join(all_nodes_names)

#then to anonymize each df, we select only the 
# id column and write that to the file
nodes_mentor2 <- select(nodes_mentor2, id)  

writexl::write_xlsx(nodes_mentor2
                    , "network analysis sandbox/data/nodes_mentor.xlsx")

#Then we repeat this for each of the 3 other dfs

#first I renamed some columns before joining
edges_mentor2 <- edges_mentor |>
  left_join(all_nodes_names) |>
  select('from' = id, 'Name' = to_mentor2)

edges_mentor2 <- edges_mentor2 |>
  left_join(all_nodes_names) |>
  select(from, 'to' = id)

writexl::write_xlsx(edges_mentor2
                    , "network analysis sandbox/data/nodes_edges.xlsx")

#Repeat the above joining fun for the tech question 
nodes_tech2 <- nodes_tech |>
  left_join(all_nodes_names) |>
  select(id) 

writexl::write_xlsx(nodes_tech2,
           "network analysis sandbox/data/nodes_tech.xlsx")

edges_tech2 <- edges_tech |>
  left_join(all_nodes_names) |>
  select('from' = id, 'Name' = to_tech2)

edges_tech2 <- edges_tech2 |>
  left_join(all_nodes_names) |>
  select(from, 'to' = id)


writexl::write_xlsx(edges_tech2,
                    "network analysis sandbox/data/edges_tech.xlsx")

###test plots ----
#This is an igraph object of the network
g_mentor <- graph_from_data_frame(edges_mentor2
                                  , directed = FALSE
                                  , vertices = nodes_mentor2)



g_tech <- graph_from_data_frame(edges_tech2
                                  , directed = FALSE
                                  , vertices = nodes_tech2)

par(mfrow=c(1, 2))
plot(g_tech, main = "Tech Assistance")
plot(g_mentor, main = "Mentoring")
