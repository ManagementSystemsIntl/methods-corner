

#script to test out basic viz of network

source("network analysis sandbox/prep/prep.R")

#reading in the nodes - keeping ID and Names column for now
nodes <- read_xlsx("network analysis sandbox/data/DATA SCIENCE AND ADVANCED ANALYTICS(1-13).xlsx") |>
                  select(ID, Name)

#reading in edges and renaming columns to variations of from and to
df <-read_xlsx("network analysis sandbox/data/DATA SCIENCE AND ADVANCED ANALYTICS(1-13).xlsx") |>
  select(from = `ID`, Name, to_mentor = `Who within MSI are you able to turn to for mentorship/career guidance?`
         , to_tech_ques = `Who within MSI do you turn to most often to discuss or get help on technical questions?`)

#reformat `to` columns by splitting them and then pivoting_longer

#create a dataframe for the mentor question  
edges_mentor <- df |>
  mutate(to_mentor2 = strsplit(to_mentor, ",")) |>
  unnest("to_mentor2") |>
  select(from, Name, to_mentor2)

#need to clean up names in to_mentor and to_tech_ques columns
unique(edges_mentor$to_mentor2)

#this fixes the names of the to_mentor2 column
edges_mentor$to_mentor2 <- edges_mentor$to_mentor2 |>
  recode("Tim R" = "Tim Reilly"
         , " Michelle Adams-Matson" = "Michelle Adams-Matson"
         , "Tim R." = "Tim Reilly"
         , " Michelle" = "Michelle Adams-Matson"
         , " John Wildgrube" = "John Wildgrube"
         , " Gaelle Simon" = "Gaelle Simon"
         , " George Sarris" = "George Sarris"
         , " Ajiri Ubogu" = "Ajiri Ubogu"
         , " Brian Calhoon" = "Brian Calhoon"
         , " Natalya Ghurbanyan" = "Natalya Ghurbanyan")

#all the names mentioned need to be in a df for nodes
nodes_mentor <- as_tibble(unique(c(unique(edges_mentor$Name)
                            , unique(edges_mentor$to_mentor2)))) |>
  filter(!is.na(value))


#create a dataframe for the tech question
edges_tech <- edges |>
  mutate(to_tech2 = strsplit(to_tech_ques, ",")) |>
  unnest("to_tech2") |>
  select(from, Name, to_tech2)

#see what names need to be fixed
unique(edges_tech$to_tech2)

#fix those names
edges_tech$to_tech2 <- edges_tech$to_tech2 |>
  dplyr::recode("No one right now" = ""
         , " Melanie Murphy"  = "Melanie Murphy"
         , " Tim Reilly" = "Tim Reilly"
         , "Dan" = "Dan Killian"
         , " David" = "David Hinkle"
         , " Tim S." = "Tim Schifflet"
         , " Clare Bambrick" = "Clare Bambrick"
         , " Robert Underwood" = "Robert Underwood"
         , "Dave Hinkle" = "David Hinkle"
         , " Brian Calhoon" = "Brian Calhoon"
         , "N/A" = "")

#all the names mentioned need to be in a df for nodes
nodes_tech <- as_tibble(unique(c(unique(edges_tech$Name)
                                   , unique(edges_tech$to_tech2)))) |>
  filter(!is.na(value) 
         | value != "N/A"
         | value == " ") |>
  na.omit()

#remove one blank row
nodes_tech <- nodes_tech[-13,]
#Populate the nodes with edges who are not present in the nodes

mentors <- unique(edges_mentor$to_mentor2)

techs <- unique(edges_tech$to_tech2) 

sources <- unique(nodes$Name)

#This will create our list of master IDs so that nodes are identified 
#by the same number in each network
all_nodes_names <- as_tibble(unique(c(techs, mentors, sources))) |>
  filter(!is.na(value)) |>
  na.omit()

#remove one blank row
all_nodes_names <- all_nodes_names[-1,]

#The above vector contains all the nodes.

#
g_mentor <- graph_from_data_frame(edges_mentor
                                  , directed = FALSE
                                  , vertices = nodes_mentor)
