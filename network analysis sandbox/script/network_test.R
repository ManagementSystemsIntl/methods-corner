

#script to test out basic viz of network

source("network analysis sandbox/prep/prep.R")

#reading in the nodes - keeping ID and Names column for now
nodes <- read_xlsx("network analysis sandbox/data/DATA SCIENCE AND ADVANCED ANALYTICS(1-13).xlsx") |>
                  select(ID, Name)

#reading in edges and renaming columns to variations of from and to
edges <-read_xlsx("network analysis sandbox/data/DATA SCIENCE AND ADVANCED ANALYTICS(1-13).xlsx") |>
  select(from = `ID`, to_mentor = `Who within MSI are you able to turn to for mentorship/career guidance?`
         , to_tech_ques = `Who within MSI do you turn to most often to discuss or get help on technical questions?`)

edges_split <- edges |>
  

#reformat `to` columns by splitting them and then pivoting_longer

#create a dataframe for the mentor question  
edges_mentor <- edges |>
  mutate(to_mentor2 = strsplit(to_mentor, ",")) |>
  unnest("to_mentor2") |>
  select(from, to_mentor2)

#create a dataframe for the tech question
edges_tech <- edges |>
  mutate(to_tech2 = strsplit(to_tech_ques, ",")) |>
  unnest("to_tech2") |>
  select(from, to_tech2)


#need to clean up names in to_mentor and to_tech_ques columns
unique(edges_mentor$to_mentor2)

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

unique(edges_tech$to_tech2)

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

#Populate the nodes with edges who are not present in the nodes

mentors <- unique(edges_mentor$to_mentor2)

techs <- unique(edges_tech$to_tech2) 

sources <- unique(nodes$Name)

edge_names <- unique(c(techs, mentors, sources)) 

#The above vector contains all the nodes.
#resume here. Putting all the names into the nodes dataset
test <- bind_rows(nodes, edge_names)

##Fixing this
nodes2 <- nodes |>
  mutate(names = case_when(Name %in% edge_names$value ~ Name
                           , !str_detect(Name, edge_names) ~ edge_names))


#
g_mentor <- graph_from_data_frame(edges_mentor
                                  , directed = FALSE
                                  , vertices = nodes)
