

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
edges_mentor <- edges |>
  mutate(to_mentor2 = strsplit(to_mentor, ",")) |>
  #mutate(to_tech_ques2 = strsplit(to_tech_ques, ",")) |>
  unnest("to_mentor2") |>
  select(from, to_mentor, to_mentor2)
  #unnest("to_tech_ques2")
  #separate(col = "to_tech_ques"
   #        , into = c("one_tech", "two_tech", "three_tech")
    #       , sep = ","
     #      , fill = "right") |>
  pivot_longer(cols = c("one", "two", "three")
               , names_to = "names"
               , values_to = "to_mentor") |>
  pivot_longer(cols = c("one_tech", "two_tech", "three_tech")
               , values_to = "to_tech_ques") |>
  select(from, to_mentor, to_tech_ques)

#confirm that I have from and to columns, then split into two edges dataframes
head(edges_split)

#need to clean up names in to_mentor and to_tech_ques columns
unique(edges_split$to_mentor)

edges_split$to_mentor <- edges_split$to_mentor |>
  recode("Tim R" = "Tim Reilly"
         , " Michelle Adams-Matson" = "Michelle Adams-Matson"
         , "Tim R." = "Tim Reilly"
         , "Michelle" = "Michelle Adams-Matson"
         , " John Wildgrube" = "John Wildgrube"
         , " Gaelle Simon" = "Gaelle Simon"
         , " George Sarris" = "George Sarris"
         , " Ajiri Ubogu" = "Ajiri Ubogu"
         , " Brian Calhoon" = "Brian Calhoon"
         , " Natalya Ghurbanyan" = "Natalya Ghurbanyan")

unique(edges_split$to_tech_ques)

edges_split$to_tech_ques <- edges_split$to_tech_ques |>
  recode("No one right now" = NA
         , " Melanie Murphy"  = "Melanie Murphy"
         , " Tim Reilly" = "Tim Reilly"
         , "Dan" = "Dan Killian"
         , "David" = "David Hinkle"
         , " Tim S." = "Tim Schifflet"
         , " Robert Underwood" = "Robert Underwood"
         , "Dave Hinkle" = "David Hinkle"
         , " Brian Calhoon" = "Brian Calhoon"
         , "N/A" = "NA")

#edges for the mentor question
edges_mentor <- edges_split |>
  select(from, to_mentor)

#edges for the tech question question
edges_tech <- edges_split |>
  select(from, to_tech_ques)


#
g_mentor <- graph_from_data_frame(edges_mentor
                                  , directed = FALSE
                                  , vertices = nodes)
