

#script to clean the raw data and format it for use
# igraph, ggraph, visNetwork 

source(here::here("network_analysis_sandbox/prep/prep.R"))

set.seed(13287)

###cleaning for descriptive stats-Mentors-----

#SEA only and only those who listed mentors

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
  filter(!is.na(practice_area)
         & !is.na(to_mentor))

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
df1_nodes <- unique(c(df1_ties$Name, df1_ties$to_mentor3)) |>
  data.frame() |>
  rename(value = 1)

#join it to the attributes
df2_nodes <- df1_nodes |>
  left_join(df2, by = c("value" = "Name"))

#write in some of the data for missing people in our network
df2_nodes$home_field <- df2_nodes$home_field |>
  replace_na("Home office")

df2_nodes$practice_area <- df2_nodes$practice_area |>
  replace_na("SEA")


#delete any duplicate nodes
df2_vertices <- subset(df2_nodes, !duplicated(value)) 



###cleaning for descriptive stats-Tech-----

#SEA only and only those who listed tech advisors

df_tech <- read_xlsx(here::here("network_analysis_sandbox/data/staff_survey_data_03-21.xlsx")) |>
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
  filter(!is.na(practice_area)
         & !is.na(to_tech_ques))

df_tech$practice_area <- df_tech$practice_area |>
  recode("Strategy, Evaluation and Analysis" = "SEA"
         , "Education" = "SEA")

#reworking this data set for mentoring
df_tech1 <- df_tech |>
  filter(practice_area == "SEA"
         & home_field == "Home office") |>
  select(-to_mentor)

df_tech1 <- df_tech1 |>
  mutate(to_tech2 = strsplit(to_tech_ques, "[,;&]+")) |>
  unnest("to_tech2") 

#need to clean up names in to_mentor column
#This allows us to view all unique rows in the column
unique(df_tech1$to_tech2)

#this fixes the names of the to_mentor3 column
df_tech1$to_tech3 <- df_tech1$to_tech2 |>
  str_trim() |> #eliminates extra spaces before and after text
  recode("No one right now" = "skip"
         , "Dan" = "Dan Killian"
         , "David" = "David Hinkle"
         , "Tim S." = "Tim Shifflett"
         , "None" = "skip"
         , "NA" = "skip"
         , "N/A" = "skip")
         

df_tech1 <- df_tech1 |>
  relocate(to_tech3, .before = home_field)

#find the individual softwares
softwares <- paste(c("R", "Stata", "SPSS", "Spss", "Nvivo", "Qualtrics", "MaxQDA"
                     , "Python", "ArcGIS", "Excel", "excel", "SAS"
                     , "EpiInfo", "STATA", "Kobo Tool Box", "Dedoose"
                     , "SMath Studio", "Tableau"), collapse = "|")

#create a new column that extracts the names of software used 
#then create another column that unnests any lists
df_tech2 <- df_tech1 |>
  mutate(new = str_extract_all(software, pattern = softwares)) |>
  filter(to_tech3 !="skip")

#Create a ties df
df_tech_ties <- df_tech2 |>
  select(Name, to_tech3)



#Create a nodes df with all attributes
#create the list of nodes
df_tech_nodes <- as_tibble(unique(c(df_tech_ties$Name, df_tech_ties$to_tech3)))

#join it to the attributes
df_tech_nodes1 <- df_tech_nodes |>
  left_join(df_tech2, by = c("value" = "Name"))

#delete any duplicate nodes
df_tech_vertices <- subset(df_tech_nodes1, !duplicated(value)) 

#replace some missing values
df_tech_vertices$home_field <- df_tech_vertices$home_field |>
  replace_na("Home office")

df_tech_vertices$practice_area <- df_tech_vertices$practice_area |>
  replace_na("SEA")

#make a graph object
df_graph_tech <- graph_from_data_frame(df_tech_ties
                                       , directed = FALSE
                                       , vertices = df_tech_vertices)

ggraph(df_graph_tech, "with_kk") +
  geom_node_point()+
  geom_edge_link() +
  geom_node_text(aes(label = df_tech_vertices$value))


###Lists to anonymize networks----

#Add a number column that can be used to anonymize each network 
df_anon <- unique(c(df1_ties$Name
                      , df1_ties$to_mentor3
                      , df_tech_ties$Name
                      , df_tech_ties$to_tech3)) |>
  as.data.frame() |>
  mutate(number = row_number()
         , .before = everything()) |>
  rename(names = 2)

#This object has mentor ties anonymized
ment_ties_anon <- df1_ties |>
  left_join(df_anon, by = c("Name" = "names")) |>
  left_join(df_anon, by = c("to_mentor3" = "names")) |>
  rename(number.name = 3, number.to = 4) |>
  select(number.name, number.to)

#Saving this as an xlsx file to use in the quarto file
#writexl::write_xlsx(ment_ties_anon
 #                    , "./network_analysis_sandbox/data/edges_mentor.xlsx")

#This object has mentor nodes anonymized
ment_nodes_anon <- df2_vertices |>
  left_join(df_anon, by = c("value" = "names")) |>
  relocate(number) |>
  select(number, 5:10, new)

#Saving this as an xlsx file to use in the quarto file
#writexl::write_xlsx(ment_nodes_anon
 #                  , "./network_analysis_sandbox/data/nodes_mentor.xlsx")

#This object has tech ties anonymized
tech_ties_anon <- df_tech_ties |>
  left_join(df_anon, by = c("Name" = "names")) |>
  left_join(df_anon, by = c("to_tech3" = "names")) |>
  rename(number.name = 3, number.to = 4) |>
  select(number.name, number.to)

#Saving this as an xlsx file to use in the quarto file
#writexl::write_xlsx(tech_ties_anon
 #                   , "./network_analysis_sandbox/data/edges_tech.xlsx")


#This object has tech nodes anonymized
tech_nodes_anon <- df_tech_vertices |>
  left_join(df_anon, by = c("value" = "names")) |>
  relocate(number) |>
  select(number, 5:10, new)


#Saving this as an xlsx file to use in the quarto file
#writexl::write_xlsx(tech_nodes_anon
 #                   , "./network_analysis_sandbox/data/nodes_tech.xlsx")


#anonymized graph object for mentorship network
graph_ment_anon <- graph_from_data_frame(ment_ties_anon
                                       , directed = FALSE
                                       , vertices = ment_nodes_anon)

graph_tech_anon <- graph_from_data_frame(tech_ties_anon
                                         , directed = FALSE
                                         , vertices = tech_nodes_anon)
