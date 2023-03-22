

#script to clean the raw data and format it for use
# igraph, ggraph, visNetwork 

source(here::here("network_analysis_sandbox/prep/prep.R"))

set.seed(13287)

###cleaning for descriptive stats------
#importing and cleaning column names
df <- read_xlsx(here::here("network_analysis_sandbox/scripts/staff_survey_data_03-21.xlsx")) |>
  select(Name 
        , home_field = `Are you home office or field office staff?` 
        , practice_area = `Practice Area` 
        , training = `Training/Credential specific to data analytics` 
        , advanced_analyses = `Any specific advanced analyses you have conducted?`
        , software = `Any specific competencies with statistical analysis software?` 
        , aspirations = `Any specific aspirations for data analytics?`) 


#find the individual softwares
softwares <- paste(c("R", "Stata", "SPSS", "Spss", "Nvivo", "Qualtrics", "MaxQDA"
                     , "Python", "ArcGIS", "Excel", "excel", "SAS"
                     , "EpiInfo", "STATA", "Kobo Tool Box", "Dedoose"
                     , "SMath Studio", "Tableau"), collapse = "|")

#create a new column that extracts the names of software used 
#then create another column that unnests any lists
df1 <- df |>
  mutate(new = str_extract_all(software, pattern = softwares)) |>
          unnest(new) 

#clean up a few software names
df1$new <- df1$new |>
  recode("STATA" = "Stata"
         , "Spss" = "SPSS"
         , "EXCEL" = "Excel") 

#then create a count of which software packages are used at MSI
df2 <- df1 |>
  filter(!is.na(new)) |>
  group_by(new) |>
  count() |>
  mutate(new2 = case_when(n == 1 ~ "Other"
                          , TRUE ~ new)) |>
  select("Software" = new2, "Count" = n, -new) |>
  arrange(desc(Count))


ggplot(data = df2
       , aes(reorder(factor(Software), -Count), Count)) +
  geom_point(size = 14, color = my_pal[[2]]) +
  geom_segment(aes(x = factor(df2$Software), xend = factor(df2$Software) 
               , y = 0, yend = df2$Count)
               , linewidth = 2
               , color = my_pal[[2]]
               , alpha = .7) +
  geom_text(aes(x = Software, y = Count, label = Count)
            , color = "white") #

  





###network cleaning-------  
#reading in dataframe and renaming columns to variations of from and to
df <-read_xlsx("network_analysis_sandbox/scripts/staff_survey_data_03-21.xlsx") |>
  select(ID, Name, to_mentor = `Who within MSI are you able to turn to for mentorship/career guidance?`
         , to_tech_ques = `Who within MSI do you turn to most often to discuss or get help on technical questions?`)

#reformat `to` columns by splitting them 
#and then pivoting_longer

#create a dataframe for the mentor question  
edges_mentor <- df |>
  mutate(to_mentor2 = strsplit(to_mentor, "[,;&]+")) |>
  unnest("to_mentor2") |>
  select(ID, Name, to_mentor2)
 
#had to separately clean the " and " for some reason
edges_mentor <- edges_mentor |>
  mutate(to_mentor3 = strsplit(to_mentor2, " and ")) |>
  unnest("to_mentor3")
  
#need to clean up names in to_mentor column
#This allows us to view all unique rows in the column
unique(edges_mentor$to_mentor3)

#this fixes the names of the to_mentor3 column
edges_mentor$to_mentor3 <- edges_mentor$to_mentor3 |>
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


edges_mentor2 <- edges_mentor |>
  filter(!is.na(to_mentor3)) |>
  filter(to_mentor3 != "skip") |>
  filter(to_mentor3 != "") |>
  select(ID, Name, to_mentor3)

#all the names mentioned need to be in a df for nodes
nodes_mentor <- as_tibble(unique(c(unique(edges_mentor2$Name)
                            , unique(edges_mentor2$to_mentor3)))) |>
  select('Name' = value)


###create a dataframe for the tech question
#-repeat what was done above---###
edges_tech <- df |>
  mutate(to_tech2 = strsplit(to_tech_ques, "[,;&]+")) |>
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
edges_tech <- edges_tech |>
  mutate(to_clean = strsplit(to_tech2, ", ")) |>
  unnest("to_clean") 

#remove the last white space
edges_tech$to_clean <- edges_tech$to_clean |>
  str_trim()

#remove the the "skip" and the NA
edges_tech <- edges_tech |>
  filter(!is.na(to_clean)) |>
  filter(to_clean != "skip") |>
  select(Name, to_clean)
  
#all the names mentioned need to be in a df for nodes
nodes_tech <- as_tibble(unique(c(unique(edges_tech$Name)
                                   , unique(edges_tech$to_clean)))) |>
  rename("Name" = "value")

###Populate the master list of nodes and generate ids----

all_nodes_names <- as_tibble(unique(c(nodes_mentor$Name
                                      , nodes_tech$Name)))  
#confirm names are repeated more than once
janitor::get_dupes(all_nodes_names)

#create unique ids for each of the names
#The id column in this is the unique id for each name
all_nodes_names <-  all_nodes_names |>
  mutate(id = 1:nrow(all_nodes_names)) |>
  select(id, 'Name' = value)

#The above df contains all the nodes.

#Left join all_nodes_names to nodes_mentor object to get the ids
nodes_mentor2 <- nodes_mentor |>
  left_join(all_nodes_names)

#then to anonymize each df, we select only the 
# id column and write that to the file
nodes_mentor_xlsx <- nodes_mentor2 |>
  select(id)  

writexl::write_xlsx(nodes_mentor_xlsx
                    , "network_analysis_sandbox/data/nodes_mentor.xlsx")

#Then we repeat this for each of the 3 other dfs

#first I renamed some columns before joining
edges_mentor_join <- edges_mentor2 |>
  left_join(all_nodes_names) |>
  select('from' = id, 'Name' = to_mentor3) #Used the column name "Name" to make the next join easier

edges_mentor_xlsx <- edges_mentor_join |>
  left_join(all_nodes_names) |>
  select(from, 'to' = id)

writexl::write_xlsx(edges_mentor_xlsx
                    , "network_analysis_sandbox/data/edges_mentor.xlsx")

#Repeat the above joining fun for the tech question 
nodes_tech_xlsx <- nodes_tech |>
  left_join(all_nodes_names) |>
  select(id) 

writexl::write_xlsx(nodes_tech_xlsx,
           "network_analysis_sandbox/data/nodes_tech.xlsx")

edges_tech2 <- edges_tech |>
  left_join(all_nodes_names) |>
  select('from' = id, 'Name' = to_clean)

edges_tech_xlsx <- edges_tech2 |>
  left_join(all_nodes_names) |>
  select(from, 'to' = id)


writexl::write_xlsx(edges_tech_xlsx,
                    "network_analysis_sandbox/data/edges_tech.xlsx")

###test plots ----
#This is an igraph object of the network
g_mentor <- graph_from_data_frame(edges_mentor_xlsx
                                  , directed = FALSE
                                  , vertices = nodes_mentor_xlsx)



g_tech <- graph_from_data_frame(edges_tech_xlsx
                                  , directed = FALSE
                                  , vertices = nodes_tech_xlsx)

par(mfrow=c(1, 2))
plot(g_tech, main = "Tech Assistance")
plot(g_mentor, main = "Mentoring")

V(g_mentor)

E(g_mentor)

