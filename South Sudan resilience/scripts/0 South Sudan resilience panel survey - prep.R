# prep

# packages ---- 

base_packages <- c("tidyverse", "easystats", "corrplot","DescTools","estimatr","extrafont","janitor",
                   "reshape2", "haven", "broom","HH","Hmisc","plotrix","scales","sysfonts","foreign",
                   "car", "ICC","openxlsx","readr","readxl","sjmisc","sjPlot","flextable", "sjstats",
                   "sjlabelled","skimr", "labelled", "texreg","psych","viridis","here","jtools",
                   "huxtable","stringi", "kableExtra", "knitr")

lapply(base_packages, library, character.only=T)

viz_packages <- c("patchwork","gganimate","ggstatsplot","ggthemes","ggrepel","ggpubr","cowplot","ggdist","ggtext",
                  "geomtextpath","ggfortify", "ggridges", "gghighlight")
lapply(viz_packages, library, character.only=T)

model_packages <- c("plm","estimatr")
lapply(model_packages, library, character.only=T)


bayes_packages <- c("rstan","rstanarm","brms")
lapply(bayes_packages, library, character.only=T)


ie_packages <- c("gsynth")
lapply(ie_packages, library, character.only=T)

map_packages <- c("rgeoboundaries", "ggmap", "sf","rnaturalearth","rnaturalearthdata",
                  "mapview", "spData", "tmap", "crsuggest")

lapply(map_packages, library, character.only=T)

table_packages <- c("gt", "gtsummary", "gtExtras","flextable")
lapply(table_packages, library, character.only=T)

miss_packages <- c("missForest","missMDA")
lapply(miss_packages,library, character.only=T)

library(dplyr)

# formatting ---- 

options(digits=3, scipen=6)

# set default
base <- theme_bw() + theme(panel.grid.minor.x=element_blank(),
                           panel.grid.minor.y=element_blank(),
                           plot.title=element_text(face="bold",size=18, hjust=.5, family = "Source Sans Pro"),
                           plot.subtitle = element_text(size=16, family="Source Sans Pro"),
                           plot.caption=element_text(size=12, family="Source Sans Pro"),
                           axis.title=element_text(size=16, family="Source Sans Pro"),
                           axis.text=element_text(size=14, family="Source Sans Pro"),
                           legend.text=element_text(size=14, family="Source Sans Pro"),
                           strip.text=element_text(size=14, family="Source Sans Pro"),
                           panel.border=element_blank(),
                           axis.ticks = element_blank())

theme_set(base)

faceted <- theme_bw() +
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          plot.title=element_text(face="bold",size=18, hjust=.5, family = "Source Sans Pro"),
          plot.subtitle = element_text(size=16, family="Source Sans Pro"),
          plot.caption=element_text(size=12, family="Source Sans Pro"),
          axis.title=element_text(size=16, family="Source Sans Pro"),
          axis.text=element_text(size=14, family="Source Sans Pro"),
          legend.text=element_text(size=14, family="Source Sans Pro"),
          strip.text=element_text(size=14, family="Source Sans Pro"))



facet_style <- function(){theme_bw() +
        theme(panel.grid.minor.x=element_blank(),
              panel.grid.minor.y=element_blank(),
              plot.title=element_text(face="bold",size=18, hjust=.5, family = "Source Sans Pro"),
              plot.subtitle = element_text(size=16, family="Source Sans Pro"),
              plot.caption=element_text(size=12, family="Source Sans Pro"),
              axis.title=element_text(size=16, family="Source Sans Pro"),
              axis.text=element_text(size=14, family="Source Sans Pro"),
              legend.text=element_text(size=14, family="Source Sans Pro"),
              strip.text=element_text(size=14, family="Source Sans Pro"))
}


# USAID_palette ---- 

usaid_blue <- "#002F6C"
usaid_red <- "#BA0C2F"
rich_black <- "#212721"
medium_blue <- "#0067B9"
light_blue <- "#A7C6ED"
web_blue <- "#205493"
dark_red <- "#651D32"
dark_grey <- "#6C6463"
medium_grey <- "#8C8985"
light_grey <- "#CFCDC9"


# labels and keys ---- 

sex_key <- data.frame(sex=1:2,
                      sex_lab=c("Male","Female"))

sex_key


inc_labs <- c("Farm/crop production",
              "Cattle production/sales",
              "Goat production/sales",
              "Sheep production/sales",
              "Fishing and sales",
              "Ag wage labor in village",
              "Ag wage labor outside village",
              "Wage labor in village",
              "Wage labor outside village",
              "Salaried work",
              "Wild bush sales",
              "Honey production/sales",
              "Petty trade other products",
              "Petty trade own products",
              "Other self-employment ag",
              "Other self-employment non-ag",
              "Rental of land/property",
              "Remittances",
              "Gifts/inheritance",
              "Food / cash safety net")

inc_labs2 <- c("a. Farm/crop production",
               "b. Cattle production/sales",
               "c. Goat production/sales",
               "d. Sheep production/sales",
               "e. Fishing and sales",
               "f. Ag wage labor in village",
               "g. Ag wage labor outside village",
               "h. Wage labor in village",
               "i. Wage labor outside village",
               "j. Salaried work",
               "k. Wild bush sales",
               "l. Honey production/sales",
               "m. Petty trade other products",
               "n. Petty trade own products",
               "o. Other self-employment ag",
               "p. Other self-employment non-ag",
               "q. Rental of land/property",
               "r. Remittances",
               "s. Gifts/inheritance",
               "t. Food / cash safety net")

inc_key <- read_csv(here("output/tables/keys/inc key.csv"))
inc_key

inc_key <- data.frame(inc_code=1:20,
                        varname=names(inc_key),
                       inc_lab=inc_labs,
                        inc_lab2=inc_labs2)
  
write_csv(inc_key, here("output/tables/inc key.csv"))
#  
#  

shock_key <- read_csv(here("output/tables/keys/shock key.csv"))

shock_labs <- c("Floods",
                "Drought",
                "Erosion",
                "Loss of land",
                "Increase in food prices",
                "Theft", 
                "No crop inputs",
                "Crop disease",
                "Crop pests",
                "Crop theft",
                "No livestock inputs",
                "Livestock disease",
                "Livestock theft",
                "Unable to sell",
                "Illness in household",
                "Death in household")

resil_labs <- c("Anticipatory capacity","Absorptive capacity","Transformative capacity","Adaptive capacity","Financial capital",
                "Social capital","Institutional capital","Human capital")

resil_items <- c("Prepared for future shock","Able to recover from shock","Able to change livelihood to adapt to any shock","Able to adapt to increased frequency or severity of shock",
                 "Able to access financial support","Can rely on family and friends","Can rely on government support","Have learned from past shocks")


resil_key <- data.frame(resil_num=1:8, resil=resil_labs, item=resil_items)
resil_key

county_labs <- c("Akobo",
                 "Budi",
                 "Duk",
                 "Leer",
                 "Mayendit",
                 "Panyijar",
                 "Pibor",
                 "Uror",
                 "Wau")

county_labs2 <- c("Akobo (Nile)",
                  "Budi (Equa)",
                  "Duk (Nile)",
                  "Leer (Nile)",
                  "Mayendit (Nile)",
                  "Panyijar (Nile)",
                  "Pibor (Nile)",
                  "Uror (Nile)",
                  "Wau (Ghazel)")

reg_county_labs <- c("Greater Upper Nile - Akobo",
                     "Equatoria - Budi",
                     "Greater Upper Nile - Duk",
                     "Greater Upper Nile - Leer",
                     "Greater Upper Nile - Mayendit",
                     "Greater Upper Nile - Panyijar",
                     "Greater Upper Nile - Pibor",
                     "Greater Upper Nile - Uror",
                     "Bahr El Ghazel - Wau")


# cnty_key <- data.frame(cnty_code = 1:9,
#                        county_lab=county_labs,
#                        reg_cnty_lab=reg_county_labs)
# 
# cnty_key 

county_key <- read_csv(here("output/tables/keys/county_key.csv"))
county_key

# reg_state_cnty_key <- dat %>%
#   group_by(region, state, county) %>%
#   dplyr::summarize(n=n()) %>%
#   dplyr::select(-n)
# 
# reg_state_cnty_key 

geo_key <- read_csv(here("output/tables/keys/geo key.csv"))


diet_labs <- c("Cereals",
               "Roots/tubers",
               "Vit A vegetables",
               "Leafy greens",
               "Other vegetables",
               "Vit A fruits",
               "Other fruits",
               "Organ meat",
               "Domesticated meat",
               "Bush meat",
               "Eggs",
               "Fish",
               "Legumes",
               "Milk products",
               "Oil and fats",
               "Sweets",
               "Spices/condiments")

diet_key <- data.frame(diet_code=1:17,
                       diet_lab=diet_labs) 

fies_key <- read_csv(here("output/tables/keys/fies key.csv"))

conf_labs <- c("Land",
               "Water",
               "Pasture",
               "Forestry",
               "Cattle",
               "Goat",
               "Migration route",
               "Boundary",
               "Revenge",
               "Dowry",
               "Elopement",
               "Cattle raid",
               "Fishing grudge",
               "Gender based violence",
               "Livelihood",
               "Agriculture")

# conf_key <- data.frame(varname=conf_types_mn$conflict,
#                         conf_key=1:16,
#                         conf_lab=conf_labs)
#  
# write_csv(conf_key, "output/tables/conf_key.csv")

conf_key <- read_csv(here("output/tables/keys/conf_key.csv"))

conf_key 

resolve_labs <- c("Dialogue",
              "Alternative dispute resolution",
              "Traditional elders",
              "Faith-based organization",
              "Judiciary",
              "Police/Army",
              "UN",
              "Other")

resolve_key <- data.frame(res_num=1:8,
                      res_lab=resolve_labs)

resolve_key  

satis_labs <- c("Not at all satisfied","A little satisfied","Satisfied","Very satisfied")

effect_labs <- c("Not effective","A little effective","Effective","Very effective")

socap_labs <- c("Able to lean on relatives\nin community (Bonding)",
                "Able to help relatives\nin community (Bonding)",
                "Able to lean on non-relatives\nin community (Bonding)",
                "Able to help non-relatives\nin community (Bonding)",
                "Able to lean on relatives\noutside community (Bridging)",
                "Able to help relatives\noutside community (Bridging)",
                "Able to lean on non-relatives\n outside community (Bridging)",
                "Able to help non-relatives\n outside community (Bridging)")

socap_labs

grp_items <- c("q501a","q501b","q501c","q501d","q501e","q501f","q501g","q501h","q501i","q501j","q501k","q501l","q501m","q501n","q501o","q501p")


grp_labs <- c("Water users",
              "Grazing land users",
              "Community natural resources",
              "Credit/microfinance",
              "Savings",
              "Mutual help",
              "Religious",
              "Mothers",
              "Women",
              "Youth",
              "Sports",
              "Disaster planning",
              "Cattle raiding",
              "Cattle herding",
              "Cattle protection",
              "Small animal herding")

grp_key <- data.frame(grp_code=1:16,
                      grp_name = grp_labs)

grp_key

ews_labs <- c("Natural hazards",
              "Weather patterns",
              "Upcoming rainfall",
              "Water prices/availability",
              "Animal health",
              "Crop health",
              "Livestock prices",
              "Animal product prices",
              "Grazing conditions",
              "Conflict",
              "Food prices")

ews_key <- data.frame(ews_code = 1:11,
                      ews_lab = ews_labs)

ews_key

emerg_labs <- c("Emergency action plan in place",
                "Emergency plan addressed shock that household faced",
                "Emergency plan successfully mitigated effect of shock",
                "Disaster planning group in community")

rate_labs <- c("Never",
               "Rarely",
               "Sometimes",
               "Often")
rate_labs

hhs_labs <- c("Never",
              "Sometimes",
              "Often")

q314_key <- data.frame(q_314=1:9,
                       activity_lab=c("Crop farming",
                                      "Defense",
                                      "Education",
                                      "Animal husbandry",
                                      "Trade",
                                      "Public administration",
                                      "Health",
                                      "Construction",
                                      "Public servant"))

age_dec_key <- data.frame(age_dec=1:10,
                          age_lab=c("0-10",
                                    "11-19",
                                    "20-29",
                                    "30-39",
                                    "40-49",
                                    "50-59",
                                    "60-69",
                                    "70-79",
                                    "80-89",
                                    "90+"))

age_dec_key

ed_ord_key <- data.frame(hh_ed_ord=0:2,
                         ed_lab=c("None","Primary","Secondary or higher"))

ed_ord_key

traffic_labs <- c("Revenge","Money","Support household","Feed community","Have more children","Never acceptable")

traffic_key <- data.frame(traffic_num=1:6,
                          traffic_lab=traffic_labs)

traffic_labs2 <- c("Strongly Disagree", "Disagree", "Slightly Disagree", "Sligthly Agree", "Agree", "Strongly Agree")

traffic_labs4 <-c("Acceptable", "Never Acceptable")

traffic_key2 <- data.frame(traffic_num=1:6,
                          traffic_lab=traffic_labs2)

traffic_key2


# read data ---- 

dat <- read_rds(here("data/prepared/South Sudan resilience panel survey - prepared.rds"))

datNames <- data.frame(names(dat))

