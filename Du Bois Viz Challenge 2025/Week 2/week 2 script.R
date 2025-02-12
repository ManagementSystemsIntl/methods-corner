# Du Bois Visualization Challenge
# Week 2

# first, install base and viz packages
# not all are needed here but good to have 

# base packages
base_packages <- c("tidyverse", "easystats", "corrplot","DescTools","estimatr","extrafont","janitor",
                   "reshape2", "haven", "broom","HH","Hmisc","plotrix","scales","sysfonts","foreign","car",
                   "ICC","openxlsx","readr","readxl","sjmisc","sjPlot","flextable", "sjstats","sjlabelled","skimr",
                   "labelled", "texreg","viridis","here","jtools","huxtable","stringi", "kableExtra", "psych", "knitr", 
                   "fastDummies", "sn", "diagis")

# install packages not yet installed
installed_packages <- base_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(base_packages[!installed_packages])
}

# load packages
lapply(base_packages, library, character.only=T)

# viz packages
viz_packages <- c("patchwork","gganimate","ggstatsplot","ggthemes","ggrepel","ggpubr","cowplot","ggdist","ggtext",
                  "geomtextpath","ggfortify", "ggridges", "gghighlight")

# install packages not yet installed
installed_packages <- viz_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(viz_packages[!installed_packages])
}

# load packages
lapply(viz_packages, library, character.only=T)

# next let's make objects for color options
# there are plenty base R colors you can 
# choose from using a simple Google search

# USAID color palette 
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

# now we can start with the week 2 challenge

# create an object of week 2 data
d2 <- read.csv("week 2 data.csv")

# view dataset
view(d2)

# view names of dataset
names(d2)

# rename variables and add labels dataset
d2 <- d2 %>%
  rename("year"=Date, "acres"=Land) %>%
  mutate(year = set_variable_labels(year, label = "Year"),
         acres = set_variable_labels(acres, label = "Land Owned (acres)"))

# create new variables for log and square root
d2 <- d2 %>%
  as.tibble() %>%
  mutate(ln_acre=log(acres),
         scale=sqrt(ln_acre))

# create a ggplot of week 2 data
ggplot(d2, aes(year, acres)) + 
  stat_smooth(color=usaid_blue,
              alpha=.8) +
  geom_point(color=usaid_blue,
             size=1.8,
             alpha=.6) + 
  scale_x_continuous(breaks=seq(1875, 1900, 5)) +
  scale_y_continuous(breaks=seq(3e5, 1e6, 1e5),
                     labels=label_comma(scale=.001,
                                        suffix="K"),
                     sec.axis=dup_axis()) + 
  labs(x="",
       y="",
       title="Acres of land owned by African-Americans in Georgia",
       caption="This is my awesome figure\nChallenge 2")

# save the ggplot
ggsave(here("week 2 viz.png"),
       device="png",
       type="cairo",
       height=5,
       width=8)





