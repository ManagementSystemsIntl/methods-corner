# Du Bois Visualization Challenge
# Week 1

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

# now we can start with the week 1 challenge

# create an object of week 1 data
d <- read.csv("week 1 data.csv")

# view dataset
view(d)

# view names of dataset
names(d)

# rename variables of dataset
d <- d %>% 
  rename(year="Year", value="Land.Value..Dollars.")

# create a ggplot of week 1 data
ggplot(d, aes(year, value)) + 
  geom_point(color=usaid_blue,
             size=2.4) + 
  geom_line(color=usaid_blue,
            alpha=.7) +
  scale_y_continuous(label=dollar_format(scale=.000001, suffix="M"),
                     sec.axis=dup_axis()) + 
  labs(x="",
       y="",
       title="Value of land owned by African-Americans in Georgia",
       caption="This is my awesome figure")

# save the ggplot
ggsave(here("week 1 viz.png"),
       device="png",
       type="cairo",
       height=5,
       width=8)
