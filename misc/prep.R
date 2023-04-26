
# prep

# packages <- c("arm", "BMA", "brms", "corrplot","DescTools", "estimatr","extrafont", "extrafontdb", "janitor", 
#               "reshape2","tidyr","broom", "haven", "HH","Hmisc","lubridate","knitr", "margins", "magrittr", 
#               "plotrix", "scales","survey", "srvyr", "sysfonts", "foreign","car", "ICC", "PowerUpR", "openxlsx", 
#               "ggrepel", "readr", "readxl", "sjmisc", "sjPlot", "sjstats", "sjlabelled", "skimr","labelled", 
#               "texreg", "janitor","psych","dplyr", "tidyverse", "viridis", "here", "ggridges", "ggthemes", 
#               "DT", "jtools", "huxtable", "stringi", "gghighlight", "plm", "rethinking" , "brms", "rstan", 
#               "rstanarm","tidybayes","texreg","gt","gtsummary","huxtable","stargazer", "gsynth", "panelView", 
#               "assertr", "pointblank", "validate", "sandwich")

#lapply(packages, library, character.only=T)

# font_add_google("Source Sans Pro", "sans-serif")


base_packages <- c("tidyverse", "easystats", "corrplot","DescTools","estimatr","extrafont","janitor",
                   "reshape2", "haven", "broom","HH","Hmisc","plotrix","scales","sysfonts","foreign","car",
                   "ICC","openxlsx","readr","readxl","sjmisc","sjPlot","flextable", "sjstats","sjlabelled","skimr",
                   "labelled", "texreg","psych","viridis","here","jtools","huxtable","stringi", "citr", "kableExtra")

lapply(base_packages, library, character.only=T)

viz_packages <- c("patchwork","gganimate","ggstatsplot","ggthemes","ggrepel","ggpubr","cowplot","ggdist","ggtext",
                  "geomtextpath","ggfortify", "ggridges", "gghighlight")
lapply(viz_packages, library, character.only=T)

model_packages <- c("plm","estimatr")
lapply(model_packages, library, character.only=T)


bayes_packages <- c("rstan","rstanarm","brms","rethinking")
lapply(bayes_packages, library, character.only=T)

ie_packages <- c("gsynth", "MatchIt", "did", "bacondecomp")
lapply(ie_packages, library, character.only=T)


map_packages <- c("rgeoboundaries", "ggmap", "sf","rnaturalearth","rnaturalearthdata",
                  "mapview", "spData", "spDataLarge", "tmap", "crsuggest")

lapply(map_packages, library, character.only=T)

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


# read data


