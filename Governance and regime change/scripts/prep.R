# prep

packages <- c("arm", "BMA", "brms", "corrplot", "dummies","DescTools", "estimatr","extrafont", "extrafontdb", 
              "janitor", "reshape2","tidyr","broom", "haven", "HH","Hmisc","lubridate","knitr", "margins", 
              "magrittr", "plotrix", "scales","survey", "srvyr", "sysfonts", "foreign","car", "ICC",  
              "openxlsx", "ggrepel", "readr", "readxl", "sjmisc", "sjPlot", "sjstats", "sjlabelled", "skimr",
              "labelled", "texreg", "janitor","psych","dplyr", "tidyverse", "viridis", "here", "ggridges",
              "ggthemes", "DT", "jtools", "huxtable", "stringi", "gghighlight", "plm", "rethinking" , "brms", 
              "rstan", "rstanarm","tidybayes","texreg","gt","gtsummary","huxtable","stargazer", "gsynth", 
              "panelView", "assertr", "validate", "sandwich", "zoo")

lapply(packages, library, character.only=T)

sysfonts::font_add_google("Open Sans", "sans-serif")

options(brms.backend="cmdstanr")

options(digits=3, scipen=6)

# set default
base <- theme_bw() + theme(panel.grid.minor.x=element_blank(),
                           panel.grid.minor.y=element_blank(),
                           plot.title=element_text(face="bold",size=18, hjust=.5, family = "Gill Sans Mt"),
                           plot.subtitle = element_text(size=16, family="Gill Sans Mt"),
                           plot.caption=element_text(size=12, family="Gill Sans Mt"),
                           axis.title=element_text(size=16, family="Gill Sans Mt"),
                           axis.text=element_text(size=14, family="Gill Sans Mt"),
                           legend.text=element_text(size=14, family="Gill Sans Mt"),
                           strip.text=element_text(size=14, family="Gill Sans Mt"),
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