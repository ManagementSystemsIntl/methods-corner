# prep

packages <- c("arm", "BMA", "brms", "corrplot", "dummies","DescTools", "estimatr","extrafont", "extrafontdb", "janitor", 
              "reshape2","tidyr","broom", "haven", "HH","Hmisc","lubridate","knitr", "margins", "magrittr", "plotrix", 
              "scales","survey", "srvyr", "sysfonts", "foreign","car", "ICC", "openxlsx", "ggrepel", "readr", "readxl", 
              "sjmisc", "sjPlot", "sjstats", "sjlabelled", "skimr","labelled", "texreg", "janitor","psych","dplyr", 
              "tidyverse", "tidylog", "viridis", "here", "ggridges", "ggthemes", "DT", "jtools", "huxtable", "stringi", "gghighlight", 
              "plm", "texreg","gt","gtsummary","huxtable","stargazer", "panelView", "assertr", "pointblank", "validate", 
              "sandwich", "here") 

lapply(packages, library, character.only=T)

democracyPackages <- c("democracyData", "vdem", "vdemdata")
lapply(packages, library, character.only=T)

iePackages <- c("gsynth", "MatchIt")
lapply(iePackages, library, character.only=T)

bayesPackages <- c("rethinking","brms","rstanarm","tidybayes","cmdstanr")
lapply(bayesPackages, library, character.only=T)

didPackages <- c("bacondecomp", "staggered","pretrends","HonestDiD", "did", "gsynth", "panelView", "didimputation", 
                 "synthdid", "TwoWayFEWeights","fixest", "DRDID", "DIDmultiplegt", "did2s", "fect")
lapply(didPackages, library, character.only=T)

setwd(here("new d-i-d estimators"))

# font_add_google("Source Sans Pro", "sans-serif")

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
        plot.title=element_text(face="bold",size=18, hjust=.5, family = "Gill Sans Mt"),
        plot.subtitle = element_text(size=16, family="Gill Sans Mt"),
        plot.caption=element_text(size=12, family="Gill Sans Mt"),
        axis.title=element_text(size=16, family="Gill Sans Mt"),
        axis.text=element_text(size=14, family="Gill Sans Mt"),
        legend.text=element_text(size=14, family="Gill Sans Mt"),
        strip.text=element_text(size=14, family="Gill Sans Mt"))



facet_style <- function(){theme_bw() +
    theme(panel.grid.minor.x=element_blank(),
          panel.grid.minor.y=element_blank(),
          plot.title=element_text(face="bold",size=18, hjust=.5, family = "Gill Sans Mt"),
          plot.subtitle = element_text(size=16, family="Gill Sans Mt"),
          plot.caption=element_text(size=12, family="Gill Sans Mt"),
          axis.title=element_text(size=16, family="Gill Sans Mt"),
          axis.text=element_text(size=14, family="Gill Sans Mt"),
          legend.text=element_text(size=14, family="Gill Sans Mt"),
          strip.text=element_text(size=14, family="Gill Sans Mt"))
}


