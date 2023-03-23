# LCA demo

# prep ---- 

base_packages <- c("tidyverse", "easystats", "corrplot","DescTools","estimatr","extrafont","janitor",
                   "reshape2", "haven", "broom","HH","Hmisc","plotrix","scales","sysfonts","foreign","car",
                   "ICC","openxlsx","readr","readxl","sjmisc","sjPlot","sjstats","sjlabelled","skimr",
                   "labelled", "texreg","psych","viridis","here","jtools","huxtable","stringi")
lapply(base_packages, library, character.only=T)

viz_packages <- c("patchwork","gganimate","ggstatsplot","ggthemes","ggrepel","ggpubr","cowplot","ggdist","ggtext",
                  "geomtextpath","ggfortify", "ggridges", "gghighlight")
lapply(viz_packages, library, character.only=T)

lca_packages <- c("lcca","depmixS4","poLCA")
lapply(lca_packages, library, character.only=T)

setwd(here("brown bags/2023/Latent Class Analysis"))

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

d <- read_dta("data/atrisk.dta") %>%
  as.data.frame()

head(d)
names(d)
str(d)

varlabs <- lapply(d, function(x) attr(x, "label")) %>%
  unlist()
varlabs

d_des <- data.frame(psych::describe(d)) %>%
  rownames_to_column("var") %>%
  mutate(var_lab = varlabs) %>%
  relocate(var_lab, .after=var)
d_des

mns <- d_des$mean[4:8]
mns

var_key <- data.frame(var_num=1:5,
                      variable=names(d)[4:8],
                      mn=mns)

var_key

lapply(d[,4:8], mean)

# indicators are alcohol use, truancy, vandalism, theft, and weapons use
# two covariates: age and sex

# because we are assuming categorical indicators, we need the indicator variables to start with 1, not 0

vars <- c("alcohol", "truant","vandalism","theft","weapon") 

d2 <- d

for (i in 1:length(vars)){
  d2[,vars[i]] <- d2[,vars[i]]+1
}

head(d2)

f1 <- as.formula(cbind(alcohol, truant, vandalism, theft, weapon) ~1 )

set.seed(5234)

# indicators only ---- 

  # one class

c1 <- poLCA(f1, data=d2, nclass=1)
c1

c1_t <- tidy(c1)
c1_m <- glance(c1)
c1_m

c1$probs

c1_class <- data.frame(model="model_1",
                       classes=1,
                       class=1,
                       prob=1)

c1_class

  # two classes

c2 <- poLCA(f1, data=d2, nclass=2)
c2
c2_t <- tidy(c2) %>%
  arrange(variable, class)
c2_t
c2$predclass

c2_class <- data.frame(model="model_2",
                       classes=2,
                       class=1:2,
                       prob=c(.931, .069))

c2_class

  # three classes

c3 <- poLCA(f1, data=d2, nclass=3, nrep=5, maxiter = 5e3)
c3

c3_class <- data.frame(model="model_3",
                       classes=3,
                       class=1:3,
                       prob=c(.08, .897, .023))

c3_class

c3_predclass <- c3$predclass
str(c3_predclass)
frq(c3_predclass)

  # four classes

c4 <- poLCA(f1, data=d2, nclass=4, nrep=5, maxiter=5e3)
c4 # no convergance

# c4_class <- data.frame(model="model_4",
#                        classes=4,
#                        class=1:4,
#                        prob=c())



# c5 <- poLCA(f1, data=d2, nclass=5)
# c5
# summary(c5)

  # compare models

cs <- list(c1,c2,c3)

model_fits <- map_df(cs, glance) %>%
  mutate(classes=1:3) %>%
  relocate(classes, .before=logLik) %>%
  arrange(AIC)

model_fits

item_probs <- map_df(cs, tidy) %>%
  mutate(estimate=round(estimate, 3),
         model=c(rep("model_1",10), rep("model_2", 20), rep("model_3", 30))) %>%
  filter(outcome==2) %>%
  dplyr::select(-outcome, -std.error) %>%
  pivot_wider(names_from=model,
              values_from=estimate) %>%
  left_join(var_key) %>%
  relocate(var_num, .before=variable) %>%
  relocate(mn, .before=class)

item_probs

item_probs2 <- map_df(cs, tidy) %>%
  mutate(estimate=round(estimate, 3),
         model=c(rep("model_1",10), rep("model_2", 20), rep("model_3", 30))) %>%
  filter(outcome==2) %>%
  dplyr::select(-outcome, -std.error) %>%
  pivot_wider(names_from=variable,
              values_from=estimate) %>%
  mutate(class_probs=c(1,.931,.069,.08,.897, .023))

item_probs2



class_probs <- bind_rows(c1_class, c2_class, c3_class) %>%
  pivot_wider(names_from=model,
              values_from=prob)
class_probs

class_probs2 <- bind_rows(c1_class, c2_class, c3_class) %>%
  pivot_wider(names_from=class,
              values_from=prob)
class_probs2

# indicators with covariates ---- 

names(d2)

f2 <- as.formula(cbind(alcohol, truant, vandalism, theft, weapon) ~ age + male )

c2_1 <- poLCA(f2, data=d2, nclass=1)
c2_1 # 29737


c2_2 <- poLCA(f2, data=d2, nclass=2)
c2_2 # 28773


c2_3 <- poLCA(f2, data=d2, nclass=3)
c2_3 # 28715




