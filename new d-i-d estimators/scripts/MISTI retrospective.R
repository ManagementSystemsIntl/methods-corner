
source("scripts/prep.r")

dat <- read_csv("data/W1-5 long.csv")

names(dat)

class(dat$Wave)
frq(dat$Wave)

# treatment matrix

dat2 <- dat %>%
  filter(!(Wave==1 & treatment==1),
         !(Wave==2 & treatment==1)) %>%
  select(village=1, 
         wave=2,
         treat=3, 
         SI2,
         district=district.x,
         province,
         region,
         dist,
         elevation,
         pop,
         lang,
         ethnicity,
         SI2,
         RI2,
         SIKA,
         Project) %>%
  mutate(stability=scale(SI2),
         resilience=scale(RI2),
         treat2=ifelse(lag(treat)==1, 1, treat),
         treat2 = ifelse(is.na(treat2), treat, treat2),
         vil_wave=paste(village, wave)) %>%
  na.omit() %>%
  group_by(village) %>%
  mutate(treat3=cumsum(treat)) %>%
  ungroup() %>%
  relocate(treat2, .after=treat) %>%
  relocate(treat3, .after=treat2) %>%
  arrange(village, wave, desc(treat)) %>%
  as.data.frame()

dat2
str(dat2)
names(dat)

       




l1 <- lm(stability ~ treat, dat2)

summary(l1)

l2 <- lm(stability ~ treat + as.factor(wave) + as.factor())

?plm

p0 <- plm(stability ~ treat,
          model="pooling",
          data=dat2)

summary(p0)

p1 <- plm(stability ~ treat, 
          index=c("village","wave"),
          effect="twoways",
          model="within",
          data=dat2)

summary(p1)


dat2
str(dat2)

dat3 <- dat2 %>%
  select(village, wave, treat, stability, resilience) %>%
  group_by(village) %>%
  mutate(treat2=ifelse(lag(treat)==1, 1, treat),
         treat2 = ifelse(is.na(treat2), treat, treat2),
         diff=ifelse(treat != treat2, 1,0)) %>%
  ungroup() %>%
  na.omit() %>%
  arrange(village, wave)

dat3



wave <- dat3 %>%
  select(-(4:7)) %>%
  pivot_wider(names_from=wave,
              values_from=treat) %>%
  na.omit() %>%
  select(1, 4, everything())

wave

wave2 <- dat3 %>%
  select(1,2,6) %>%
  pivot_wider(names_from=wave,
              values_from=treat2) %>%
  na.omit() %>%
  select(1, 4, everything())

wave2

wave3 <- dat3 %>%
  select(1,2,6) %>%
  pivot_wider(names_from=wave,
              values_from=treat2) %>%
  #na.omit() %>%
  select(1, 4, everything())

wave3



write_csv(wave2, "output/MISTI W1-5 treatment raw matrix.csv")


out2 <- wave2 %>%
  pivot_longer(2:6,
               names_to="wave",
               values_to="treat")

out2 # has villages that are measured all five waves, treatments in wave 1-2 dropped

treated <- out2 %>%
  group_by(wave) %>%
  summarize(treat=sum(treat),
            comparison = 502-treat)


treated

frq(out2$wave)

colSums(wave2[,2:6])

frq(dat$Total.Activities)
frq(dat$m6)
describe(dat$dist)

names(dat)

si2 <- dat2 %>%
  select(village=1, 
         Wave=wave,
         district,
         province,
         region,
         dist,
         elevation,
         pop,
         lang,
         ethnicity,
         SI2,
         SIKA,
         Project) %>%
  mutate(stability=scale(SI2),
         vil_wave=paste(village, wave)) %>%
  select(-village, -Wave)

names(si2)
si2
describe(si2[,2:3])

out2

dat2 <- dat2 %>%
  select(-village, -wave, -treat)

out3 <- out2 %>%
  mutate(vil_wave = paste(village, wave)) %>%
  left_join(dat2, by="vil_wave") %>%
  group_by(village) %>%
  mutate(type = case_when(sum(treat)==0 ~ "Never treated",
                          sum(treat)==1 ~ "Wave 5 treated",
                          sum(treat)==2 ~ "Wave 4 treated",
                          sum(treat)==3 ~ "Wave 3 treated",
                          sum(treat)==4 ~ "Wave 2 treated",
                          sum(treat)==5 ~ "Wave 1 treated"),
         first.treat = case_when(sum(treat)==0 ~ 0,
                                   sum(treat)==1 ~ 5,
                                   sum(treat)==2 ~ 4,
                                   sum(treat)==3 ~ 3,
                                   sum(treat)==4 ~ 2,
                                   sum(treat)==5 ~ 1),
         wave=as.numeric(wave),
         idname=factor(village),
         idname=as.numeric((idname))) %>%
  na.omit() %>%
  as.data.frame()

write_csv(out3, "data/MISTI all villages.csv")

out3 <- read_csv("data/MISTI all villages.csv")

test <- read_csv("data/MISTI all villages.csv")

names(out3)
str(out3)
head(out3)
frq(out3$type)
frq(out3$first.treated)

wvTyp <- out3 %>%
  group_by(wave, type) %>%
  summarize(stability=mean(stability, na.rm=T),
            resilience=mean(resilience, na.rm=T),
            n=n())

wvTyp <- dat2 %>%
  group_by(wave, type) %>%
  summarize(stability=mean(stability, na.rm=T))



wvTyp

ggplot(wvTyp, aes(wave, stability, color=as.factor(type), group=as.factor(type))) + 
  geom_point() + 
  geom_line() +
  scale_color_viridis_d()

ggplot(wvTyp, aes(wave, stability, color=as.factor(type), group=as.factor(type))) + 
  scale_x_discrete() + 
  geom_hline(yintercept=0, size=1, color="darkgrey", alpha=.8) +
  geom_vline(xintercept=4.5, size=1, color="darkgoldenrod", alpha=.8) +
  geom_point(size=3) + 
  geom_line(size=1) +
  scale_color_viridis_d() +
  facet_wrap(~type) + 
  faceted +
  theme(legend.position="none") +
  labs(x="",
       y="",
       title="Stability in Afghanistan",
       caption="Across five semi-annual data collection waves, 2011-2014\nStability scaled to mean zero, standard deviation one\nVertical line is presidential election of 2014") +
  scale_y_continuous(limits=c(-.5,1.5),
                     breaks=seq(-.5,1.5,.5)) 
# +
#   geom_point(aes(y=resilience), size=3) + 
#   geom_line(aes(y=resilience), size=1)

ggsave("viz/Stability by treatment onset.png",
       type="cairo",
       device="png",
       height=4,
       width=6)


ggplot(out3, aes(wave, stability, color=as.factor(type))) + 
  geom_point() + 
  stat_smooth() + 
  scale_color_viridis_d()

wvTyp <- out3 %>%
  group_by(wave, type)


l1 <- lm(stability ~ treat, out3)

summary(l1)


l2 <- lm(stability ~ treat + as.factor(wave), out3)

summary(l2)

?lm_robust

r1 <- lm_robust(stability ~ treat, 
                clusters=village,
                fixed_effects=wave,
                data=out3)

summary(r1)

names(out3)

r2 <- lm_robust(stability ~ treat + dist + elevation + pop + lang, 
                clusters=village,
                fixed_effects=wave + village,
                data=out3)

summary(r2)



r3 <- lm_robust(stability ~ treat + dist + elevation + pop + Project + treat:Project, 
                clusters=village,
                fixed_effects=region,
                data=out3)

summary(r3)




l3 <- lm(stability ~ treat + as.factor(wave) + treat:as.factor(wave), out3)

summary(l3)





# w2 ----

w2 <- out3 %>%
  filter(type=="Wave 2 treated")

w2

ggplot(w2, aes(x=wave, y=stability)) + 
  scale_x_discrete() +
  geom_vline(xintercept=1.5, color="darkgoldenrod", size=1.2, alpha=.8) +
  geom_point(aes(color=village), size=3) +
  stat_smooth(aes(group=village, color=as.factor(village)), size=1.2, span=.4, method="lm", se=F) +
  stat_smooth(color="dodgerblue", size=4, span=.8, method="lm", se=F) + 
  scale_color_viridis_d(option="D")

?scale_color_viridis


r1_w2 <- lm_robust(stability ~ treat + dist + elevation + pop + Project,
                   clusters=village,
                   #fixed_effects=region,
                   data=out3)

summary(r1_w2)



# w1-2

w1_2 <- out3 %>%
  filter(wave<3)


w2L <- w2 %>%
  select(village, wave, treat, stability) %>%
  pivot_wider(names_from=wave,
              values_from=stability)



library(did)
?att_gt

names(out3)

out4 <- out3 %>%
  group_by(village) %>%
  mutate(first.treat=first(treat==1))

out4

names(out3)
frq(out3$village)

out3 %>% nrow(distinct(., village))

nrow(distinct(out3, village))

479*5

?distinct

# Callaway and Sant'Anna ---- 

head(out3)
str(out3)

callway_1 <- att_gt(yname="stability",
                    tname="wave",
                    idname="idname",
                    gname="first.treat",
                    xformla=~1,
                    #anticipation=1,
                    data=out3)


summary(callway_1)
callway_1
$W


call_out_1 <- data.frame(att=callway_1$att,
                         se = callway_1$se,
                         group=rep(2:5,each=4),
                         color=rep(viridis(4), each=4),
                         wave=rep(2:5,4)) %>%
  mutate(type=ifelse(group==2, "Wave 2 treated",
                     ifelse(group==3, "Wave 3 treated",
                            ifelse(group==4, "Wave 4 treated", "Wave 5 treated"))),
         treat=c(1,1,1,1,0,1,1,1,0,0,1,1,0,0,0,1),
         lower=att-2.6*se,
         upper=att+2.6*se,
         xintercept=rep(c(1.8, 2.8, 3.8, 4.8), each=4))

call_out_1

treats <- call_out_1 %>%
  group_by(group) %>%
  summarize(xintercept=mean(xintercept))

treats

ggplot(call_out_1, aes(wave, att, color=as.factor(treat))) + #, group=as.factor(group))) + #, color=as.factor(group))) + 
  geom_hline(yintercept=0, color="darkgoldenrod", size=1, alpha=.8) +
  #geom_vline(data=treats, aes(xintercept=xintercept), color="darkgrey", size=1, alpha=.8) +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, size=1) +
  geom_label(aes(label=round(att,2)),
             show.legend=F) +
  scale_color_manual(values=c("firebrick","darkblue"),
                     labels=c("Comparison","Treatment")) +
  facet_wrap(~type, scales="free") +
  faceted +
  theme(legend.position="bottom",
        legend.title=element_blank()) + 
  labs(x="", 
       y="",
       title="Change in stability, by time treated") + 
  scale_x_continuous(limits=c(1.8, 5.2),
                     breaks=1:5) +
  scale_y_continuous(limits=c(-1.4,3.95),
                     breaks=seq(-1,4,1))

ggsave("viz/stability, by time treated (callaway).png",
       device="png",
       type="cairo",
       height=6,
       width=7)

a <- ggdid(callway_1)
a

es <- aggte(callway_1, type="dynamic")
es
summary(es)

b <- ggdid(es)

b 

call_out_dyn <- data.frame(time=-3:3,
                           att_dyn = es$att.egt,
                           att_dyn_se = es$se.egt) %>%
  mutate(lower=att_dyn - 1.96*att_dyn_se,
         upper = att_dyn + 1.96*att_dyn_se,
         col=c(rep("firebrick", 3), rep("darkblue", 4)))

call_out_dyn

ggplot(call_out_dyn, aes(x=time, y=att_dyn)) + 
  geom_hline(yintercept=0, size=1, color="darkgrey", alpha=.6) +
  geom_point(aes(color=time<0)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color=time<0), width=0, size=1) +
  geom_label(aes(label=round(att_dyn,2), color=time<0),
             show.legend=F) +
  scale_color_manual(values=c("TRUE"="firebrick","FALSE" = "darkblue"),
                     labels=c("Treatment", "Comparison")) +
  theme(legend.title=element_blank(),
        legend.position="bottom") +
  scale_x_continuous(breaks=-3:3) +
  scale_y_continuous(limits=c(-.5,2)) +
  labs(x="Time to treatment",
       y="",
       title="Dynamic treatment effects, Stability",
       caption="Measured in standard deviation units") + 
  guides(color=guide_legend(reverse=T))


ggsave("viz/stability, dynamic treatment effects (callaway).png",
       device="png",
       type="cairo",
       height=5,
       width=7)

  

  

grp <- aggte(callway_1, type="group")

grp

ggdid(grp)


call_out_grp <- data.frame(
  cohort=c("Wave 2 treated","Wave 3 treated","Wave 4 treated","Wave 5 treated", "All waves treated"),
  att_grp = c(grp$att.egt,grp$overall.att),
  att_grp_se = c(grp$se.egt, grp$overall.se)) %>%
  mutate(lower=att_grp - 1.96*att_grp_se,
         upper = att_grp + 1.96*att_grp_se)


ggplot(call_out_grp, aes(att_grp, fct_rev(cohort))) + #, group=as.factor(group))) + #, color=as.factor(group))) + 
  geom_vline(xintercept=0, color="darkgrey", size=1.2, alpha=.8) +
  geom_point(size=3) + 
  geom_errorbar(aes(xmin=lower, xmax=upper), width=0, size=1, color="darkblue") +
  geom_label(aes(label=round(att_grp,2)),
             show.legend=F, color="darkblue") +
  theme(legend.position="bottom",
        legend.title=element_blank()) + 
  labs(x="", 
       y="",
       title="Change in stability, by cohort",
       caption="Measured in standard deviation units") + 
  scale_x_continuous(limits=c(-1, 3),
                     breaks=seq(-1,3,.5))


ggsave("viz/stability, by cohort (callaway).png",
       device="png",
       type="cairo",
       height=5,
       width=7)


         

#         col=c(rep("firebrick", 3), rep("darkblue", 4)))

call_out_grp




data(mpdta)
mp <- mpdta


# with covariates
out1 <- att_gt(yname="lemp",
               tname="year",
               idname="countyreal",
               gname="first.treat",
               xformla=~lpop,
               data=mpdta)
summary(out1)

# without covariates
out2 <- att_gt(yname="lemp",
               tname="year",
               idname="countyreal",
               gname="first.treat",
               xformla=NULL,
               data=mpdta)
summary(out2)


library(DIDmultiplegt)

?did_multiplegt 

aer1 <- did_multiplegt(out3, 
                       Y="stability",
                       G = "village",
                       T = "wave",
                       D = "treat",
                       brep=50,
                       placebo=3,
                       dynamic=3)
,
                       trends_lin = "village")

summary(aer1)
aer1
aer1[1]
aer1$

frq(out3$village)


library(bacondecomp)

b1 <- bacon(stability ~ treat,
            data=out3,
            id_var="village",
            time_var="wave")


library(HonestDiD)

# TWFE with heterogeneity (de Chaisemartin D'Haultfoeuille) ---- 

library(TwoWayFEWeights)

?twowayfeweights

callway_1 <- att_gt(yname="stability",
                    tname="wave",
                    idname="idname",
                    gname="first.treat",
                    xformla=~1,
                    #anticipation=1,
                    data=out3)

names(out3)

twfe1 <- twowayfeweights(out3, 
                         Y="stability",
                         G = "village",
                         T = "wave",
                         D = "treat",
                         cmd_type="feTR")

twfe1
summary(twfe1)
describe(twfe1$weight)




