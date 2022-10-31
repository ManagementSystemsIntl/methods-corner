# d-i-d estimators applied to MISTI

dat <- read_csv("MISTI/data/MISTI all villages.csv")

names(dat)
head(dat)

# descriptive look at cohorts ---- 
# 
# wvTyp <- dat %>%
#   group_by(wave, type) %>%
#   summarize(stability=mean(stability, na.rm=T)) %>%
#   ungroup()
# 
# wvTyp
# str(wvTyp)
# describe(wvTyp)
# 
# ggplot(wvTyp, aes(wave, stability, color=as.factor(type), group=as.factor(type))) + 
#   scale_x_discrete() + 
#   geom_hline(yintercept=0, size=1, color="darkgrey", alpha=.8) +
#   geom_vline(xintercept=4.5, size=1, color="darkgoldenrod", alpha=.8) +
#   geom_point(size=3) + 
#   geom_line(size=1) +
#   scale_color_viridis_d() +
#   facet_wrap(~type) + 
#   faceted +
#   theme(legend.position="none") +
#   labs(x="",
#        y="",
#        title="Stability in Afghanistan",
#        caption="Across five semi-annual data collection waves, 2011-2014\nStability scaled to mean zero, standard deviation one\nVertical line is presidential election of 2014") +
#   scale_y_continuous(limits=c(-2,1),
#                      breaks=seq(-2,1,.25))
# 
# ggsave("MISTI/viz/Stability by treatment onset.png",
#        type="cairo",
#        device="png",
#        height=4,
#        width=7)

# Callaway and Sant'Anna ----

library(did)

?att_gt

callaway_1 <- att_gt(yname="stability",
                    tname="wave",
                    idname="idname",
                    gname="first.treat",
                    xformla=~dist + elevation + pop + lang,
                    data=dat)

callaway_1
callaway_1$se

callaway_1_out <- data.frame(att=callaway_1$att,
                         se = callaway_1$se,
                         group=rep(2:5,each=4),
                         color=rep(viridis(4), each=4),
                         wave=rep(2:5,4)) %>%
  mutate(type=ifelse(group==2, "Wave 2 treated",
                     ifelse(group==3, "Wave 3 treated",
                            ifelse(group==4, "Wave 4 treated", "Wave 5 treated"))),
         treat=c(1,1,1,1,0,1,1,1,0,0,1,1,0,0,0,1),
         lower=att-1.96*se,
         upper=att+1.96*se,
         xintercept=rep(c(1.8, 2.8, 3.8, 4.8), each=4))

callaway_1_out

treats <- callaway_1_out %>%
  group_by(group) %>%
  summarize(xintercept=mean(xintercept))

treats

ggplot(callaway_1_out, aes(wave, att, color=as.factor(treat))) + #, group=as.factor(group))) + #, color=as.factor(group))) + 
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
  scale_y_continuous(limits=c(-1.044,3.5),
                     breaks=seq(-1,3,1))

ggsave("MISTI/viz/stability, by time treated (callaway with controls).png",
       device="png",
       type="cairo",
       height=6,
       width=7)



callaway_1_dyn <- aggte(callaway_1, type="dynamic")
callaway_1_dyn

callaway_1_dyn_out <- data.frame(time=-3:3,
                           att_dyn = callaway_1_dyn$att.egt,
                           att_dyn_se = callaway_1_dyn$se.egt) %>%
  mutate(lower=att_dyn - 1.96*att_dyn_se,
         upper = att_dyn + 1.96*att_dyn_se,
         col=c(rep("firebrick", 3), rep("darkblue", 4)))

callaway_1_dyn_out


ggplot(callaway_1_dyn_out, aes(x=time, y=att_dyn)) + 
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
  scale_y_continuous(limits=c(-.394,2.35),
                     breaks=seq(-.5,2,.5)) +
  labs(x="Time to treatment",
       y="",
       title="Dynamic treatment effects, Stability",
       caption="Measured in standard deviation units") + 
  guides(color=guide_legend(reverse=T))


ggsave("MISTI/viz/stability, dynamic treatment effects (Callaway with controls).png",
       device="png",
       type="cairo",
       height=5,
       width=7)


callaway_1_grp <- aggte(callaway_1, type="group")

callaway_1_grp

callaway_1_out_grp <- data.frame(
  cohort=c("Wave 2 treated","Wave 3 treated","Wave 4 treated","Wave 5 treated", "All waves treated"),
  att_grp = c(callaway_1_grp$att.egt, callaway_1_grp$overall.att),
  att_grp_se = c(callaway_1_grp$se.egt, callaway_1_grp$overall.se)) %>%
  mutate(lower=att_grp - 1.96*att_grp_se,
         upper = att_grp + 1.96*att_grp_se)

callaway_1_out_grp


ggplot(callaway_1_out_grp, aes(att_grp, fct_rev(cohort))) + #, group=as.factor(group))) + #, color=as.factor(group))) + 
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


ggsave("MISTI/viz/stability, by cohort (Callaway with controls).png",
       device="png",
       type="cairo",
       height=5,
       width=7)





