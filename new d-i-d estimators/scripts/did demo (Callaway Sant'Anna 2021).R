# did demo

f1 <- lm(stab_std ~ treat + as.factor(wave) + as.factor(village),
         data=m15)
summary(f1)

library(feos)



library(did)

# mistifull ---- 

?conditional_did_pretest

pre <- conditional_did_pretest(yname="stab_std",
                                tname="wave",
                                idname="idname",
                                gname="first.treat",
                                xformla=~1,
                                #xformla= ~ elevation + pop + lang,
                                #anticipation=1,
                                data=mistifull)

c1 <- att_gt(yname="stab_std",
             tname="wave",
             idname="idname",
             gname="first.treat",
             #xformla=~1,
             xformla= ~ elevation + pop + lang,
             #anticipation=1,
             data=mistifull)


summary(c1)
c1


c1.1 <- att_gt(yname="stab_std",
             tname="wave",
             idname="idname",
             gname="first.treat",
             xformla=~1,
             control_group = "notyettreated",
             data=mistifull)


summary(c1.1)
c1




# m15 ---- 

?did
?att_gt
?aggte

c1 <- att_gt(yname="stab_std",
             tname="wave",
             idname="idname",
             gname="first.treat",
             xformla=~1,
             #xformla= ~ elevation + pop + lang,
             #anticipation=1,
             data=mistifull)


summary(c1)
c1

c1.1 <- att_gt(yname="stab_std",
             tname="wave",
             idname="idname",
             gname="first.treat",
             xformla=~1,
             #xformla= ~ elevation + pop + lang,
             anticipation=1,
             data=m15)


summary(c1.1)
c1.1




c2 <- att_gt(yname="stab_std",
             tname="wave",
             idname="idname",
             gname="first.treat",
             #xformla=~1,
             xformla= ~ nsp,
             #anticipation=1,
             data=mistifull)


summary(c2)
c2
c1


c3 <- att_gt(yname="stab_std",
             tname="wave",
             idname="idname",
             gname="first.treat",
             #xformla=~1,
             xformla= ~ nsp + ln_dist,
             #anticipation=1,
             data=mistifull)


summary(c3)
c3
c2
c1


c3_out <- data.frame(att=c3$att,
                     se = c3$se,
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

c3_out

ggplot(c3_out, aes(wave, att, color=as.factor(treat))) + 
  geom_hline(yintercept=0, color="darkgoldenrod", size=1, alpha=.8) +
  geom_point(size=3) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, size=1) +
  geom_label(aes(label=round(att,2)),
             show.legend=F) +
  scale_color_manual(values=c("firebrick","darkblue"),
                     labels=c("Untreated","Treated")) +
  facet_wrap(~type, scales="free") +
  faceted +
  theme(legend.position="bottom",
        legend.title=element_blank()) + 
  labs(x="", 
       y="",
       title="Change in stability, by time treated",
       caption="Callaway Sant'Anna did
       Outcomes in standard deviation units") + 
  scale_x_continuous(limits=c(1.8, 5.2),
                     breaks=1:5) +
  scale_y_continuous(limits=c(-1.4,3.95),
                     breaks=seq(-1,4,1))

ggsave("viz/Callaway did/stability, by time treated (Callaway c3) 2.png",
       device="png",
       type="cairo",
       height=6,
       width=7)


c3.1 <- att_gt(yname="stab_std",
             tname="wave",
             idname="idname",
             gname="first.treat",
             #xformla=~1,
             xformla= ~ nsp + ln_dist,
             control_group = "notyettreated",
             data=mistifull)


summary(c3.1)
c3
c2
c1


c3_simple <- aggte(c3, type="simple")

c3_dyn <- aggte(c3, type="dynamic")
c3_dyn

c3_dyn_out <- data.frame(time=-3:3,
                           att_dyn = c3_dyn$att.egt,
                           att_dyn_se = c3_dyn$se.egt) %>%
  mutate(lower=att_dyn - 1.96*att_dyn_se,
         upper = att_dyn + 1.96*att_dyn_se,
         col=c(rep("firebrick", 3), rep("darkblue", 4)))

c3_dyn_out

ggplot(c3_dyn_out, aes(x=time, y=att_dyn)) + 
  geom_hline(yintercept=0, size=1, color="darkgrey", alpha=.6) +
  geom_point(aes(color=time<0)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper, color=time<0), width=0, size=1) +
  geom_label(aes(label=round(att_dyn,2), color=time<0),
             show.legend=F) +
  scale_color_manual(values=c("TRUE"="firebrick","FALSE" = "darkblue"),
                     labels=c("Not yet treated", "Treatment")) +
  theme(legend.title=element_blank(),
        legend.position="bottom") +
  scale_x_continuous(breaks=-3:3) +
  scale_y_continuous(limits=c(-.5,1.5)) +
  labs(x="Time to treatment",
       y="",
       title="Dynamic treatment effects, Stability",
       caption="Measured in standard deviation units
       Callaway Sant'Anna did") 
+ 
  guides(color=guide_legend(reverse=T))


ggsave("viz/Callaway did/stability, dynamic treatment effects (callaway).png",
       device="png",
       type="cairo",
       height=5,
       width=7)

c3_grp <- aggte(c3.1, type="group")
c3_grp


c3_grp_out <- data.frame(
  cohort=c("Wave 2 treated","Wave 3 treated","Wave 4 treated","Wave 5 treated", "All waves treated"),
  att_grp = c(c3_grp$att.egt,c3_grp$overall.att),
  att_grp_se = c(c3_grp$se.egt, c3_grp$overall.se)) %>%
  mutate(lower=att_grp - 1.96*att_grp_se,
         upper = att_grp + 1.96*att_grp_se)


ggplot(c3_grp_out, aes(att_grp, fct_rev(cohort))) + #, group=as.factor(group))) + #, color=as.factor(group))) + 
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
       caption="Measured in standard deviation units
       Callaway Sant'Anna did") + 
  scale_x_continuous(limits=c(-1, 1.5),
                     breaks=seq(-1,1.5,.5))


ggsave("viz/Callaway did/stability, by cohort (callaway).png",
       device="png",
       type="cairo",
       height=5,
       width=7)

c3_cal <- aggte(c3, type="calendar")
c3_cal

cal_cal_out <- data.frame(
  cohort=c("Wave 2","Wave 3","Wave 4","Wave 5", "All waves"),
  att_grp = c(cal_cal$att.egt,cal_cal$overall.att),
  att_grp_se = c(cal_cal$se.egt, cal_cal$overall.se)) %>%
  mutate(lower=att_grp - 1.96*att_grp_se,
         upper = att_grp + 1.96*att_grp_se)

cal_cal_out


ggplot(cal_cal_out, aes(att_grp, fct_rev(cohort))) + #, group=as.factor(group))) + #, color=as.factor(group))) + 
  geom_vline(xintercept=0, color="darkgrey", size=1.2, alpha=.8) +
  geom_point(size=3) + 
  geom_errorbar(aes(xmin=lower, xmax=upper), width=0, size=1, color="darkblue") +
  geom_label(aes(label=round(att_grp,2)),
             show.legend=F, color="darkblue") +
  theme(legend.position="bottom",
        legend.title=element_blank()) + 
  labs(x="", 
       y="",
       title="Change in stability, by calendar time",
       caption="Measured in standard deviation units
       Callaway Sant'Anna did") + 
  scale_x_continuous(limits=c(-.5, 1.5),
                     breaks=seq(-.5,1.5,.5))




ot <- mistifull %>%
  group_by(type, wave) %>%
  summarize(stab=mean(stab_std),
            se=std.error(stab_std)) %>%
  mutate(lower=stab-se*1.96,
         upper=stab+se*1.96)

ggplot(ot, aes(x=wave, y=stab, color=type)) + 
  geom_point() + 
  geom_line() + 
  scale_color_viridis_d()

ggplot(ot, aes(x=wave, y=stab, color=type)) + 
  geom_point() + 
  geom_line() + 
  scale_color_viridis_d() + 
  facet_wrap(~type) + 
  faceted



c4 <- att_gt(yname="stab_std",
             tname="wave",
             idname="idname",
             gname="first.treat",
             #xformla=~1,
             xformla= ~ nsp + ln_dist + ln_elevation,
             #anticipation=1,
             data=mistifull)

names(mistifull)
summary(c4)
c4
c2
c1

c4_out <- data.frame(att=c4$att,
                         se = c4$se,
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

c4_out


c5 <- att_gt(yname="stab_std",
             tname="wave",
             idname="idname",
             gname="first.treat",
             #xformla=~1,
             xformla= ~ nsp + ln_dist + ln_elevation + ln_pop,
             #anticipation=1,
             data=mistifull)

names(mistifull)
summary(c5)
c5


# m25 ---- 

c25_1 <- att_gt(yname="stab_std",
             tname="wave",
             idname="idname",
             gname="first.treat",
             xformla=~1,
             #xformla= ~ elevation + pop + lang,
             #anticipation=1,
             data=m25)


summary(c25_1)
c25_1


c25_2 <- att_gt(yname="stab_std",
                tname="wave",
                idname="idname",
                gname="first.treat",
                #xformla=~1,
                xformla= ~ elevation + pop,
                #anticipation=1,
                data=m25)


summary(c25_2)
c25_1

m15 %>%
  distinct(village, .keep_all = T) 

%>%
  frq(cohort)

  group_by(village, cohort) %>%
  tally()

frq(m15$cohort)
frq(m15$treated)

ggplot(mistifull, aes(wave)) + 
  geom_line(aes(y=stab_std, group=village, color=as.factor(treat_event)),
                size=.3, alpha=.3) +
  facet_wrap(~region) +
  faceted +
  scale_color_manual(values=c("firebrick2", "dodgerblue2"), labels=c("Comparison","Treatment")) +
  theme(legend.position="bottom",
        legend.title=element_blank()) +
  stat_smooth(aes(y=stab_std, color=as.factor(treat_event)),
              se=F, size=2)


ggplot(m15, aes(wave)) + 
  geom_line(aes(y=stab_std, group=village, color=as.factor(treat_event)),
            size=.2, alpha=.2) +
  scale_color_manual(values=c("firebrick2", "dodgerblue2"), labels=c("Comparison","Treatment")) +
  theme(legend.position="bottom",
        legend.title=element_blank()) +
  stat_smooth(aes(y=stab_std, color=as.factor(treat_event)))




