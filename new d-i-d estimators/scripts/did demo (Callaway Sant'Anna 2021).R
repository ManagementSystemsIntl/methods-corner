# did demo

f1 <- lm(stab_std ~ treat + as.factor(wave) + as.factor(village),
         data=m15)
summary(f1)

library(feos)



library(did)

# m15 ---- 

?did
?att_gt

c1 <- att_gt(yname="stab_std",
             tname="wave",
             idname="idname",
             gname="first.treat",
             xformla=~1,
             #xformla= ~ elevation + pop + lang,
             #anticipation=1,
             data=m15)


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
             xformla= ~ elevation + pop + lang,
             #anticipation=1,
             data=m15)


summary(c2)
c2
c1


c3 <- att_gt(yname="stab_std",
             tname="wave",
             idname="idname",
             gname="first.treat",
             #xformla=~1,
             xformla= ~ elevation + pop + lang + region,
             #anticipation=1,
             data=m15)


summary(c3)
c3
c2
c1



c4 <- att_gt(yname="stab_std",
             tname="wave",
             idname="idname",
             gname="first.treat",
             #xformla=~1,
             xformla= ~ elevation + pop + lang,
             #anticipation=1,
             data=m15)


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

ggplot(m15, aes(wave)) + 
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




