
ot

frq(dat$Concord)
frq(dat$AttendINLTC)

ot <- dat %>% 
  group_by(Concord) %>% 
  summarise(attend=mean(AttendINLTC),
            n=n())

frq(dat$Concord)

log1 <- glm(AttendINLTC ~ Concord, data=dat, family=binomial)
summary(log1)

Conc_dat <- data.frame(Concord=0:100)

log1_pred <- predict(log1, 
     type="response",
     se.fit=T)

log1_predicted <- data.frame(score=dat$Concord,
                             pred=log1_pred$fit,
                             se=log1_pred$se.fit,
                             attend=dat$AttendINLTC) %>%
  mutate(lower=pred-1.96*se,
         upper=pred+1.96*se) %>%  
  arrange(score)

head(log1_predicted)

ggplot(log1_predicted, aes(score, pred, color=as.factor(attend))) + 
  geom_point(position=position_jitterdodge(jitter.height=.01),
             size=.5,
             alpha=.6) +
  scale_color_manual(values=c(usaid_red, usaid_blue),
                     labels=c("Did not attend",
                              "Attended")) +
  #geom_line(aes(y=upper),
  #          linetype="dotdash",
  #          color="grey60") +
  #geom_line(aes(y=lower),
  #          linetype="dotdash",
  #          color="grey60") +
  scale_x_continuous(limits=c(48,80),
                     breaks=seq(50,80,10)) + 
  scale_y_continuous(limits=c(.55,.75),
                     breaks=seq(.55,.75, .05),
                     labels=percent_format(),
                     sec.axis=dup_axis()) +
  labs(x="\nExamination score (0-100)",
       y="Probability\nof admission",
       title="Entrance examination to Kabul University,\nInternational Legal Training Center") +
  theme(legend.title=element_blank(),
        axis.title.y=element_text(angle=0, vjust=.5),
        axis.title.y.right=element_blank(),
        plot.background = element_rect(fill="aliceblue"),
        panel.background = element_rect(fill="aliceblue"),
        legend.background=element_rect(fill="aliceblue")) 

ggsave("viz/INLTC entrance examination predicted probability alice.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


log1_rangepred <- predict(log1,
                          type="response",
                          se.fit=T,
                          newdata=Conc_dat)

log1_rangepred

log1_rng <- data.frame(score=0:100,
                       pred=predict(log1,
                                    type="response",
                                    newdata=Conc_dat))

log1_rng

                       