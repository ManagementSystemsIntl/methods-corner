# statistical power explore

library(pwr)
library(WebPower)
library(Superpower)


# repeated measures anova ---- 

?WebPower::wp.rmanova

bet <- wp.rmanova(n=300,
                ng=3,
                nm=2,
                f=.2,
                type=0)

bet
str(bet)

between <- data.frame(id=1:120,
                  type="between",
                  n=seq(5,600,5),
                  power=NA)

between

for(i in 1:120) {
  temp <- wp.rmanova(n=between$n[i],
                     ng=3,
                     nm=2,
                     f=.2,
                     type=0)
  between$power[i] <- temp$power
}

between

ggplot(between, aes(x=n, y=power)) + 
  #stat_smooth(se=F) +
  geom_line(color="dodgerblue2",
            size=1) +
  scale_x_continuous(limits=c(0,600),
                     breaks=seq(0,600,100)) +
  scale_y_continuous(limits=c(0,1),
                     breaks=seq(0,1,.2),
                     labels=percent) +
  labs(x="\nSample size",
       y="Power",
       title="Statistical power for repeated measures ANOVA
       Between subjects effect") +
  theme(axis.title.y=element_text(angle=0, vjust=.5)) +
  geom_segment(aes(x=0, xend=243, y=.8, yend=.8),
               size=1, color="dodgerblue2", linetype="dotdash", alpha=.5) +
  geom_segment(aes(x=245, xend=243, y=0, yend=.8),
               size=1, color="dodgerblue2", linetype="dotdash", alpha=.5) 
  

between


within <- data.frame(id=1:120,
                      type="within",
                      n=seq(5,600,5),
                      power=NA)

for(i in 1:120) {
  temp <- wp.rmanova(n=within$n[i],
                     ng=3,
                     nm=2,
                     f=.2,
                     type=1)
  within$power[i] <- temp$power
}

within
sqrt(.25/400)

ggplot(within, aes(x=n, y=power)) + 
  #stat_smooth(se=F) +
  geom_line(color="dodgerblue2",
            size=1) +
  scale_x_continuous(limits=c(0,600),
                     breaks=seq(0,600,100)) +
  scale_y_continuous(limits=c(0,1),
                     breaks=seq(0,1,.2),
                     labels=percent) +
  labs(x="\nSample size",
       y="Power",
       title="Statistical power for repeated measures ANOVA
       Within subjects effect") +
  theme(axis.title.y=element_text(angle=0, vjust=.5)) +
  geom_segment(aes(x=0, xend=197, y=.8, yend=.8),
               size=1, color="dodgerblue2", linetype="dotdash", alpha=.5) +
  geom_segment(aes(x=197, xend=197, y=0, yend=.8),
               size=1, color="dodgerblue2", linetype="dotdash", alpha=.5) 



interact <- data.frame(id=1:120,
                     type="interaction",
                     n=seq(5,600,5),
                     power=NA)

for(i in 1:120) {
  temp <- wp.rmanova(n=interact$n[i],
                     ng=2,
                     nm=2,
                     f=.2,
                     type=2)
  interact$power[i] <- temp$power
}

interact
sqrt(.25/400)

ggplot(interact, aes(x=n, y=power)) + 
  #stat_smooth(se=F) +
  geom_line(color="dodgerblue2",
            size=1) +
  scale_x_continuous(limits=c(0,600),
                     breaks=seq(0,600,100)) +
  scale_y_continuous(limits=c(0,1),
                     breaks=seq(0,1,.2),
                     labels=percent) +
  labs(x="\nSample size",
       y="Power",
       title="Statistical power for repeated measures ANOVA
       Interaction effect") +
  theme(axis.title.y=element_text(angle=0, vjust=.5)) +
  geom_segment(aes(x=0, xend=242, y=.8, yend=.8),
               size=1, color="dodgerblue2", linetype="dotdash", alpha=.5) +
  geom_segment(aes(x=242, xend=242, y=0, yend=.8),
               size=1, color="dodgerblue2", linetype="dotdash", alpha=.5) 





