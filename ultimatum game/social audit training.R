
setwd("C:/Egnyte/Private/dkillian/Watchdog-Social Audit training/slide decks")

?read_excel
ult <- read_excel("AMANAT CRC Training Workbook.xlsx", sheet="Icebreaker", range="D1:E19") %>%
  mutate(Decision=as.factor(Decision))
str(ult)
ult

ggplot(ult, aes(x=Decision, y = Offer)) + geom_boxplot()

boxplot(ult)

library(yarrr)
?pirateplot
pirateplot(Offer ~ Decision, ult, main="Average amount offered\namong Acceptors and Rejectors",
           theme=4,
           point.cex=1.2,
           bean.lwd=.2)

bean.lwd, bean.lty, inf.lwd, avg.line.lwd, bar.lwd

describe(ult)

describeBy(ult, ult$Decision)


d <- read_excel("ultimatum game/ultimatum game (AMANAT social audit 2018).xlsx") %>%
  mutate(reject=ifelse(Decision=="Refused", 1,0),
         offer_range = case_when(Offer<50 ~ 1,
                                 Offer>30 & Offer < 70 ~ 2,
                                 Offer>55 ~ 3,
                                 TRUE ~ NA_real_),
         above49=ifelse(Offer>49, 1,0),
         above39=ifelse(Offer>39, 1,0),
         above29=ifelse(Offer>29, 1,0),
         above59=ifelse(Offer>59, 1,0),
         above69=ifelse(Offer>69, 1,0)) %>%
  set_labels(offer_range, labels=c("Less than $50", "$50-$55", "Greater than $55")) %>%
  mutate(offer_range_chr = as_character(offer_range)) %>%
  arrange(Decision, desc(Offer))

d
head(d)

frq(d$Offer)
frq(d$offer_range)


library(flextable)
dflex <- d %>%
  select(1:2) %>%
  flextable::flextable()

dflex

?save_as_docx

save_as_docx(dflex, path="ultimatum game/ultimatum game results.docx")

flxacc <- d %>%
  select(1:2) %>%
  filter(Decision=="Accept") %>%
  flextable()

save_as_docx(flxacc, path="ultimatum game/ultimatum game accept.docx")

flxrej <- d %>%
  select(1:2) %>%
  filter(Decision=="Refused") %>%
  flextable()

save_as_docx(flxrej, path="ultimatum game/ultimatum game reject.docx")

rng <- d %>%
  group_by(offer_range, offer_range_chr) %>%
  summarise(refused=mean(reject),
            se=std.error(reject),
            n=n()) %>%
  mutate(lower=se-1.96*se,
         upper=se+1.96*se)

rng

ggplot(rng, aes(offer_range, refused)) + 
  geom_point() + 
  geom_line(color="navy",
            size=1) +
  geom_label(aes(label=paste(round(refused*100,0), "%", sep="")),
             fill="navy",
             color="white",
             label.padding = unit(.2, "lines")) +
  scale_x_continuous(limits=c(.75, 3.25),
                     breaks=1:3,
                     labels=rng$offer_range_chr) +
  scale_y_continuous(limits=c(0,1),
                     breaks=seq(0,1,.1),
                     labels=percent_format(accuracy=1),
                     sec.axis=dup_axis()) +
  labs(x="\nOffer",
       y="Refusal\nrate",
       title="Ultimatum game results") +
  theme(axis.title.y=element_text(angle=0, vjust=.5),
        axis.title.y.right=element_blank())

ggsave("ultimatum game/refusals by offer category.png",
       device="png",
       type="cairo",
       height=4,
       width=5)


?geom_label

ot <- d %>%
  group_by(Offer) %>%
  summarise(reject=mean(reject),
            n=n())

ot

ot29 <- d %>%
  group_by(above29) %>%
  summarise(reject=mean(reject))

ot29

ot39 <- d %>%
  group_by(above39) %>%
  summarise(reject=mean(reject))

ot39

ot49 <- d %>%
  group_by(above49) %>%
  summarise(reject=mean(reject))

ot49

ot59 <- d %>%
  group_by(above59) %>%
  summarise(reject=mean(reject))

ot59

ot49 <- d %>%
  group_by(above49) %>%
  summarise(reject=mean(reject))

ot49






lm(reject~Offer, d) %>%
  summary()

ggplot(d, aes(Offer)) + 
  geom_density_ridges(aes(y=Decision))

ggplot(d, aes(Offer)) + 
  geom_bar() + 
  scale_y_continuous(breaks=1:10)
  
  
  
  
  
  
# bayes ---- 

str(d)

b1 <- brm(reject ~ Offer,
          data=d)
  
# rstanarm ---- 

r1 <- stan_glm(reject ~ Offer,
               data=d)

print(summary(r1), digits=3)  
r1  
  
l1 <- stan_glm(reject ~ Offer,
               data=d,
               family="binomial")

print(summary(l1, digits=3))

posterior_interval(l1, prob=.5)

pp_check(l1)

library(tidybayes)
library(modelr)

?data_grid
?add_epred_draws
?modelr::seq_range

l1_pred <- d %>%
  data_grid(Offer=seq_range(Offer, by=1)) %>%
  add_epred_draws(l1,
                  ndraws=100) 

l1_pred
str(l1_pred)

l1_pred_df <- l1_pred %>%
  as.data.frame()

str(l1_pred_df)
head(l1_pred_df)

head(d)

ggplot(l1_pred_df, aes(Offer, .epred)) +
  geom_dots(data=d, aes(y=reject, x=Offer, group=reject,
                        side=ifelse(reject, "bottom", "top")),
            scale=.15, 
            #pch=19, 
            #color="grey60", 
            alpha=.5) +
  scale_color_manual(values=c("Accept"="dodgerblue2", "Refused" = "firebrick2"),
                     name="Response",
                     guide=guide_legend(override.aes=list(shape=19))) +
  stat_smooth()

  stat_lineribbon(.width=.5,
                  color="dodgerblue2",
                  fill="grey60",
                  alpha=.5) +
  scale_x_continuous(breaks=seq(0,100,10)) +
  scale_y_continuous(limits=c(0,1),
                     breaks=seq(0,1,.1),
                     labels=percent_format(accuracy=1))

?stat_lineribbon
            
mutate(test_offer=seq(0,100,1)) 


test <- data.frame(test_offer=seq(0,100,1))

head(test)

test %>%
  add_epred_draws(l1, 
                  ndraws=50)

data_grid()


