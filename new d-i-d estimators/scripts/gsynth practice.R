# gsynth practice

library(gsynth)
library(panelView)

data(gsynth)


# simdata -----------------------------------------------------------------

str(simdata)
head(simdata)

frq(simdata$time)
frq(simdata$D)
table(simdata$time,
      simdata$D)

panelView(Y ~ D,
          data=simdata,
          index=c("id","time"))

panelView(Y ~ D, 
          data=simdata,
          index=c("id","time"),
          type="outcome")

s1 <- gsynth(Y ~ D + X1 + X2, 
             data=simdata,
             index=c("id","time"),
             force="two-way",
             CV=T,
             r=c(0,5),
             se=T,
             inference="parametric",
             nboots=1000,
             parallel=F)

s1
summary(s1)
print(s1)
plot(s1, color="dodgerblue2") + nobord

a + 
  scale_color_manual(values="dodgerblue")
  
stat_smooth(color="dodgerblue")

str(s1)

s1_att <- s1$att
s1_att

s1_est <- s1$est.ind %>%
  as.data.frame %>%
  rowid_to_column() %>%
  select(1:6) %>%
  set_names(nm=c("period","att","se","lower","upper","pval")) 

names(s1_est)
s1_est
str(s1_est)

ggplot(s1_est, aes(x=period, y=att)) + 
  stat_smooth(color="dodgerblue2") +
  nobord

# turnout -----------------------------------------------------------------

str(turnout)
head(turnout)

panelView(turnout ~ policy_edr,
          data=turnout,
          index=c("abb","year"))



# GIST revenue ------------------------------------------------------------

#dat <- read_excel("C:/Egnyte/Private/dkillian/GIST/GIST analysis/data/MSI survey/revenue/revenue cleaned.xlsx")


dat <- read_csv("C:/Egnyte/Private/dkillian/GIST/GIST analysis/data/MSI survey/revenue/revenue converted.csv") 

names(dat)

dat <- dat %>%
  select(1:15, name:revyr, yr, revyr_calc, revenue:logrev_topcode) %>%
  filter(revyr<2020)
  
  mutate(revtreat=1)

head(dat)
sum(dat$revyr != dat$revyr_calc)
which(dat$revyr != dat$revyr_calc)

a <- table(dat$name, dat$revyr_calc) %>%
  as.data.frame

out <- dat %>%
  select(serial, revyr_calc, revenue) %>%
  as.data.frame

out2 <- out %>%
  pivot_wider(id_cols=1,
              names_from=revyr_calc,
              values_from=revenue,
              values_fill=0)
,
              values_fn=length)


?pivot_wider

str(out)
head(out)
names(out)

out2 <- out

out2[,5:13][is.na(out2[,5:13])] <- 0

panelView(revenue ~ winfin,
          data=dat,
          index=c("name","revyr"))

panelView(revenue ~ finalist,
          data=dat,
          index=c("name","revyr"),
          type="outcome")



s1_g <- gsynth(logrev_topcode ~ winfin, 
             data=dat,
             index=c("name","revyr"),
             force="two-way",
             CV=T,
             r=c(0,5),
             se=T,
             inference="parametric",
             nboots=1000,
             parallel=F)

s1
summary(s1)
print(s1)
a <- plot(s1, color="dodgerblue2") + nobord

a + 
  scale_color_manual(values="dodgerblue")

stat_smooth(color="dodgerblue")

str(s1)

s1_att <- s1$att
s1_att

s1_est <- s1$est.ind %>%
  as.data.frame %>%
  rowid_to_column() %>%
  select(1:6) %>%
  set_names(nm=c("period","att","se","lower","upper","pval")) 

names(s1_est)
s1_est
str(s1_est)

ggplot(s1_est, aes(x=period, y=att)) + 
  stat_smooth(color="dodgerblue2")

