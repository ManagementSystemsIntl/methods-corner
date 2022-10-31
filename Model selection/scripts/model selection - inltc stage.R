# model selection
# INLTC Stage data

# description ---- 

names(dat)
str(dat)

# gain scores, brms ---- 

gn0 <- brm(gain ~ treat,
            data=dat,
            backend="cmdstanr",
            prior=set_prior("student_t(74, 2,7)",
                            coef="treat"))

summary(gn0)
save_model
write_rds(gn0, file="models/gn0.rds")


b1 <- brm(gain ~ Concord + AttendINLTC,
          data=dat,
          backend="cmdstanr")



,
prior=set_prior("normal(2,5)",
                coef="AttendINLTC"))

summary(b1)
summary(l1)


b1.1 <- brm(gain ~ Concord + AttendINLTC,
            data=dat,
            backend="cmdstanr",
            prior=set_prior("normal(2,7)",
                            coef="AttendINLTC"))

summary(b1)
summary(b1.1)
summary(l1)


b2 <- brm(gain ~ Concord + AttendINLTC + Female,
          data=dat,
          backend="cmdstanr")

,
prior=set_prior(c("normal(2,5)", coef="AttendINLTC"),
)

summary(b2)
summary(l2)


b2.1 <- brm(gain ~ Concord + AttendINLTC + Female,
            data=dat,
            backend="cmdstanr",
            prior=c(set_prior("normal(2,7)", coef="AttendINLTC"),
                    set_prior()
                    ("normal(0,5)", coef="Female")))

summary(b2)
summary(b2.1)
summary(l2)

b3 <- brm(gain ~ Concord + AttendINLTC + Female + KabulUniversity,
          data=dat,
          backend="cmdstanr")



b3.1 <- brm(gain ~ Concord + AttendINLTC + Female + KabulUniversity,
            data=dat,
            backend="cmdstanr",
            prior=c(set_prior("normal(2,7)", coef="AttendINLTC"),
                    set_prior("normal(0,5)", coef="Female"),
                    set_prior("normal(0,5)", coef="KabulUniversity")))

summary(b3)
summary(b3.1)
summary(l2)


b4 <- brm(gain ~ Concord + AttendINLTC + Female + KabulUniversity + Shariah,
          data=dat,
          backend="cmdstanr")



b4.1 <- brm(gain ~ Concord + AttendINLTC + Female + KabulUniversity + Shariah,
            data=dat,
            backend="cmdstanr",
            prior=c(set_prior("normal(2,7)", coef="AttendINLTC"),
                    set_prior("normal(0,5)", coef="Female"),
                    set_prior("normal(0,5)", coef="KabulUniversity"),
                    set_prior("normal(0,5)", coef="Shariah")))

summary(b4)
summary(b4.1)
summary(l2)



b5 <- brm(gain ~ Concord + AttendINLTC + Female + KabulUniversity + Shariah + YrsGraduated,
          data=dat,
          backend="cmdstanr")



b5.1 <- brm(gain ~ Concord + AttendINLTC + Female + KabulUniversity + Shariah + YrsGraduated,
            data=dat,
            backend="cmdstanr",
            prior=c(set_prior("normal(2,7)", coef="AttendINLTC"),
                    set_prior("normal(0,5)", coef="Female"),
                    set_prior("normal(0,5)", coef="KabulUniversity"),
                    set_prior("normal(0,5)", coef="Shariah"),
                    set_prior("normal(0,5)", coef="YrsGraduated")))

summary(b5)
summary(b5.1)


b6.1 <- brm(gain ~ Concord + AttendINLTC + Female + KabulUniversity + OutsideKabul + Shariah + YrsGraduated,
            data=dat,
            backend="cmdstanr",
            prior=c(set_prior("normal(2,7)", coef="AttendINLTC"),
                    set_prior("normal(0,5)", coef="Female"),
                    set_prior("normal(0,5)", coef="KabulUniversity"),
                    set_prior("normal(0,5)", coef="Shariah"),
                    set_prior("normal(0,5)", coef="YrsGraduated"),
                    set_prior("normal(0,5)", coef="OutsideKabul")))

summary(b6)
summary(b6.1)



b1.1 <- add_criterion(b1.1, criterion="waic")
b2.1 <- add_criterion(b2.1, criterion="waic")
b3.1 <- add_criterion(b3.1, criterion="waic")
b4.1 <- add_criterion(b4.1, criterion="waic")
b5.1 <- add_criterion(b5.1, criterion="waic")
b6.1 <- add_criterion(b6.1, criterion="waic")

a <- loo_compare(b1.1, b2.1, b3.1, b4.1, b5.1, b6.1, criterion="waic")

a

b1.1 <- add_criterion(b1.1, criterion="loo")
b2.1 <- add_criterion(b2.1, criterion="loo")
b3.1 <- add_criterion(b3.1, criterion="loo")
b4.1 <- add_criterion(b4.1, criterion="loo")
b5.1 <- add_criterion(b5.1, criterion="loo")
b6.1 <- add_criterion(b6.1, criterion="loo")

a_loo <- loo_compare(b1.1, b2.1, b3.1, b4.1, b5.1, b6.1, criterion="loo")

a_loo
?loo_compare
?update

tab_model(b1.1, b2.1, b3.1, b4.1, b5.1,
          collapse.ci = T)

?ensemble

ot <- ensemble(b1.1, b2.1)

summary(ot)

b1.1out <- cplot(b1.1)

?cplot

cplot(b1.1)

marginal_effects(b1.1)

?marginal_effects

conditional_effects(b1.1)

ot <- conditional_effects(b1.1)

ot[1]

?predict

b1.1_pred <- predict(b1.1)
b2.1_pred <- predict(b2.1)

WAIC(b5.1)
?AIC
AIC(b5.1)

compare(b1.1, b2.1)

# gain scores, rethinking ---- 


# ancova, brms ---- 

describeBy(dat$post, dat$treat)

banc0 <- brm(post ~ treat,
            data=dat,
            backend="cmdstanr",
            prior=c(set_prior("student_t(74, 75,10)", class="Intercept"),
                    set_prior("student_t(74, 2,7)", coef="treat")))

write_rds(banc0, file="models/brm/ancova/banc0.rds")
banc0 <- read_rds("models/brm/ancova/banc0.rds")

summary(banc0)

describe(dat$pre)
describeBy(dat$pre, dat$treat)

banc1 <- brm(post ~ treat + pre,
            data=dat,
            backend="cmdstanr",
            prior=c(set_prior("student_t(74, 50,10)", class="Intercept"),
                    set_prior("student_t(74, 2,7)", coef="treat"),
                    set_prior("student_t(74, 0,1)", coef="pre")))

write_rds(banc1, file="models/brm/ancova/banc1.rds")
banc1 <- read_rds("models/brm/ancova/banc1.rds")

summary(banc1)


banc2 <- brm(post ~ treat + pre + female,
             data=dat,
             backend="cmdstanr",
             prior=c(set_prior("student_t(74, 50,10)", class="Intercept"),
                     set_prior("student_t(74, 2,7)", coef="treat"),
                     set_prior("student_t(74, 0,1)", coef="pre"),
                     set_prior("student_t(74, 0,1)", coef="female")))

write_rds(banc2, file="models/brm/ancova/banc2.rds")
banc2 <- read_rds("models/brm/ancova/banc2.rds")

summary(banc2)




banc3 <- brm(post ~ treat + pre + female + outside_kabul,
             data=dat,
             backend="cmdstanr",
             prior=c(set_prior("student_t(74, 50,10)", class="Intercept"),
                     set_prior("student_t(74, 2,7)", coef="treat"),
                     set_prior("student_t(74, 0,1)", coef="pre"),
                     set_prior("student_t(74, 0,1)", coef="female"),
                     set_prior("student_t(74, 0,1)", coef="outside_kabul")))

write_rds(banc3, file="models/brm/ancova/banc3.rds")
banc3 <- read_rds("models/brm/ancova/banc3.rds")

summary(banc3)



banc4 <- brm(post ~ treat + pre + female + outside_kabul + kabuluni,
             data=dat,
             backend="cmdstanr",
             prior=c(set_prior("student_t(74, 50,10)", class="Intercept"),
                     set_prior("student_t(74, 2,7)", coef="treat"),
                     set_prior("student_t(74, 0,1)", coef="pre"),
                     set_prior("student_t(74, 0,1)", coef="female"),
                     set_prior("student_t(74, 0,1)", coef="outside_kabul"),
                     set_prior("student_t(74, 0,1)", coef="kabuluni")))

write_rds(banc4, file="models/brm/ancova/banc4.rds")
banc4 <- read_rds("models/brm/ancova/banc4.rds")

summary(banc4)



banc5 <- brm(post ~ treat + pre + female + outside_kabul + kabuluni + shariah,
             data=dat,
             backend="cmdstanr",
             prior=c(set_prior("student_t(74, 50,10)", class="Intercept"),
                     set_prior("student_t(74, 2,7)", coef="treat"),
                     set_prior("student_t(74, 0,1)", coef="pre"),
                     set_prior("student_t(74, 0,1)", coef="female"),
                     set_prior("student_t(74, 0,1)", coef="outside_kabul"),
                     set_prior("student_t(74, 0,1)", coef="kabuluni"),
                     set_prior("student_t(74, 0,1)")))

write_rds(banc5, file="models/brm/ancova/banc5.rds")
banc5 <- read_rds("models/brm/ancova/banc5.rds")

summary(banc5)




banc6 <- brm(post ~ treat + pre + female + outside_kabul + kabuluni + shariah + yrsgrad,
             data=dat,
             backend="cmdstanr",
             prior=c(set_prior("student_t(74, 50,10)", class="Intercept"),
                     set_prior("student_t(74, 2,7)", coef="treat"),
                     set_prior("student_t(74, 0,1)", coef="pre"),
                     set_prior("student_t(74, 0,1)", coef="female"),
                     set_prior("student_t(74, 0,1)", coef="outside_kabul"),
                     set_prior("student_t(74, 0,1)", coef="kabuluni"),
                     set_prior("student_t(74, 0,1)", coef="shariah"),
                     set_prior("student_t(74, 0,1)", coef="yrsgrad")))

write_rds(banc6, file="models/brm/ancova/banc6.rds")
banc6 <- read_rds("models/brm/ancova/banc6.rds")

summary(banc6)


describe(dat$entrance)

banc7 <- brm(post ~ treat + pre + female + outside_kabul + kabuluni + shariah + yrsgrad + entrance,
             data=dat,
             backend="cmdstanr",
             prior=c(set_prior("student_t(74, 50,10)", class="Intercept"),
                     set_prior("student_t(74, 2,7)", coef="treat"),
                     set_prior("student_t(74, 0,1)", coef="pre"),
                     set_prior("student_t(74, 0,1)", coef="female"),
                     set_prior("student_t(74, 0,1)", coef="outside_kabul"),
                     set_prior("student_t(74, 0,1)", coef="kabuluni"),
                     set_prior("student_t(74, 0,1)", coef="shariah"),
                     set_prior("student_t(74, 0,1)", coef="yrsgrad"),
                     set_prior("student_t(74, 0,1)", coef="entrance")))

write_rds(banc7, file="models/brm/ancova/banc7.rds")
banc7 <- read_rds("models/brm/ancova/banc7.rds")

summary(banc7)

?tab_model
tab_model(banc0, banc1, banc2, banc3, banc4, banc5, banc6, banc7,
          dv.labels=c("Model 0", "Model 1", "Model 2", "Model 3","Model 4","Model 5","Model 6","Model 7"),
                      collapse.ci = T,
                      file="models/brm/ancova/inltc stage regs brm ancova.htm")

names(dat)
library(causalTree)

?causalTree
banc_caustree <- causalTree(post ~ pre + entrance + female + outside_kabul + kabuluni + shariah + yrsgrad,
                       data=dat
                       , treatment=dat$treat
                       , split.Rule = "CT"
                       , cv.option = "CT"
                       , split.Honest = T
                       , cv.Honest = T
                       , split.Bucket = T
                       ,propensity = 0.5
                       #, minsize = 50
)

opcp <- banc_caustree$cptable[,1][which.min(banc_caustree$cptable[,3])]

banc_caustree <- prune(banc_caustree, opcp)

plot(banc_caustree)
rpart.plot(banc_caustree)

library(rattle)
fancyRpartPlot(banc_caustree)

fancyRpartPlot(perf_honest_tree_prune, main = "Honest Causal Tree for USAID Activity Awareness Treatment Effects"
               # , palettes = "BuGn"
               , family = "Gill Sans MT")


mod <- as.formula("post ~ treat + pre + entrance + female + outside_kabul + kabuluni + shariah + yrsgrad")
preds <- as.formula("post ~ treat + pre + entrance + female + outside_kabul + kabuluni + shariah + yrsgrad")

x <- dat %>%
  select(treat, pre, entrance, female, outside_kabul, kabuluni, shariah, yrsgrad)

library(BMA)
a <- bic.glm(mod, data=dat, glm.family = "gaussian")

str(a)
a
a[2]
a$probne0

a_out <- data.frame(a$probne0) %>%
  rownames_to_column("measure") %>%
  mutate(prob=a.probne0/100,
         lab=c("Attended INLTC", "Pretest","Entrance exam score","Female","Not from Kabul","Attended Kabul University", "Shariah faculty","Years since graduation")) %>%
  arrange(desc(prob))
a_out

ggplot(a_out, aes(prob, y=reorder(lab, prob))) +
  geom_point(size=4, color="dodgerblue2") +
  scale_x_continuous(limits=c(0,1),
                     breaks=seq(0,1,.2),
                     labels=percent) +
  labs(x="",
       y="",
       title="Variable importance by\nBayesian Model Averaging",
       caption="package BMA")

ggsave("viz/banc bma.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


banc_ct <- party::ctree(mod, 
                        data=dat)

plot(banc_ct, type="simple")






?add_criterion
banc0 <- add_criterion(banc0, criterion="waic")
banc1 <- add_criterion(banc1, criterion="waic")
banc2 <- add_criterion(banc2, criterion="waic")
banc3 <- add_criterion(banc3, criterion="waic")
banc4 <- add_criterion(banc4, criterion="waic")
banc5 <- add_criterion(banc5, criterion="waic")
banc6 <- add_criterion(banc6, criterion="waic")
banc7 <- add_criterion(banc7, criterion="waic")

banc0 <- add_criterion(banc0, criterion="loo")
banc1 <- add_criterion(banc1, criterion="loo")
banc2 <- add_criterion(banc2, criterion="loo")
banc3 <- add_criterion(banc3, criterion="loo")
banc4 <- add_criterion(banc4, criterion="loo")
banc5 <- add_criterion(banc5, criterion="loo")
banc6 <- add_criterion(banc6, criterion="loo")
banc7 <- add_criterion(banc7, criterion="loo")

?compare
compare(banc0, banc1, banc2, banc3, banc4, banc5, banc6, banc7)

loo_compare(banc0, banc1, banc2, banc3, banc4, banc5, banc6, banc7)


# ancova, rethinking ---- 


