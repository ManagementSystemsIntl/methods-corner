# meta analysis

# prepare ---- 

getwd()

# Afg ---- 

afg_full <- read_rds("../BPPS analysis/data/prepared/BPPS prepared.rds") %>%
  mutate(study="Afghanistan",
         perception_ord = case_when(USAID_perception_ord > 3 ~ 1,
                                    USAID_perception_ord < 3 ~ -1,
                                    USAID_perception_ord == 3 ~ 0,
                                    TRUE ~ NA_real_))
#         USAID_perception_cen=USAID_perception_ord-3)

frq(afg_full$perception_ord)
names(afg_full)

frq(afg_full$USAID_perception_ord)
frq(afg_full$perception_ord)

afg <- afg_full %>%
  select(study, 
         info,
         perception=USAID_perception_bin,
         perception_ord) %>%
  na.omit()

frq(afg$perception)
frq(afg$percption_ord)


head(afg)
names(afg)

frq(afg$perception_ord)
frq(afg$usaid_percep)

afg_tal <- afg %>%
  group_by(info, perception) %>%
  tally()

afg_tal

# Iraq ---- 

irq_full <- read_rds("../Iraq Perception Study/data/perf_merg.rds") %>%
  mutate(perception_ord = case_when(USAID_perf_ord > 0 ~ 1,
                                    USAID_perf_ord == 0 ~ 0,
                                    USAID_perf_ord < 0 ~ -1,
                                    TRUE ~ NA_real_))

names(irq_full)
frq(irq_full$USAID_perf_ord)
frq(irq_full$perception_ord)

irq <- irq_full %>%
  mutate(study = case_when(hh==1 ~ "Iraq - household",
                           TRUE ~ "Iraq - crowdsource")) %>%
  select(study, 
         info=info_treat, 
         perception=USAID_performance,
         perception_ord) %>%
  na.omit()


frq(irq$USAID_performance)
frq(irq$perception_ord)
  
  # group sizes

tal <- irq %>%
  group_by(info, perception) %>%
  tally()

tal

library(metafor)
?escalc

met_iq <- data.frame(study="Iraq",
                     tperf=898,
                     tneg=460,
                     cperf=907,
                     cneg=442)


met_iq

met_iq2 <- escalc(ai=tperf,
                  bi=tneg,
                  ci=cperf,
                  di=cneg,
                  data=met_iq,
                  measure="RR",
                  slab="Iraq")

met_iq2

?stan_glm

irq1 <- stan_glm(USAID_performance ~ seen_USAID + USAID_message + info_treat + USAID_activity + USAID_training + 
                   daily_hrs_radio + daily_hrs_tv + social_media_ord + fac_radio_print + fac_social +
                   fac_elders + fac_tv + shiite +
                   female + rural + education + madrassa_ord + age_grp + ses + never_married +  right_direction +
                   perf_fed_cen + unsafe_cen + victim_witness +as.factor(Gov) + premise,
                   family=binomial(link="logit"),
                   data=irq,
                   cores=8,
                   iter=4000,
                   chains=4)

summary(irq1, digits=3)


irq1_glm <- glm(USAID_performance ~ info_treat, 
                family="binomial",
                data=irq)

summary(irq1_glm)



irq2_glm <- glm(USAID_performance ~ seen_USAID + USAID_message + info_treat + USAID_activity + USAID_training + 
                   daily_hrs_radio + daily_hrs_tv + social_media_ord + fac_radio_print + fac_social +
                   fac_elders + fac_tv + shiite +
                   female + rural + education + madrassa_ord + age_grp + ses + never_married +  right_direction +
                   perf_fed_cen + unsafe_cen + victim_witness +as.factor(Gov) + premise,
                 family="binomial",
                 data=irq)

summary(irq2_glm)




perf9 <- stan_glmer(USAID_performance ~ seen_USAID + USAID_message + info_treat + USAID_activity + USAID_training +
                      daily_hrs_radio + daily_hrs_tv + social_media_ord + fac_radio_print + fac_social +
                      fac_elders + fac_tv + shiite +
                      female + rural + education + madrassa_ord + age_grp + ses + never_married+right_direction +
                      perf_fed_cen + unsafe_cen + victim_witness +as.factor(Gov) +
                      (seen_USAID + USAID_message + info_treat + USAID_activity + USAID_training | premise),
                    data=perf_merg,
                    cores=8)

?stan_glmer

# wbg ----

getwd()

wbg_full <- read_rds("../Palestinian Perception Study/data/prepared/Palestinian Perception Study - prepared.rds") %>%
  mutate(study="WB-Gaza",
         perception_ord = case_when(usaid_perf_cen > 0 ~ 1,
                                    usaid_perf_cen == 0 ~ 0,
                                    usaid_perf_cen < 0 ~ -1,
                                    TRUE ~ NA_real_))

names(wbg_full)
frq(wbg_full$usaid_perf_cen)
frq(wbg_full$perception_ord)

wbg <- wbg_full %>%
  select(study, 
         info=info_trt, 
         perception=usaid_perf_bin,
         perception_ord) %>%
  na.omit()

head(wbg)
frq(wbg$perception_ord)

frq(wbg_full$usaid_perf_bin)

wbg_tal <- wbg %>%
  group_by(info, perception) %>%
  tally()

wbg_tal

# Bangladesh ---- 

Y:\Private\dan.killian\AMELA\BPPS\background docs\Foreign aid, foreign policy, and domestic government legitimacy (Dietrich et al 2015)\dataverse

bd_full <- read_dta("meta-analysis/data/Dietrich et al 2015/Branding_Bangladesh_November2015.dta") %>%
  mutate(study="Bangladesh",
         perception=ifelse(influence_us>0,1,0))

names(bd_full)

frq(bd_full$perception)
frq(bd_full$influence_us)
psych::describe(bd_full$influence_us)
describeBy(bd_full$influence_us, bd_full$branded)
describeBy(bd_full$perception, bd_full$branded)

lm(perception ~ branded,
   data=bd_full) %>%
  summary()

bd <- bd_full %>%
  select(study, 
         info=branded,
         perception,
         perception_ord=influence_us)

head(bd)  
map(bd, function(x) frq(x))
lapply(bd, frq)

# all ---- 

library(dmetar)

data(ThirdWave)
ThirdWave



dat <- bind_rows(afg, irq, wbg, bd) %>%
  as.data.frame() %>%
  remove_attributes("na.action")

str(dat)
head(dat)
frq(dat$study)

write_rds(dat, "meta-analysis/data/record level data.rds")
write_csv(dat, "meta-analysis/data/record level data.csv")

?brmsformula

priors <- c(prior(normal(0,1), class=Intercept),
            prior(cauchy(0,.5), class=sd))

priors

s1 <- stan_glmer(perception ~ info + (1|study),
                 cores=8,
                 data=dat)

summary(s1, digits=3)

s1_dat <- as.data.frame(s1)
head(s1_dat)

s1_gath <- s1_dat %>%
  pivot_longer(cols=3:7,
               names_to="study2") %>%
  mutate(study=case_when(contains(str_sub(study2, ))

?pivot_longer
?str_sub
?contains

library(tidybayes)

get_variables(s1)

s1_draws <- spread_draws(s1, r_study[study,], b_Intercept) 

frq(s1_draws$term)

s1_draws <- spread_draws(s1, b[(Intercept)], b[term, study])

%>% 
  mutate(b_Intercept = r_study + b_Intercept)

head(s1_draws)

s1_pooled_draws <- spread_draws(s1, b_Intercept) %>% 
  mutate(study = "Overall")

forest.data <- bind_rows(study.draws, 
                         pooled.effect.draws) %>% 
  ungroup() %>%
  mutate(Author = str_replace_all(Author, "[.]", " ")) %>% 
  mutate(Author = reorder(Author, b_Intercept))

forest.data.summary <- group_by(forest.data, Author) %>% 
  mean_qi(b_Intercept)

ggplot(aes(b_Intercept, 
           relevel(Author, "Pooled Effect", 
                   after = Inf)), 
       data = forest.data) +
  
  # Add vertical lines for pooled effect and CI
  geom_vline(xintercept = fixef(m.brm)[1, 1], 
             color = "grey", size = 1) +
  geom_vline(xintercept = fixef(m.brm)[1, 3:4], 
             color = "grey", linetype = 2) +
  geom_vline(xintercept = 0, color = "black", 
             size = 1) +
  
  # Add densities
  geom_density_ridges(fill = "blue", 
                      rel_min_height = 0.01, 
                      col = NA, scale = 1,
                      alpha = 0.8) +
  geom_pointintervalh(data = forest.data.summary, 
                      size = 1) +
  
  # Add text and labels
  geom_text(data = mutate_if(forest.data.summary, 
                             is.numeric, round, 2),
            aes(label = glue("{b_Intercept} [{.lower}, {.upper}]"), 
                x = Inf), hjust = "inward") +
  labs(x = "Standardized Mean Difference", # summary measure
       y = element_blank()) +
  theme_minimal()





s2 <- stan_glmer(perception ~ (info|study),
                 cores=8,
                 data=dat)

summary(s2, digits=3)


s3 <- stan_glmer(perception ~ info + (info|study),
                 cores=8,
                 data=dat)

summary(s3, digits=3)

get_variables(s3)



# logistic ---- 

l2 <- brm(perception ~ info|study,
          family=bernoulli(link="logit"),
          backend="cmdstanr",
          cores=8,
          data=dat)

saveRDS(l2, "meta-analysis/l2 percep info each study.rds")

summary(l2)
ranef(l2)
exp(.49) / (1 + exp(.49))
exp(.13) / (1 + exp(.13))

mn <- dat %>%
  summarise(mn=mean(perception))

mn

library(ggeffects)
library(modelr)

st <- c("Afghanistan",
        "Bangladesh",
        "Iraq - crowdsource",
        "Iraq - household",
        "WB-Gaza")

st <- data.frame(frq(dat$study)) %>%
  select(2) %>%
  as.vector()
st

?distinct
dat %>%
  distinct(study)

l2_pred <- dat %>%
  data_grid(study=st,
            info=c(0,1)) |> 
  add_epred_draws(l2,
                  ndraws=100) %>%
  ungroup() %>%
  group_by(info, .draw) %>%
  mutate(indices=cur_group_id()) %>%
  ungroup() 

%>%
  arrange(.draw)

l2_pred
dfrq(l2_pred$indices)
frq(l2_pred$.draw)

summ <- l2_pred %>%
  group_by(study, info) %>%
  summarise(pos=mean(.epred))

summ

ggplot(l2_pred, aes(info, .epred, color=study, group=indices)) +
  geom_line(aes(color=study, group=indices), alpha=.3, size=.3) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks=0:1,
                     labels=c("Not\ntreated","Treated")) +
  scale_y_continuous(labels=percent_format(accuracy=1),
                     limits=c(0,1)) +
#                     sec.axis = sec_axis(~.,
#                                         breaks=end$brk, #c(.45,.3,.2,.08,.04),
#                                         labels=end$lab)) +
  theme(legend.position="none") +
  labs(x="",
       y="") 






l3 <- brm(perception ~ 1 + info + (info|study),
          family=bernoulli(link="logit"),
          backend="cmdstanr",
          cores=8,
          data=dat)

saveRDS("meta-analysis/l3 percep info by info study.rds")




# ordered ---- 

library(tidybayes)
library(modelr)

o1 <- brm(ordered(perception_ord) ~ 1 + info + (1|study),
          family=cumulative,
          backend="cmdstanr",
          cores=8,
          data=dat)

saveRDS(o1, "meta-analysis/models/o1.rds")

summary(o1)
ranef(o1)

o1_pred <- dat |>
  data_grid(study=st,
            info=0:1) |>
  add_epred_draws(o1, 
                  ndraws=100) |> #, n=100) |>
  ungroup() |>
  mutate(cat = factor(as.numeric(.category), labels=c("Negative","Neutral","Positive")))

o1_pred

frq(o1_pred$.category)
frq(dat$perception_ord)

o1_pred <- o1_pred %>%
  group_by(.draw, cat) %>%
  mutate(indices=cur_group_id()) %>%
  ungroup()


end <- data.frame(brk=c(.45,.28,.19,.07,.03),
                  lab = c("Somewhat positive",
                          "Very positive",
                          "Neutral",
                          "Somewhat negative",
                          "Very negative"),
                  color=viridis(5))

end

o <- ggplot(o1_pred, aes(info, .epred, color=cat)) +
  geom_line(aes(group=indices), alpha=.3, size=.3) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks=0:1,
                     labels=c("Untreated","Treated")) +
  scale_y_continuous(labels=percent_format(accuracy=1),
                     limits=c(0,.8)) +
#                     sec.axis = sec_axis(~.,
#                                         breaks=end$brk, #c(.45,.3,.2,.08,.04),
#                                         labels=end$lab)) +
  #data=end)#, #c("Somewhat positive",
  #         "Very positive",
  #          "Neutral",
  #          "Somewhat negative",
  #         "Very negative"))) +
  theme(legend.position="none") +
  labs(x="",
       y="") 
#  geom_line(aes(x=usaid_famil, y=.value), size=4, color="pink")
#  geom_textline(aes(label=cat), hjust=.75)

o





o2 <- brm(ordered(perception_ord) ~ (info|study),
          family=cumulative,
          backend="cmdstanr",
          cores=8,
          data=dat)

summary(o2)
ranef(o2)
saveRDS(o2, "meta-analysis/models/o2 perception ord info within study.rds")

?conditional_effects

conditional_effects(o2, categorical=T)

plot(conditional_effects(o3))

get_variables(o2)


o3 <- brm(ordered(perception_ord) ~ 1 + info + (info|study),
          family=cumulative,
          backend="cmdstanr",
          cores=8,
          data=dat)

summary(o3)
ranef(o3)
saveRDS(o3, "meta-analysis/models/o3 perception ord info + info within study.rds")



b1_pred <- dat %>%
  data_grid(usaid_famil = c(0,1,2)) %>%
  #add_fitted_draws(b1, n=100) %>%
  add_epred_draws(b1, 
                  ndraws=100) %>% #, n=100) %>%
  ungroup() %>%
  mutate(cat = factor(as.numeric(.category), labels=percept_key$perception_lab[1:5]))

b1_pred


b1_pred <- b1_pred %>%
  group_by(.draw, cat) %>%
  mutate(indices=cur_group_id()) %>%
  ungroup()

?cur_group_id

end <- data.frame(brk=c(.45,.28,.19,.07,.03),
                  lab = c("Somewhat positive",
                          "Very positive",
                          "Neutral",
                          "Somewhat negative",
                          "Very negative"),
                  color=viridis(5))

end

b <- ggplot(b1_pred, aes(usaid_famil, .epred, color=cat)) +
  geom_line(aes(group=indices), alpha=.3, size=.3) +
  scale_color_viridis_d() +
  scale_x_continuous(breaks=0:2,
                     labels=c("Not\nfamiliar","Somewhat\nfamiliar","Very\nfamiliar")) +
  scale_y_continuous(labels=percent_format(accuracy=1),
                     limits=c(0,.5),
                     sec.axis = sec_axis(~.,
                                         breaks=end$brk, #c(.45,.3,.2,.08,.04),
                                         labels=end$lab)) +
  #data=end)#, #c("Somewhat positive",
  #         "Very positive",
  #          "Neutral",
  #          "Somewhat negative",
  #         "Very negative"))) +
  theme(legend.position="none") +
  labs(x="",
       y="") 

b





# study level ---- 

afg_out <- data.frame(study="Afghanistan",
                      TE=.044,
                      seTE=.014,
                      n=6733)

irq_out <- data.frame(study="Iraq",
                      TE=0,
                      seTE=.045,
                      n=2707)

wbg_out <- data.frame(study="WB-Gaza",
                      TE=.041,
                      seTE=.023,
                      n=2555)

out <- bind_rows(afg_out, irq_out, wbg_out)

out

write_csv(out, "meta-analysis/data/lpm outcomes.csv")

?metagen

library(metafor)
library(meta)

out_gen <- metagen(TE,
                   seTE, 
                   studlab=study,
                   data=out)

out_gen
summary(out_gen)

forest(out_gen)


  # metabin

metbin <- read_excel("meta-analysis/data/USAID perception info treatment outcomes.xlsx",
                     sheet="metabin")

metbin

?metabin
metbin_fit <- metabin(event.e,
                      n.e,
                      event.c,
                      n.c,
                      studlab=Study,
                      sm="RR",
                      data=metbin)

summary(metbin_fit)

forest(metbin_fit)

  # brm

b1 <- brm(TE|se(seTE) ~ 1 + (1|study),
          data=out)

s1 <- stan_glmer(TE|se(seTE) ~ 1 + (1|study),
                 data=out)


?rma.mv



# baggr binary ---- 

library(baggr)

tal <- dat %>%
  group_by(study, info, perception) %>%
  tally()

tal

dat %>%
  group_by(study, info) %>%
  tally()

md <- data.frame(study=c("Afghanistan", "Iraq-crowdsource","Iraq-household","WB-Gaza"),
                 a = c(1837, 396, 502, 696),
                 n1i = c(3380, 629, 729, 1259),
                 c = c(1699, 374, 533, 696),
                 n2i = c(3353, 599, 750, 1309))

md

md_prepOR <- prepare_ma(md,
                      effect="logOR",
                      group="study")

md_prepOR

labbe(md_prepOR,
      plot_model=T,
      shade_se="or")

?baggr

md_OR <- baggr(md_prepOR, 
            effect="Logarithm of odds ratio")
md_OR

forest_plot(md_OR)


md_prepRR <- prepare_ma(md,
                        effect="logRR",
                        group="study")

labbe(md_prepRR)

md_rr <- baggr(md_prepRR,
               effect="Logarithm of relative risk")

summary(md_rr)
md_rr

forest_plot(md_rr,
            show="both",
            print="inputs")
exp(.0596)
exp(.0231)

md_prepRD <- prepare_ma(md,
                        effect="RD",
                        group="study")

md_prepRD

md_rd <- baggr(md_prepRD,
               effect="Risk difference")

md_rd

forest_plot(md_rd,
            show="both",
            print="inputs")

# with Bangladesh

.84*1177
.8*1086

md2 <- data.frame(study=c("Afghanistan", "Iraq-crowdsource","Iraq-household","WB-Gaza", "Bangladesh"),
                 a = c(1837, 396, 502, 696, 989),
                 n1i = c(3380, 629, 729, 1259, 1177),
                 c = c(1699, 374, 533, 696, 869),
                 n2i = c(3353, 599, 750, 1309, 1086))

md2


md2_prepRR <- prepare_ma(md2,
                        effect="logRR",
                        group="study")

labbe(md2_prepRR,
      plot_model=T,
      shade_se="rr")

md2_rr <- baggr(md2_prepRR,
               effect="Logarithm of relative risk")

summary(md2_rr)
md2_rr

forest_plot(md2_rr,
            show="both",
            print="inputs")
exp(.0365)
exp(.0281)

md2_prepRD <- prepare_ma(md2,
                        effect="RD",
                        group="study")

md2_prepRD

md2_rd <- baggr(md2_prepRD,
               effect="Risk difference")

md2_rd

forest_plot(md2_rd,
            show="both",
            print="inputs")




















