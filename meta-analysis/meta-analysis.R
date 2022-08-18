# meta analysis

# prepare ---- 

getwd()

# Afg ---- 

afg_full <- read_rds("Y:/Private/dkillian/AMELA/BPPS/BPPS analysis/data/prepared/BPPS prepared.rds") %>%
  mutate(study="Afghanistan")

afg <- afg_full %>%
  select(study, info, perception=USAID_perception_bin) %>%
  na.omit()

frq(afg$USAID_perception_bin)

names(afg)

frq(afg$)
frq(afg$usaid_percep)

afg_tal <- afg %>%
  group_by(info, perception) %>%
  tally()

afg_tal

# Iraq ---- 

irq_full <- read_rds("../Iraq Perception Study/data/perf_merg.rds") 

names(irq_full)

irq <- irq_full %>%
  mutate(study = case_when(hh==1 ~ "Iraq - household",
                           TRUE ~ "Iraq - crowdsource")) %>%
  select(study, info=info_treat, perception=USAID_performance) %>%
  na.omit()


frq(irq$USAID_performance)
  
  # group sizes

tal <- irq %>%
  group_by(info_treat, USAID_performance) %>%
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
  mutate(study="WB-Gaza")

wbg <- wbg_full %>%
  select(study, info=info_trt, perception=usaid_perf_bin) %>%
  na.omit()

head(wbg)

frq(wbg_full$usaid_perf_bin)

wbg_tal <- wbg %>%
  group_by(info, perception) %>%
  tally()

wbg_tal

# all ---- 

library(dmetar)

data(ThirdWave)
ThirdWave



dat <- bind_rows(afg, irq, wbg) %>%
  as.data.frame() %>%
  remove_attributes("na.action")

str(dat)

?brmsformula

priors <- c(prior(normal(0,1), class=Intercept),
            prior(cauchy(0,.5), class=sd))

priors

s1 <- stan_glmer(perception ~ info + (1|study),
                 cores=8,
                 data=dat)

summary(s1, digits=3)

library(tidybayes)

s1_draws <- spread_draws(s1, r_study[study,], b_Intercept) %>% 
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








