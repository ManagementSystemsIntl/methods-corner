# MISTI 
# data prep

dat <- read_csv("data/W1-5 long.csv")

names(dat)

class(dat$Wave)
frq(dat$Wave)
frq(dat$treat)

dat <- dat %>%
  arrange(Village.Code, desc(Wave), desc(Total.Activities))

dat2 <- dat %>%
  #  filter(!(Wave==1 & treatment==1),
  #         !(Wave==2 & treatment==1)) %>%
  select(village=1, 
         wave=2,
         treat=3, 
         #stab=SI2,
         district=district.x,
         province,
         region,
         dist,
         elevation,
         pop,
         lang,
         ethnicity,
         stab=SI2,
         #         resil=RI2,
         SIKA,
         Project) %>%
  mutate(idname=as.numeric(factor(village)),
         stab_std=scale(stab),
         #         resil_std=scale(resil),
         #treat2=ifelse(lag(treat)==1, 1, treat),
         #treat2 = ifelse(is.na(treat2), treat, treat2),
         vil_wave=paste(village, wave)) %>%
  na.omit() %>%
  arrange(village, wave) %>%
  group_by(village) %>%
  mutate(#wave=as.numeric(wave),
    treat_cum=cumsum(treat),
    treat_event=ifelse(treat_cum>0,1,0)) %>%
  ungroup() %>%
  #mutate(prog = case_when()
  #type = case_when(sum(treat_event)==0 ~ "Never treated",
  #                         sum(treat_event)==1 ~ "Wave 5 treated",
  #                         sum(treat_event)==2 ~ "Wave 4 treated",
  #                         sum(treat_event)==3 ~ "Wave 3 treated",
  #                         sum(treat_event)==4 ~ "Wave 2 treated",
  #                         sum(treat_event)==5 ~ "Wave 1 treated"),
  #        first.treat=case_when(type=="Never treated" ~ 0,
  #                              type=="Wave 1 treated" ~ 1,
  #                              type=="Wave 2 treated" ~ 2,
  #                              type=="Wave 3 treated" ~ 3,
#                              type=="Wave 4 treated" ~ 4,
#                              type=="Wave 5 treated" ~ 5)) %>%
#  relocate(treat2, .after=treat) %>%
relocate(treat_cum, .after=treat) %>%
  relocate(treat_event, .after=treat) %>%
  arrange(village, wave, desc(treat)) %>%
  as.data.frame()

# measure matrix 

wave_mat <- dat2 %>%
  select(village, wave) %>%
  arrange(village, wave) %>%
  pivot_wider(names_from=wave,
              values_from=wave) %>%
  #na.omit() %>% # only villages with five measurements %>%
  select(1, 4, everything()) # order columns to be 1-5

?unite

wave_matrix <- wave_mat %>%
  unite(measured, -1, sep=" ")

wave_matrix

#dat2 <- dat2 %>%
#  left_join(wave_matrix)

frq(dat2$measured)

measure_tab <- data.frame(frq(wave_matrix$measured)) %>%
  arrange(desc(frq))

measure_tab

# treat matrix

treat_mat <- dat2 %>%
  select(village, wave, treat, measured) %>%
  arrange(village, wave) %>%
  pivot_wider(names_from=wave,
              values_from=treat) %>%
  #na.omit() %>% # only villages with five measurements %>%
  select(1, 2,5, everything()) # order columns to be 1-5

?unite

treat_matrix <- treat_mat %>%
  unite(treated, -(1:2), sep=" ")

dat2 <- dat2 %>%
  left_join(treat_matrix) %>%
  mutate(stab_std=as.numeric(stab_std),
         idname=as.numeric(factor(village)))



# wave_treat <- dat2 %>%
#   select(village, wave, treat, progression) %>%
#   arrange(village, wave) %>%
#   pivot_wider(names_from=wave,
#               values_from=treat) %>%
#   #na.omit() %>% # only villages with five measurements %>%
#   select(1, 2,5, everything()) # order columns to be 1-5
#   mutate(prog=across(c(2:6), fn=paste(sep="-")))
# 
#   
# wave_treat
# 
# wave_treat_event <- dat2 %>%
#   select(village, wave, treat_event) %>%
#   arrange(village, wave) %>%
#   pivot_wider(names_from=wave,
#               values_from=treat_event) %>%
#   #na.omit() %>% # only villages with five measurements %>%
#   select(1, 4, everything()) # order columns to be 1-5
# 
# wave_treat_event


m15 <- dat2 %>%
  filter(measured=="1 2 3 4 5") %>%
  arrange(village, wave) %>%
  group_by(village) %>%
  mutate(cohort = case_when(sum(treat_event)==0 ~ "Never treated",
                 sum(treat_event)==1 ~ "Wave 5 treated",
                 sum(treat_event)==2 ~ "Wave 4 treated",
                 sum(treat_event)==3 ~ "Wave 3 treated",
                 sum(treat_event)==4 ~ "Wave 2 treated",
                 sum(treat_event)==5 ~ "Wave 1 treated"),
         first.treat=case_when(cohort=="Never treated" ~ 0,
                      cohort=="Wave 1 treated" ~ 1,
                      cohort=="Wave 2 treated" ~ 2,
                      cohort=="Wave 3 treated" ~ 3,
                      cohort=="Wave 4 treated" ~ 4,
                      cohort=="Wave 5 treated" ~ 5))

write_csv(m15, "data/MISTI villages waves 1-5.csv")
write_rds(m15, "data/MISTI villages waves 1-5.rds")

frq(m15$cohort)


m25 <- dat2 %>%
  filter(measured=="NA 2 3 4 5") %>%
  arrange(village, wave) %>%
  group_by(village) %>%
  mutate(cohort = case_when(sum(treat_event)==0 ~ "Never treated",
                            sum(treat_event)==1 ~ "Wave 5 treated",
                            sum(treat_event)==2 ~ "Wave 4 treated",
                            sum(treat_event)==3 ~ "Wave 3 treated",
                            sum(treat_event)==4 ~ "Wave 2 treated",
                            sum(treat_event)==5 ~ "Wave 1 treated"),
         first.treat=case_when(cohort=="Never treated" ~ 0,
                               cohort=="Wave 1 treated" ~ 1,
                               cohort=="Wave 2 treated" ~ 2,
                               cohort=="Wave 3 treated" ~ 3,
                               cohort=="Wave 4 treated" ~ 4,
                               cohort=="Wave 5 treated" ~ 5))

write_csv(m25, "data/MISTI villages waves 2-5.csv")
write_rds(m25, "data/MISTI villages waves 2-5.rds")







