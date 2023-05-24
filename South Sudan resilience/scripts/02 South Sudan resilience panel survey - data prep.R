# South Sudan resilience panel
# prep

getwd()

source("scripts/0 South Sudan resilience panel survey - prep.r")

d <- read_csv("data/daily/mels_resilience_panel_survey_english (5-24-23).csv")

d <- d %>%
  mutate(id = 1:nrow(d)) %>%
  dplyr::select(id, everything())

# sources of income ---- 

frq(d$`401_a`)

d <- d %>%
  mutate(inc_farm = ifelse(`401_a`==1, 1,0),
         inc_cattle = ifelse(`401_b`==1, 1,0),
         inc_goats = ifelse(`401_c`==1, 1,0),
         inc_sheep = ifelse(`401_d`==1, 1,0),
         inc_fishing = ifelse(`401_e`==1, 1,0),
         inc_agwageout = ifelse(`401_f`==1, 1,0),
         inc_agwagein = ifelse(`401_g`==1, 1,0),
         inc_nonagout = ifelse(`401_h`==1, 1,0),
         inc_nonagin = ifelse(`401_i`==1, 1,0),
         inc_salary = ifelse(`401_j`==1, 1,0),
         inc_bush = ifelse(`401_k`==1, 1,0),
         inc_honey = ifelse(`401_l`==1, 1,0),
         inc_tradeother = ifelse(`401_m`==1, 1,0),
         inc_tradeown = ifelse(`401_n`==1, 1,0),
         inc_selfag = ifelse(`401_o`==1, 1,0),
         inc_selfnonag = ifelse(`401_p`==1, 1,0),
         inc_rent = ifelse(`401_q`==1, 1,0),
         inc_remit = ifelse(`401_r`==1, 1,0),
         inc_inherit = ifelse(`401_s`==1, 1,0),
         inc_assist = ifelse(`401_t`==1, 1,0),
         inc_other = ifelse(`401_u`==1, 1,0))

frq(d$inc_farm)

d <- d %>%
  mutate(inc_farm2 = case_when(`401_a`==1 ~ 1,
                              TRUE ~ 0),
         inc_cattle2 = case_when(`401_b`==1 ~ 1,
                                 TRUE ~ 0),
         inc_goats2 = case_when(`401_c`==1 ~ 1,
                                TRUE ~ 0),
         inc_sheep2 = case_when(`401_d`==1 ~ 1,
                                TRUE ~ 0),
         inc_fishing2 = case_when(`401_e`==1 ~ 1,
                                  TRUE ~ 0),
         inc_agwageout2 = case_when(`401_f`==1 ~ 1,
                                    TRUE ~ 0),
         inc_agwagein2 = case_when(`401_g`==1 ~ 1,
                                   TRUE ~ 0),
         inc_nonagout2 = case_when(`401_h`==1 ~ 1,
                                   TRUE ~ 0),
         inc_nonagin2 = case_when(`401_i`==1 ~ 1,
                                  TRUE ~ 0),
         inc_salary2 = case_when(`401_j`==1 ~ 1,
                                 TRUE ~ 0),
         inc_bush2 = case_when(`401_k`==1 ~ 1,
                               TRUE ~ 0),
         inc_honey2 = case_when(`401_l`==1 ~ 1,
                                TRUE ~ 0),
         inc_tradeother2 = case_when(`401_m`==1 ~ 1,
                                     TRUE ~ 0),
         inc_tradeown2 = case_when(`401_n`==1 ~ 1,
                                   TRUE ~ 0),
         inc_selfag2 = case_when(`401_o`==1 ~ 1,
                                 TRUE ~ 0),
         inc_selfnonag2 = case_when(`401_p`==1 ~ 1,
                                    TRUE ~ 0),
         inc_rent2 = case_when(`401_q`==1 ~ 1,
                               TRUE ~ 0),
         inc_remit2 = case_when(`401_r`==1 ~ 1,
                                TRUE ~ 0),
         inc_inherit2 = case_when(`401_s`==1 ~ 1,
                                  TRUE ~ 0),
         inc_assist2 = case_when(`401_t`==1 ~ 1,
                                 TRUE ~ 0),
         inc_other2 = case_when(`401_u`==1 ~ 1,
                                TRUE ~ 0))

frq(d$inc_farm2)

d <- d %>%
  mutate(inc_pattern = paste(inc_farm2, inc_cattle2, inc_goats2, inc_sheep2, inc_fishing2, inc_agwageout2,
                             inc_agwagein2, inc_nonagout2, inc_nonagin2, inc_salary2, inc_bush2, inc_honey2,
                             inc_tradeother2, inc_tradeown2, inc_selfag2, inc_selfnonag2, inc_rent2,
                             inc_remit2, inc_inherit2, inc_assist2, inc_other2, sep="-"))

frq(d$inc_pattern)

names(d)

# income ranks ---- 

frq(d$`402_b`)
frq(d$inc_cattle)

d <- d %>%
  mutate(farm_rank = case_when(inc_farm2==0 ~ 0,
                               `402_a`==1 ~ 3,
                               `402_a`==2 ~ 2,
                               `402_a`==3 ~ 1,
                               TRUE ~ 0),
         cattle_rank = case_when(inc_cattle2==0 ~ 0,
                               `402_b`==1 ~ 3,
                               `402_b`==2 ~ 2,
                               `402_b`==3 ~ 1,
                               TRUE ~ 0),
         goats_rank = case_when(inc_goats2==0 ~ 0,
                               `402_c`==1 ~ 3,
                               `402_c`==2 ~ 2,
                               `402_c`==3 ~ 1,
                               TRUE ~ 0),
         sheep_rank = case_when(inc_sheep2==0 ~ 0,
                               `402_d`==1 ~ 3,
                               `402_d`==2 ~ 2,
                               `402_d`==3 ~ 1,
                               TRUE ~ 0),
         fishing_rank = case_when(inc_fishing2==0 ~ 0,
                               `402_e`==1 ~ 3,
                               `402_e`==2 ~ 2,
                               `402_e`==3 ~ 1,
                               TRUE ~ 0),
         agwageout_rank = case_when(inc_agwageout2==0 ~ 0,
                               `402_f`==1 ~ 3,
                               `402_f`==2 ~ 2,
                               `402_f`==3 ~ 1,
                               TRUE ~ 0),
         agwagein_rank = case_when(inc_agwagein2==0 ~ 0,
                               `402_g`==1 ~ 3,
                               `402_g`==2 ~ 2,
                               `402_g`==3 ~ 1,
                               TRUE ~ 0),
         nonagout_rank = case_when(inc_nonagout2==0 ~ 0,
                               `402_h`==1 ~ 3,
                               `402_h`==2 ~ 2,
                               `402_h`==3 ~ 1,
                               TRUE ~ 0),
         nonagin_rank = case_when(inc_nonagin2==0 ~ 0,
                               `402_i`==1 ~ 3,
                               `402_i`==2 ~ 2,
                               `402_i`==3 ~ 1,
                               TRUE ~ 0),
         salary_rank = case_when(inc_salary2==0 ~ 0,
                               `402_j`==1 ~ 3,
                               `402_j`==2 ~ 2,
                               `402_j`==3 ~ 1,
                               TRUE ~ 0),
         bush_rank = case_when(inc_bush2==0 ~ 0,
                               `402_k`==1 ~ 3,
                               `402_k`==2 ~ 2,
                               `402_k`==3 ~ 1,
                               TRUE ~ 0),
         honey_rank = case_when(inc_honey2==0 ~ 0,
                               `402_l`==1 ~ 3,
                               `402_l`==2 ~ 2,
                               `402_l`==3 ~ 1,
                               TRUE ~ 0),
         tradeother_rank = case_when(inc_tradeother2==0 ~ 0,
                               `402_m`==1 ~ 3,
                               `402_m`==2 ~ 2,
                               `402_m`==3 ~ 1,
                               TRUE ~ 0),
         tradeown_rank = case_when(inc_tradeown2==0 ~ 0,
                               `402_n`==1 ~ 3,
                               `402_n`==2 ~ 2,
                               `402_n`==3 ~ 1,
                               TRUE ~ 0),
         selfag_rank = case_when(inc_selfag2==0 ~ 0,
                               `402_o`==1 ~ 3,
                               `402_o`==2 ~ 2,
                               `402_o`==3 ~ 1,
                               TRUE ~ 0),
         selfnonag_rank = case_when(inc_selfnonag2==0 ~ 0,
                               `402_p`==1 ~ 3,
                               `402_p`==2 ~ 2,
                               `402_p`==3 ~ 1,
                               TRUE ~ 0),
         rent_rank = case_when(inc_rent2==0 ~ 0,
                               `402_q`==1 ~ 3,
                               `402_q`==2 ~ 2,
                               `402_q`==3 ~ 1,
                               TRUE ~ 0),
         remit_rank = case_when(inc_remit2==0 ~ 0,
                               `402_r`==1 ~ 3,
                               `402_r`==2 ~ 2,
                               `402_r`==3 ~ 1,
                               TRUE ~ 0),
         inherit_rank = case_when(inc_inherit2==0 ~ 0,
                               `402_s`==1 ~ 3,
                               `402_s`==2 ~ 2,
                               `402_s`==3 ~ 1,
                               TRUE ~ 0),
         assist_rank = case_when(inc_assist2==0 ~ 0,
                               `402_t`==1 ~ 3,
                               `402_t`==2 ~ 2,
                               `402_t`==3 ~ 1,
                               TRUE ~ 0),
         other_rank = case_when(inc_other2==0 ~ 0,
                               `402_u`==1 ~ 3,
                               `402_u`==2 ~ 2,
                               `402_u`==3 ~ 1,
                               TRUE ~ 0))
         
   
frq(d$cattle_rank)
frq(d$inc_farm)
frq(d$farm_rank)         

#lapply(d[,195:215], frq)


# Household Hunger Scale (HHS) ---- 

frq(d$`433`)

d <- d %>%
  mutate(hhs1 = case_when(`430`==1 ~ 1,
                          `430`==2 ~ 0,
                          TRUE ~ NA_real_),
         hhs2 = case_when(`430`==2 ~ 0,
                          `431`<3 ~ 1,
                          `431`==3 ~ 2,
                          TRUE ~ NA_real_),
         hhs3 = case_when(`432`==1 ~ 1,
                          `432`==2 ~ 0,
                          TRUE ~ NA_real_),
         hhs4 = case_when(`432`==2 ~ 0,
                          `433`<3 ~ 1,
                          `433`==3 ~ 2,
                          TRUE ~ NA_real_),
         hhs5 = case_when(`434`==1 ~ 1,
                          `434`==2 ~ 0,
                          TRUE ~ NA_real_),
         hhs6 = case_when(`434`==2 ~ 0,
                          `435`<3 ~ 1,
                          `435`==3 ~ 2,
                          TRUE ~ NA_real_),
         hhs = hhs2 + hhs4 + hhs6,
         hhs_ord = case_when(hhs<2 ~ 0,
                             hhs>1 & hhs<4 ~ 1,
                             hhs>3 ~ 2,
                             TRUE ~ NA_real_),
         hhs_severe = ifelse(hhs>3, 1,0))

frq(d$hhs)
frq(d$hhs_ord)
frq(d$hhs_severe)


# ggplot(d, aes(x=hhs)) + 
#   geom_bar(width=.4, fill="dodgerblue2", alpha=.6) +
#   scale_x_continuous(breaks=0:6) + 
#   theme(axis.title.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   labs(x="",
#        y="",
#        title="Household Hunger Scale")

#hhs_county <- dat %>%
#  group_by(county)

describe(dat$hhs)


frq(d$`436`)

# Shocks ---- 

shock_key

  # binary

d <- d %>%
  mutate(floods_bin = case_when(`436` ==1 ~ 1,
                                `436` == 2 ~ 0,
                                TRUE ~ 0),
         drought_bin = case_when(`439` ==1 ~ 1,
                                `439` == 2 ~ 0,
                                TRUE ~ 0),
         erosion_bin = case_when(`442` ==1 ~ 1,
                                `442` == 2 ~ 0,
                                TRUE ~ 0),
         land_bin = case_when(`445` ==1 ~ 1,
                             `445` == 2 ~ 0,
                             TRUE ~ 0),
         food_bin = case_when(`448` ==1 ~ 1,
                             `448` == 2 ~ 0,
                             TRUE ~ 0),
         theft_bin = case_when(`451` ==1 ~ 1,
                              `451` == 2 ~ 0,
                              TRUE ~ 0))



  # severity

d <- d %>%
  mutate(floods_fs = case_when(is.na(`436`) ~ 0,
                            is.na(`437`) ~ 0,
                            TRUE ~ `437`),
         drought_fs = case_when(is.na(`439`) ~ 0,
                            is.na(`440`) ~ 0,
                            TRUE ~ `440`),
         erosion_fs = case_when(is.na(`442`) ~ 0,
                            is.na(`443`) ~ 0,
                            TRUE ~ `443`),
         land_fs = case_when(is.na(`445`) ~ 0,
                            is.na(`446`) ~ 0,
                            TRUE ~ `446`),
         food_fs = case_when(is.na(`448`) ~ 0,
                            is.na(`449`) ~ 0,
                            TRUE ~ `449`),
         theft_fs = case_when(is.na(`451`) ~ 0,
                            is.na(`452`) ~ 0,
                            TRUE ~ `452`))
         # cropinputs_fs = case_when(is.na(`455`) ~ 0,
         #                    is.na(`456`) ~ 0,
         #                    TRUE ~ `456`),
         # cropdisease_fs = case_when(is.na(`458`) ~ 0,
         #                    is.na(`459`) ~ 0,
         #                    TRUE ~ `459`),
         # croppests_fs = case_when(is.na(`461`) ~ 0,
         #                    is.na(`462`) ~ 0,
         #                    TRUE ~ `462`),
         # croptheft_fs = case_when(is.na(`464`) ~ 0,
         #                    is.na(`465`) ~ 0,
         #                    TRUE ~ `465`),
         # livestockinputs_fs = case_when(is.na(`468`) ~ 0,
         #                    is.na(`469`) ~ 0,
         #                    TRUE ~ `469`),
         # livestockdisease_fs = case_when(is.na(`471`) ~ 0,
         #                    is.na(`472`) ~ 0,
         #                    TRUE ~ `472`),
         # livestocktheft_fs = case_when(is.na(`474`) ~ 0,
         #                    is.na(`475`) ~ 0,
         #                    TRUE ~ `475`),
         # nosell_fs = case_when(is.na(`477`) ~ 0,
         #                    is.na(`478`) ~ 0,
         #                    TRUE ~ `478`),
         # illness_fs = case_when(is.na(`480`) ~ 0,
         #                    is.na(`481`) ~ 0,
         #                    TRUE ~ `481`),
         # death_fs = case_when(is.na(`484`) ~ 0,
         #                    is.na(`485`) ~ 0,
         #                    TRUE ~ `485`))

d <- d %>%
  mutate(shocks_sev = floods_fs + drought_fs + erosion_fs + land_fs + food_fs + theft_fs)

shock_labs

d %>%
  dplyr::select(floods_fs:theft_fs) %>%
  lapply(., frq)

# Resilience ---- 

frq(d$`487`)

d <- d %>%
  mutate(resil1 = ifelse(is.na(`487`), 3, `487`),
         resil2 = ifelse(is.na(`488`), 3, `488`),
         resil3 = ifelse(is.na(`489`), 3, `489`),
         resil4 = ifelse(is.na(`490`), 3, `490`),
         resil5 = ifelse(is.na(`491`), 3, `491`),
         resil6 = ifelse(is.na(`492`), 3, `492`),
         resil7 = ifelse(is.na(`493`), 3, `493`),
         resil8 = ifelse(is.na(`494`), 3, `494`))

frq(d$resil2)

d <- d %>%
  mutate(resil1_cen = resil1 - 3,
         resil2_cen = resil2 - 3,
         resil3_cen = resil3 - 3,
         resil4_cen = resil4 - 3,
         resil5_cen = resil5 - 3,
         resil6_cen = resil6 - 3,
         resil7_cen = resil7 - 3,
         resil8_cen = resil8 - 3)


frq(d$resil1_cen)

d <- d %>%
  mutate(resil1_bin = ifelse(resil1>3, 1,0),
         resil2_bin = ifelse(resil2>3, 1,0),
         resil3_bin = ifelse(resil3>3, 1,0),
         resil4_bin = ifelse(resil4>3, 1,0),
         resil5_bin = ifelse(resil5>3, 1,0),
         resil6_bin = ifelse(resil6>3, 1,0),
         resil7_bin = ifelse(resil7>3, 1,0),
         resil8_bin = ifelse(resil8>3, 1,0))

res <- d %>%
  dplyr::select(id, resil1:resil8)

head(res)

lapply(res[,2:9], frq)

res %>%
  dplyr::select(2:9) %>%
  fa.parallel(cor="poly")

res2 <- fa(res[,2:9],
           cor="poly",
           nfactors=2)

res2
# capacities
# capital


res2_scores <- data.frame(res2$scores) %>%
  set_names(nm=c("Capacities_fac2","Capital_fac2"))

head(res2_scores)
describe(res2_scores)

d <- d %>%
  bind_cols(res2_scores)


# Development assistance ---- 

frq(d$`504`) %>%
  as.data.frame() %>%
  gt() %>%
  gtsave("output/tables/q504.html")


# Confidence in sub-national institutions ---- 

frq(d$`504`)
frq(d$`508`)
frq(d$`509`)
q509a <- data.frame(frq(d$`509_a`))
q509a

frq(d$`510`)

d <- d %>%
  mutate(donor_act = case_when(`504`==1 ~ 1,
                               TRUE ~ 0),
         donor_oversee = case_when(is.na(`504`) ~ NA_real_, # 2 cases that should have been NA
                                   `508`==1 ~ 1,
                                   `508`==2 ~ 0,
                                   TRUE ~ NA_real_),
         oversee_conf = case_when(`510`==1 ~ -2,
                                  `510`==2 ~ -1,
                                  `510`==3 ~ 1,
                                  `510`==4 ~ 2,
                                  is.na(`504`) ~ 0,
                                  is.na(`510`) ~ 0))


frq(d$donor_act)
frq(d$donor_oversee)
frq(d$oversee_conf)

# Natural resource management ---- 

frq(d$`511`)
frq(d$`512_1`)
frq(d$`513`)

d <- d %>%
  mutate(resource_govern = case_when(`511`== 1 ~ 1,
                                     TRUE ~ 0),
         govern_plants = case_when(`512_1` == 1 ~ 1,
                                      TRUE ~ 0),
         govern_land = case_when(`512_2` == 1 ~ 1,
                                   TRUE ~ 0),
         govern_water = case_when(`512_3` == 1 ~ 1,
                                   TRUE ~ 0),
         govern_fish = case_when(`512_4` == 1 ~ 1,
                                   TRUE ~ 0),
         govern_minerals = case_when(`512_5` == 1 ~ 1,
                                   TRUE ~ 0),
         govern_forests = case_when(`512_6` == 1 ~ 1,
                                   TRUE ~ 0),
         govern_livestock = case_when(`512_7` == 1 ~ 1,
                                   TRUE ~ 0),
         govern_other = case_when(`512_8` == 1 ~ 1,
                                   TRUE ~ 0),
         resource_conf = case_when(`513`==1 ~ -2,
                                   `513`==2 ~ -1,
                                   `513`==3 ~ 1,
                                   `513`==4 ~ 2,
                                   is.na(`513`) ~ 0))



frq(d$resource_govern)
frq(d$govern_plants)
frq(d$govern_land)
frq(d$resource_conf)

# Conflict ---- 

frq(d$`601`)
frq(d$`602_1`)

d <- d %>%
  mutate(conflict=case_when(`601`==1 ~ 1,
                            TRUE ~ 0),
         conflict_land = case_when(`602_1`==1 ~ 1,
                                    TRUE ~ 0),
         conflict_water = case_when(`602_2`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_pasture = case_when(`602_3`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_forestry = case_when(`602_4`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_cattle = case_when(`602_5`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_goat = case_when(`602_6`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_migration = case_when(`602_7`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_borders = case_when(`602_8`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_revenge = case_when(`602_9`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_dowry = case_when(`602_10`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_elope = case_when(`602_11`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_cattleraid = case_when(`602_12`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_fishing = case_when(`602_13`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_gbv = case_when(`602_14`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_livelihood = case_when(`602_15`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_ag = case_when(`602_16`==1 ~ 1,
                                   TRUE ~ 0),
         conflict_other = case_when(`602_17`==1 ~ 1,
                                   TRUE ~ 0))



frq(d$conflict)
frq(d$conflict_land)

frq(d$`606`)

?set_labels


d <- d %>%
  mutate(conflict_sev = `603` - 1,
         conflict_sev = case_when(conflict_sev>6 ~ 0,
                                  is.na(conflict_sev) ~ 0,
                                  TRUE ~ conflict_sev),
         how_resolved=set_labels(`605`, labels = resolve_labs),
         satis_elders = set_labels(`606`, labels=satis_labs))

frq(d$satis_elders)


# Emergency action plans ---- 

frq(d$`611`)

d <- d %>%
  mutate(emerg_plan = case_when(`610`==1 ~ 1,
                                TRUE ~ 0),
         emerg_targeted = case_when(`611`==1 ~ 1,
                                    `611`==2 ~ 0,
                                    TRUE ~ NA_real_),
         emerg_effective = `612` - 1)
         # emerg_effectve = case_when(is.na(emerg_effective) ~ 0),
         # emerg_effective = case_when(is.na(emerg_plan) ~ NA_real_,
         #                             TRUE ~ emerg_effective))
         

frq(d$emerg_plan)
frq(d$emerg_targeted)
frq(d$emerg_effective)

# Aspirations ---- 

frq(d$`629`)

d <- d %>%
  mutate(asp1_miss = ifelse(is.na(`629`), 1,0),
         asp2_miss = ifelse(is.na(`630`), 1,0),
         asp3_miss = ifelse(is.na(`634`), 1,0),
         asp4_miss = ifelse(is.na(`635`), 1,0),
         asp5_miss = ifelse(is.na(`632`), 1,0),
         asp6_miss = ifelse(is.na(`633`), 1,0),
         asp1 = case_when(`629` == 1 ~ 1,
                           TRUE ~ 0),
         asp2 = case_when(`630` == 1 ~ 1,
                          TRUE ~ 0),
         asp3 = case_when(`634` < 4 ~ 1,
                         `634` > 3 ~0,
                         TRUE ~ 0),
         asp4 = case_when(`635` < 4 ~ 1,
                          `635` > 3 ~ 0,
                          TRUE ~ 0),
         asp5 = case_when(`632` == 1 ~ 1,
                          TRUE ~ 0),
         asp6 = case_when(`633` == 6 ~ 0,
                          `633`==4 | `633` ==5 ~ 1,
                          TRUE ~ 0))

frq(d$`633`)         
frq(d$asp6)
         
# asp1 = ifelse(`629`==1, 1,0),
#          asp2 = ifelse(`630`==1, 1,0),
#          asp3 = case_when(`634` < 4 ~ 1,
#                           `634` > 3 ~0,
#                           TRUE ~ NA_real_),
#          asp4 = case_when(`635` < 4 ~ 1,
#                           `635` > 3 ~ 0,
#                           TRUE ~ NA_real_),
#          asp5 = ifelse(`632`==1, 1,0),
#          asp6 = ifelse(`633`==6, NA,
#                        ifelse(`633`==4 | `633`==5, 1, 0))
#          )

frq(d$asp1_miss)
frq(d$asp1)

asp <- d %>%
  dplyr::select(asp1:asp6)

lapply(asp, frq)

names(d)

d %>%
  dplyr::select(asp1_miss:asp6_miss) %>%
  lapply(., frq)

asp_impute <- imputePCA(asp,
                        method="regularized")
asp_impute

asp_imputed <- asp_impute$completeObs %>%
  as.data.frame()

lapply(asp_imputed, mean)

# Trafficking in Persons ---- 

frq(d$`829`)

d <- d %>%
  mutate(traffic_unaccept = ifelse(`829`==6, 1,0))

frq(d$traffic_unaccept)

# save prepared file ---- 

getwd()

write_rds(d, "data/prepared/South Sudan resilience panel survey - prepared.rds")
write_csv(d, "data/prepared/South Sudan resilience panel survey - prepared.csv")





