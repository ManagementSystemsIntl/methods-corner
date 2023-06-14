# South Sudan resilience
# explore

dat <- read_rds("data/prepared/South Sudan resilience panel survey - prepared.rds")

# income sources ---- 

inc <- dat %>%
  dplyr::select(inc_farm:inc_other) %>%
  describe() %>%
  as.data.frame() %>%
  rownames_to_column(var="var") %>%
  mutate(lab = c(inc_labs, "Other"),
         lower=mean - 1.96*se,
         upper=mean + 1.96*se,
         lower=ifelse(lower<0,0, lower),
         upper=ifelse(upper>1, 1, upper)) %>%
  dplyr::select(lab, var, n, mean, lower, upper) %>%
  arrange(desc(mean))

inc2 <- dat %>%
  dplyr::select(inc_farm2:inc_other2) %>%
  describe() %>%
  as.data.frame() %>%
  rownames_to_column(var="var") %>%
  mutate(lab = c(inc_labs, "Other"),
         lower=mean - 1.96*se,
         upper=mean + 1.96*se,
         lower=ifelse(lower<0,0, lower),
         upper=ifelse(upper>1, 1, upper)) %>%
  dplyr::select(lab, var, n, mean, lower, upper) %>%
  arrange(desc(mean))

inc2

write_csv(inc2, "output/tables/Income sources.csv")

ggplot(inc2, aes(mean, reorder(lab, mean))) + 
  geom_point() + 
  geom_errorbar(aes(xmin=lower, xmax=upper),
                width=0, color=usaid_blue, size=1) +
  geom_label(aes(label=paste(round(mean*100,1), "%", sep="")),
             color=usaid_blue, size=3) + 
  scale_x_continuous(limits=c(-.05, 1.05),
                     breaks=seq(0,1,.2),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="")

ggsave(here("output/viz/income sources 2.png"),
       device="png",
       type="cairo",
       height=6,
       width=7)


inc_gt <- inc2 %>%
  dplyr::select(`Income source` = lab, Percent=mean) %>%
  mutate(Bar=Percent*100) %>%
  gt() %>%
  gt_plt_bar_pct(column=Bar, fill=usaid_blue, background=light_grey, scaled=T) %>%
  cols_width(3 ~ px(125)) %>%
  fmt_percent(2, decimals=0) %>%
  cols_label(Bar="")

inc_gt

gtsave(inc_gt, "output/tables/Income sources.html")

# income rank ---- 

# rnk <- dat %>%
#   dplyr::select(id, farm_rank:other_rank)
# 
# names(rnk)
# str(rnk)
# head(rnk)
# lapply(rnk[,-1], frq)
# 
# rnk2 <- rnk %>%
#   dplyr::select(c(-1, -10, -18, -22)) 
# 
# # fa.parallel(rnk2,
# #             cor="poly")         
# 
# describe(rnk)
# 
# ?fa
# 
# out <- fa(rnk2)
# out
# 
# pc2 <- pca(rnk2,
#            nfactors=2)
# pc2



# Household Hunger ---- 

hhs <- dat %>%
  select(hhs1, hhs3, hhs5, hhs_severe) %>%
  describe() %>%
  as.data.frame() %>%
  mutate(lower=mean-1.96*se,
         upper=mean+1.96*se,
         `In previous four weeks..`=c("Lack of resources to get food",
                                      "Went to sleep hungry",
                                      "Whole day without eating",
                                      #"Household Hunger Scale (0-6)",
                                      "Severe household hunger")) %>%
  dplyr::select(last_col(), Percent=mean, lower, upper, everything()) 

#%>%
#  arrange(desc(Percent))
hhs

write_csv(hhs, "output/tables/Household hunger scale.csv")

hhs_gt <- hhs %>%
  dplyr::select(1, Percent) %>%
  mutate(Bar=Percent*100) %>%
  gt() %>%
  gt_plt_bar_pct(column=Bar, fill=usaid_blue, background=light_grey, scaled=T) %>%
  cols_width(3 ~ px(125)) %>%
  fmt_percent(2, decimals=0) %>%
  cols_label(Bar="")

hhs_gt

gtsave(hhs_gt, "output/tables/Household hunger scale.html")




ggplot(dat, aes(x=hhs)) + 
  geom_bar(width=.4, fill="dodgerblue2", alpha=.8) +
  scale_x_continuous(breaks=0:6) + 
  theme(axis.text.y=element_blank()) +
  labs(x="",
       y="",
       title="Household Hunger Scale")

ggsave("output/viz/household hunger bar.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


describe(dat$hhs)
frq(dat$hhs)
names(dat)

  # HHS by county 


hhs_cnty <- d %>%
  group_by(county) %>%
  summarise(se=std.error(hhs),
            hhs=mean(hhs, na.rm=T),
            n=n()) %>%
  arrange(desc(hhs)) %>%
  mutate(lower=hhs - 1.96*se, 
         upper=hhs + 1.96*se)

hhs_cnty

write_csv(hhs_cnty, "output/tables/hhs county.csv")

#ggplot(TIP_accept_cnty, aes(x=traffic_accept, y=county)) +
#  geom_point() + 
#  geom_errorbar(aes(xmin=lower, xmax=upper))


hhs_sev_cnty <- d %>%
  group_by(county) %>%
  summarise(se=std.error(hhs_severe),
            hhs_severe=mean(hhs_severe, na.rm=T),
            n=n()) %>%
  mutate(lower=hhs_severe-1.96*se,
         upper=hhs_severe+1.96*se) %>%
  arrange(desc(hhs_severe))

hhs_sev_cnty

write_csv(hhs_sev_cnty, "output/tables/hhs severe county.csv")

hhs_sev_cnty_gt <- hhs_sev_cnty %>%
  dplyr::select(county, hhs_severe) %>%
#  mutate(Bar=hhs_severe*100) %>%
  gt() %>%
#%>%
#  gt_plt_bar_pct(column=Bar, fill=usaid_blue, background=light_grey, scaled=T) %>%
#  cols_width(3 ~ px(125)) %>%
  fmt_percent(2, decimals=1) 
  #%>%
#  cols_label(
#    Bar="")

hhs_sev_cnty_gt

gtsave(hhs_cnty_gt, "output/tables/hhs severe county.docx")




# Shocks ---- 

ggplot(dat, aes(shocks_sev)) + 
#  geom_vline(xintercept=mean(dat$shocks_sev), color="darkgoldenrod2", alpha=.6, size=2) +
  geom_bar(fill="dodgerblue", alpha=.8, width=.4) + 
  geom_vline(xintercept=mean(dat$shocks_sev), color="darkgoldenrod2", alpha=.6, size=2) +
  scale_x_continuous(limits=c(0,24),
                     breaks=seq(0,24,2)) + 
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x="",
       y="",
       title="Exposure to shocks",
       caption="Sum of six shocks,\neach with severity 0-4")

frq(dat$shocks_sev)

ggsave("output/viz/Exposure to shocks.png",
       device="png",
       type="cairo",
       height=4,
       width=7)

  # binary

shk <- dat %>%
  dplyr::select(floods_bin:theft_bin) %>%
  describe() %>%
  as.data.frame %>%
  mutate(Shock = c("Floods","Drought","Erosion","Loss of land","Increase in food prices","Theft"),
         lower=mean-1.96*se,
         upper=mean+1.96*se) %>%
  dplyr::select(Shock, Percent=mean, lower, upper, everything()) %>%
  arrange(desc(Percent))

shk
str(shk)

write_csv(shk, "output/tables/Shock incidence.csv")

shk %>%
#  dplyr::select(Shock, Percent) %>%
  #mutate(Bar=Percent*100) %>%
  gt() %>%
  gtsave("output/tables/Shock incidence.docx")
  

ggplot(shk, aes(Percent, reorder(Shock, Percent))) + 
  geom_errorbar(aes(xmin=lower, xmax=upper),
                color=usaid_blue, size=1, width=0) + 
  geom_label(aes(label=paste(round(Percent*100,1), "%", sep="")),
             color=usaid_blue) +
  scale_x_continuous(labels=percent_format(accuracy=1),
                     breaks=seq(0,1,.2),
                     limits=c(0,1)) +
  labs(x="",
       y="",
       title="Incidence of shocks in previous 12 months")


shk_gt <- shk %>%
  dplyr::select(Shock, Percent) %>%
  mutate(Bar=Percent*100) %>%
  gt() %>%
  gt_plt_bar_pct(column=Bar, fill=usaid_blue, background=light_grey, scaled=T) %>%
  cols_width(3 ~ px(125)) %>%
  fmt_percent(2, decimals=0) %>%
  cols_label(Bar="")

shk_gt

gtsave(shk_gt, "output/tables/Shock incidence.html")

  # by county

shocks_sev_cnty <- d %>%
  group_by(County=county) %>%
  summarise(se=std.error(shocks_sev),
            shocks_sev=mean(shocks_sev),
            n=n()) %>%
  arrange(desc(shocks_sev)) %>%
  mutate(lower=shocks_sev - 1.96*se, 
         upper=shocks_sev + 1.96*se,
         lower=ifelse(lower<0, 0, lower),
         upper=ifelse(upper>100, 100, upper))

shocks_sev_cnty

write_csv(shocks_sev_cnty, "output/tables/Shock severity county.csv")

shocks_sev_cnty_gt <- shocks_sev_cnty %>%
  dplyr::select(County, shocks_sev) %>%
  gt() 
#%>%
#  fmt_percent(2:4, decimals=0) 

shocks_sev_cnty_gt
gtsave(shocks_sev_cnty_gt, "output/tables/Shock severity county.docx")

# Resilience ---- 

resil <- dat %>%
  dplyr::select(resil1_bin:resil8_bin) %>%
  describe() %>%
  as.data.frame %>%
  mutate(Category = resil_labs,
         Resilience = resil_items,
         lower=mean-1.96*se,
         upper=mean+1.96*se) %>%
  dplyr::select(Category, Resilience, Percent=mean, lower, upper, everything()) %>%
  arrange(desc(Percent))

resil

write_csv(resil, "output/tables/Resilience binaries.csv")

resil_gt <- resil %>%
  dplyr::select(Category, Resilience, Percent) %>%
  mutate(Bar=Percent*100) %>%
  gt() %>%
  gt_plt_bar_pct(column=Bar, fill=usaid_blue, background=light_grey, scaled=T) %>%
  cols_width(4 ~ px(125)) %>%
  fmt_percent(3, decimals=0) %>%
  cols_label(Bar="")

resil_gt

gtsave(resil_gt, "output/tables/Resilience binaries.rtf")

# Development assistance ---- 

dev <- d %>%
  dplyr::select(donor_act, donor_oversee, oversee_conf_bin) %>%
  describe() %>%
  as.data.frame %>%
  rownames_to_column("Item") %>%
  mutate(#Item = emerg_labs,
    lower=mean-1.96*se,
    upper=mean+1.96*se) %>%
  dplyr::select(#Item, 
    Percent=mean, lower, upper, everything()) 
#  arrange(desc(Percent))

dev

write_csv(dev, "output/tables/Development assistance binaries.csv")

dev_gt <- dev %>%
  dplyr::select(Item, Percent) %>%
  gt() %>%
  fmt_percent(2, decimals=0)

dev_gt

gtsave(dev_gt, "output/tables/Development assistance binaries.docx")


# Natural resource management ---- 

resrc <- d %>%
  dplyr::select(resource_govern:govern_other, resource_conf_bin) %>%
  describe() %>%
  as.data.frame %>%
  rownames_to_column("Item") %>%
  mutate(#Item = emerg_labs,
    lower=mean-1.96*se,
    upper=mean+1.96*se) %>%
  dplyr::select(#Item, 
    Percent=mean, lower, upper, everything()) 
#  arrange(desc(Percent))

resrc

write_csv(resrc, "output/tables/Natural resource management binaries.csv")

resrc_gt <- resrc %>%
  dplyr::select(Item, Percent) %>%
  gt() %>%
  fmt_percent(2, decimals=0)

resrc_gt

gtsave(resrc_gt, "output/tables/Natural resource management binaries.docx")


# Conflict ---- 

frq(d$conflict)

conf <- d %>%
  dplyr::select(conflict:conflict_other,
                conflict_sev_bin, satis_elders_bin) %>%
  describe() %>%
  as.data.frame %>%
  rownames_to_column("Item") %>%
  mutate(#Item = emerg_labs,
         lower=mean-1.96*se,
         upper=mean+1.96*se) %>%
  dplyr::select(#Item, 
                Percent=mean, lower, upper, everything()) 
#  arrange(desc(Percent))

conf

write_csv(conf, "output/tables/Conflict binaries.csv")

conf_gt <- conf %>%
  dplyr::select(Item, Percent) %>%
  gt() %>%
  fmt_percent(2, decimals=0)

conf_gt

gtsave(conf_gt, "output/tables/Conflict binaries.docx")

# Community action plans ---- 

frq(d$emerg_plan)
frq(d$emerg_targeted)
frq(d$emerg_effective)

emerg_labs <- c("Community has emergency action plan",
                "Plan addresses at least one shock that affects community",
                "Plan is effective")

emerg <- d %>%
  dplyr::select(emerg_plan, emerg_targeted, emerg_effective_bin) %>%
  describe() %>%
  as.data.frame %>%
  mutate(Item = emerg_labs,
         lower=mean-1.96*se,
         upper=mean+1.96*se) %>%
  dplyr::select(Item, Percent=mean, lower, upper, everything()) 
#  arrange(desc(Percent))

emerg

write_csv(emerg, "output/tables/Community action plan binaries.csv")

emerg_gt <- emerg %>%
  dplyr::select(Item, Percent) %>%
  gt() %>%
  fmt_percent(2, decimals=0)

emerg_gt

gtsave(emerg_gt, "output/tables/Community action plan binaries.docx")

  # by county

emerg_cnty <- d %>%
  group_by(County=county) %>%
  summarise(across(c(emerg_plan, emerg_targeted, emerg_effective_bin), \(x) mean(x, na.rm=T)))

  #                  list(mean=mean, se=se)))
  #           
  #           
  #   se=std.error(emerg_planaspirations_comp_resc),
  #           asp_resc=mean(aspirations_comp_resc),
  #           n=n()) %>%
  # arrange(desc(asp_resc)) %>%
  # mutate(lower=asp_resc - 1.96*se, 
  #        upper=asp_resc + 1.96*se,
  #        lower=ifelse(lower<0, 0, lower),
  #        upper=ifelse(upper>100, 100, upper))

emerg_cnty

write_csv(emerg_cnty, "output/tables/Emergency action plans county.csv")

emerg_cnty_gt <- emerg_cnty %>%
  #dplyr::select(County, emerg_resc) %>%
  gt() %>%
  fmt_percent(2:4, decimals=0) 

emerg_cnty_gt
gtsave(emerg_cnty_gt, "output/tables/Emergency action plans county.docx")


# Aspirations ---- 

asp_labs <- c("Each person is primarily responsible for is/her success or failure in life",
              "To be successful, above all one needs to work very hard",
              "My experience in life has been that what is going to happen will happen (disagree)",
              "Things turn out to be a matter of good or bad fortune (disagree)",
              "Hopeful for children's future",
              "Desire at least secondary school education for children")

asp <- dat %>%
  dplyr::select(asp1:asp6) %>%
  describe() %>%
  as.data.frame %>%
  mutate(Aspiration = asp_labs,
         lower=mean-1.96*se,
         upper=mean+1.96*se) %>%
  dplyr::select(Aspiration, Percent=mean, lower, upper, everything()) %>%
  arrange(desc(Percent))

asp

write_csv(asp, "output/tables/Aspirations binaries.csv")

asp_gt <- asp %>%
  dplyr::select(Aspiration, Percent) %>%
  mutate(Bar=Percent*100) %>%
  gt() %>%
  gt_plt_bar_pct(column=Bar, fill=usaid_blue, background=light_grey, scaled=T) %>%
  cols_width(3 ~ px(125)) %>%
  fmt_percent(2, decimals=0) %>%
  cols_label(Bar="")

asp_gt

gtsave(asp_gt, "output/tables/Aspiration binaries.html")

  # by county

asp_resc_cnty <- d %>%
  group_by(County=county) %>%
  summarise(se=std.error(aspirations_comp_resc),
            asp_resc=mean(aspirations_comp_resc),
            n=n()) %>%
  arrange(desc(asp_resc)) %>%
  mutate(lower=asp_resc - 1.96*se, 
         upper=asp_resc + 1.96*se,
         lower=ifelse(lower<0, 0, lower),
         upper=ifelse(upper>100, 100, upper))

asp_resc_cnty

write_csv(asp_resc_cnty, "output/tables/Aspirations rescaled county.csv")

asp_resc_cnty_gt <- asp_resc_cnty %>%
  dplyr::select(County, asp_resc) %>%
  gt() %>%
  fmt_percent(2, decimals=0) 

asp_resc_cnty_gt
gtsave(asp_resc_cnty_gt, "output/tables/Aspirations rescaled county.docx")


# Agency ---- 




# Sexual and gender based violence ---- 

q812 <- data.frame(frq(d$`812`)) %>%
  filter(!is.na(val)) %>%
  mutate(gbv_lab=gbv_labs,
         Percent=frq/sum(frq))

q812  

write_csv(q812, "output/tables/q812.csv")

q812_gt <- q812 %>%
  dplyr::select(gbv_lab, Percent) %>%
#  mutate(Bar=Percent*100) %>%
  arrange(desc(Percent)) %>%
  gt() %>%
#  gt_plt_bar_pct(column=Bar, fill=usaid_blue, background=light_grey, scaled=T) %>%
#  cols_width(3 ~ px(125)) %>%
  fmt_percent(2, decimals=0) %>%
  cols_label(gbv_lab="Acceptability of violence")
#             Bar="")

q812_gt

gtsave(q812_gt, "output/tables/q812.docx")


frq(d$gbv_accept)

  # by county

gbv_accept_cnty <- d %>%
  group_by(County=county) %>%
  summarise(se=std.error(gbv_accept),
            gbv_accept=mean(gbv_accept),
            n=n()) %>%
  arrange(desc(gbv_accept)) %>%
  mutate(lower=gbv_accept - 1.96*se, 
         upper=gbv_accept + 1.96*se,
         lower=ifelse(lower<0, 0, lower),
         upper=ifelse(upper>1, 1, upper))

gbv_accept_cnty

write_csv(gbv_accept_cnty, "output/tables/Gender based violence acceptance county.csv")

gbv_accept_cnty_gt <- gbv_accept_cnty %>%
  dplyr::select(County, gbv_accept) %>%
  #mutate(Bar=traffic_accept*100) %>%
  gt() %>%
  #gt_plt_bar_pct(column=Bar, fill=usaid_blue, background=light_grey, scaled=T) %>%
  #cols_width(3 ~ px(125)) %>%
  fmt_percent(2, decimals=0) %>%
  cols_label(gbv_accept="Acceptibility of gender-based violence")
#             Bar="")

gbv_accept_cnty_gt
gtsave(gbv_accept_cnty_gt, "output/tables/Gender based violence acceptance county.docx")


# Girls' education ---- 


# Bride price ---- 

bp_labs <- c("Willing to accept a bride price for daughter in household",
             "Bride price an acceptable transaction",
             "Bride price an important tradition")

bp <- dat %>%
  dplyr::select(bp_accept_bin, bp_fin_bin, bp_trad_bin) %>%
  describe() %>%
  as.data.frame %>%
  mutate(Attitude = bp_labs,
         lower=mean-1.96*se,
         upper=mean+1.96*se) %>%
  dplyr::select(Attitude, Percent=mean, lower, upper, everything()) %>%
  arrange(desc(Percent))

bp

write_csv(bp, "output/tables/Bride price binaries.csv")

bp_gt <- bp %>%
  dplyr::select(Attitude, Percent) %>%
  gt() %>%
  fmt_percent(2, decimals=0) 


bp_gt

gtsave(bp_gt, "output/tables/Bride price binaries.docx")

  # by county

bp_accept_cnty <- d %>%
  group_by(County=county) %>%
  summarise(se=std.error(bp_accept_bin),
            bp_accept=mean(bp_accept_bin),
            n=n()) %>%
  arrange(desc(bp_accept)) %>%
  mutate(lower=bp_accept - 1.96*se, 
         upper=bp_accept + 1.96*se,
         lower=ifelse(lower<0, 0, lower),
         upper=ifelse(upper>1, 1, upper))

bp_accept_cnty

write_csv(bp_accept_cnty, "output/tables/Bride price acceptance county.csv")

bp_accept_cnty_gt <- bp_accept_cnty %>%
  dplyr::select(County, bp_accept) %>%
  #mutate(Bar=traffic_accept*100) %>%
  gt() %>%
  #gt_plt_bar_pct(column=Bar, fill=usaid_blue, background=light_grey, scaled=T) %>%
  #cols_width(3 ~ px(125)) %>%
  fmt_percent(2, decimals=0) 
#%>%
#  cols_label(
#             Bar="")

bp_accept_cnty_gt

gtsave(bp_accept_cnty_gt, "output/tables/Bride price acceptance county.docx")

# Trafficking In Persons ----


traffic_labs <- c("Acceptable", "Never Acceptable")

traffic_accept <- data.frame(frq(d$`829`)) %>%
  filter(!is.na(val))%>%
  mutate(traffic_labs=traffic_labs[c(1,2,5,6)],
         Percent=frq/sum(frq))

traffic_accept

write_csv(traffic_accept, "output/tables/q829 TIP.csv")



traffic_labs3 <- c("To Obtain A Wife", "To Get Cattle", "To Get Land")


TIP_agree <- dat %>%
  dplyr::select(TIP_agree1:TIP_agree3) %>%
  describe() %>%
  as.data.frame %>%
  mutate(TIP_agree = traffic_labs3,
         lower=mean-1.96*se,
         upper=mean+1.96*se) %>%
  dplyr::select(TIP_agree, Percent=mean, lower, upper, everything()) %>%
  arrange(desc(Percent))

TIP_agree

write_csv(TIP_agree, "output/tables/TIP Binaries.csv")

TIP_gt <- TIP_agree %>%
  dplyr::select(TIP_agree, Percent) %>%
  #mutate(Bar=Percent*100) %>%
  gt() %>%
  #gt_plt_bar_pct(column=Bar, fill=usaid_blue, background=light_grey, scaled=T) %>%
  #cols_width(3 ~ px(125)) %>%
  fmt_percent(2, decimals=0) 
  #cols_label(TIP_agree="Acceptance of Trafficking",
  #            Bar="")

TIP_gt

gtsave(TIP_gt, "output/tables/TIP binaries.docx")

ggplot(d, aes(x=traffic_accept)) + 
  geom_bar(width=.4, fill="dodgerblue2", alpha=.6) +  
  scale_x_continuous(breaks=0:1,
                     labels=c("Acceptable", "Unacceptable")) + 
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x="",
       y="",
       title="TIP Acceptability")


ggplot(d, aes(x=TIP_agree)) + 
  geom_bar(width=.4, fill="dodgerblue2", alpha=.6) +   scale_x_continuous(breaks=0:6) + 
  theme(axis.title.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x="Disagree (1) vs Agree (2)",
       y="",
       title="Agreement with Acceptable TIP Conditions ")



# TIP_gt2 <- traffic_accept %>%
#   dplyr::select(traffic_labs, Percent) %>%
#   mutate(Bar=Percent*100) %>%
#   gt() %>%
#   gt_plt_bar_pct(column=Bar, fill=usaid_blue, background=light_grey, scaled=T) %>%
#   cols_width(3 ~ px(125)) %>%
#   fmt_percent(2, decimals=0) %>%
#   cols_label(traffic_labs="Trafficking Acceptability",
#              Bar="")
# TIP_gt2

  ## TIP by County

TIP_accept_cnty <- d %>%
  group_by(county) %>%
  summarise(se=std.error(traffic_accept_bin),
            traffic_accept=mean(traffic_accept_bin),
            n=n()) %>%
  arrange(desc(traffic_accept)) %>%
  mutate(lower=traffic_accept - 1.96*se, 
         upper=traffic_accept + 1.96*se,
         lower=ifelse(lower<0, 0, lower),
         upper=ifelse(upper>1, 1, upper))

TIP_accept_cnty

write_csv(TIP_accept_cnty, "output/tables/TIP acceptance county.csv")

ggplot(TIP_accept_cnty, aes(x=traffic_accept, y=county)) +
  geom_point(color=usaid_blue, size=2.5) + 
  geom_errorbar(aes(xmin=lower, xmax=upper), width=0, color=usaid_blue, size=.8)

TIP_accept_cnty_gt <- TIP_accept_cnty %>%
  dplyr::select(county, traffic_accept) %>%
  #mutate(Bar=traffic_accept*100) %>%
  gt() %>%
  #gt_plt_bar_pct(column=Bar, fill=usaid_blue, background=light_grey, scaled=T) %>%
  #cols_width(3 ~ px(125)) %>%
  fmt_percent(2, decimals=0) 
#%>%
#  cols_label(
#             Bar="")

TIP_accept_cnty_gt

gtsave(TIP_gt, "output/tables/TIP binaries.docx")




