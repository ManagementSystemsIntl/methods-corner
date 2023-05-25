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

hhs_labs <- c("")

lapply(hhs, frq)

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

?last

str(shk)
frq(d$`436`)

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




names(dat)


# shocks ---- 

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
frq(d$`436`)

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
str(shk)
frq(d$`436`)

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

gtsave(resil_gt, "output/tables/Resilience binaries.html")




# res3 <- fa(res[,2:9],
#            cor="poly",
#            nfactors=3)
# 
# res3
# # capital
# # transformative
# # adaptive
# 
# 
# res3_scores <- data.frame(res3$scores) %>%
#   set_names(nm=c("Capital_fac3","Transform_fac3","Absorp_fac3"))
# 
# head(res3_scores)
# describe(res3_scores)
# 
# ggplot(res3_scores, aes("Absorp_fac")) + 
# #  geom_density(aes("Absorp_fac")) +
# #  stat_density() +
# #  geom_boxplot()


# save prepared file ---- 

#write_rds(dat, "data/prepared/South Sudan resilience panel survey - prepared.rds")
#write_csv(dat, "data/prepared/South Sudan resilience panel survey - prepared.csv")



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
str(shk)
frq(d$`436`)

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











