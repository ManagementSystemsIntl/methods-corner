# Governance and regime change

library(vdem)
library(vdemdata)

vdem  

vdem()

vd <- vdem()

?VDem_plus

vd <- VDem_plus

names(vd)[1:80]

reg <- vd %>%
  select(country=vdem_country_name,
         year,
  )

head(reg)

vd_cb <- vdem_codebook

tun <- vd %>%
  filter(vdem_country_name=="Tunisia")

names(tun)[1:40]
vdNames <- data.frame(names(tun))

?contains
?str_detect

a <- which(str_detect(names(tun), "v2petersch"))
a

names(tun)[2221:2301]


coup <- read_csv("Governance and regime change/data/coups/Coup_Data_v2.0.0.csv")
str(coup)

coup_tun <- coup %>%
  filter(country=="Tunisia")

tun_eq <- tun %>%
  select(1:24,
         2221:2301) %>%
  filter(year>1999)

eqNames <- data.frame(names(tun_eq))

library(psych)
describe(tun_eq$v2pepwrses)

ggplot(tun_eq,aes(x=year, y=v2pepwrses)) + 
  geom_point() + 
  geom_line() 

tuneq2 <- tun_eq %>%
#  mutate(v2peprisch=v2peprisch/100,
#         v2pesecsch=v2pesecsch/100,
#         v2petersch=v2petersch/100) %>%
  select(year,
         v2pepwrses,
         v2pepwrsoc,
         v2pepwrgen,
         v2pepwrort,
         v2peedueq,
         v2pehealth) %>%
#         v2peprisch,
#         v2pesecsch,
#         v2petersch) %>%
  pivot_longer(cols=2:7,
               names_to="indicator",
               values_to="value") %>%
  arrange(indicator)

head(tuneq2)

tab_xtab(tun_eq$year, tun_eq$v2peprisch)

pe_nms <- c("v2pepwrses","v2pepwrsoc","v2pepwrgen","v2pepwrort","v2peedueq","v2pehealth")
  
names(tun_eq)[2:7]

pe_nms

pe_labs <- c("Equity by ses",
             "Equity by social group",
             "Equity by sex",
             "Equity by sexual orientation",
             "Equity by education",
             "Equity by health")

pe_labs2 <- c("SES",
             "Social group",
             "Sex",
             "Sexual\norientation",
             "Education",
             "Health")

pe_key <- data.frame(indicator = pe_nms,
                     pe_lab = pe_labs,
                     pe_lab2 = pe_labs2)

pe_key

tuneq2 <- tuneq2 %>%
  left_join(pe_key)

va_name <- va1 %>%
  group_by(Country_Name) %>%
  summarise(value1 = last(Estimate),
            value2 = nth(Estimate,11)) %>%
  mutate(value3 = c(-1.25, -1.55, .281),
         color=viridis(3))
va_name

end <- tuneq2 %>%
  group_by(pe_lab2) %>%
  summarize(last=last(value)) %>%
  mutate(color=viridis(6),
         last2=c(.58, .28, 1.5, 1.25, -.971, 1.84))

end

library(geomtextpath)

ggplot(tuneq2, aes(x=year, y=value, color=pe_lab2)) +
  geom_vline(xintercept=c(2010.8, 2019.5),
             color="grey60", alpha=.4, size=c(3,1.2)) +
#  geom_point() + 
  geom_line(size=1, alpha=.6) + 
#  geom_labelpath(aes(label=indicator)) +
  geom_textline(aes(label=pe_lab2), hjust="ymin", vjust=-.2) +
  theme(legend.position="none") +
  scale_y_continuous(limits=c(-2,4),
                     sec.axis=sec_axis(~.,
                                       breaks=end$last2,
                                       labels=end$pe_lab2)) +
  scale_color_viridis_d() +
  labs(x="",
       y="",
       title="V-Dem indicators and regime change in Tunisia",
       caption="V-Dem political equality indicators\n'To what extent is political power equitably distributed by_______?'")

#,
#        axis.ticks.right=element_text(color=end$color)) +

ggsave("Governance and regime change/viz/V-Dem political equality indicators, Tunisia.png",
       device="png",
       type="cairo",
       height=4,
       width=7)


  