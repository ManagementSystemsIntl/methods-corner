# Libya GFA
# ACLED

d <- read_excel("data/ACLED/Libya GFA baseline/ACLED-Libya-2024-05-17.xlsx") %>%
  select(-region) %>%
  rename(region=admin1, district=admin2) %>%
  mutate(date=as.Date(event_date),
         year=year(date),
         quarter=quarter(date),
         qtr=as.yearqtr(date),
         month=as.yearmon(date),
         month_num=month(date),
         qtr_date=ordered(paste(year, quarter,sep="-")),
         mnth_date=as.Date(paste(year, month_num, "01", sep="-"))) %>%
  #left_join(regdist) %>%
  relocate(date, .after=event_date) %>%
  relocate(year, .after=date) %>%
  relocate(quarter, .after=year) %>%
  relocate(qtr, .after=quarter) %>%
  relocate(month, .after=qtr) %>%
  relocate(qtr_date, .after=qtr) %>%
  relocate(mnth_date, .after=qtr_date) %>%
  #relocate(region, .before=district) %>%
  filter(month!="Mar 2023") 

str(d)
names(d)

frq(d$qtr_date)  
frq(d$year)
frq(d$Event_Date)
frq(d$date)
frq(d$month)

frq(d$Admin1)
frq(d$Admin2)

dists <- d %>%
  distinct(Admin1)

dists
write_csv(dists, "data/mapping/Libya/ACLED districts.csv")

regdist <- read_csv("data/mapping/Libya/ACLED districts with region.csv")

regdist

adm2 <- d %>%
  distinct(Admin2)

adm2

comb <- data.frame(adm1=dists$Admin1,
                   adm2=adm2$Admin2)

getwd()

lib0 <- read_rds("data/mapping/Libya/gadm/gadm41_LBY_0_pk.rds")
lib1 <- read_rds("data/mapping/Libya/gadm/gadm41_LBY_1_pk.rds")

library(sf)

lib0_sf <- st_read("data/mapping/Libya/gadm/gadm41_LBY.gpkg",
                   layer="ADM_ADM_0")


lib1_sf <- st_read("data/mapping/Libya/gadm/gadm41_LBY.gpkg",
                   layer="ADM_ADM_1")

lib1_sf





# events fatalities

names(d)
frq(d$event_type)

qtrType <- d %>%
  group_by(year, qtr_date, event_type) %>%
  summarise(events=n(),
            fatalities=sum(fatalities)) %>%
  as.data.frame() %>%
  mutate(dths=ifelse(is.na(fatalities), 0, fatalities)) %>%
         #qtr2=fortify.zoo(qtr)) %>%
  filter(year>2020,
         event_type!="just") 

qtrType
str(qtrType)

ggplot(qtrType, aes(qtr_date)) + 
  stat_smooth(aes(y=events),
              color=usaid_blue,
              se=F) + 
  stat_smooth(aes(y=fatalities),
              color=usaid_red,
              se=F) 
+
  #scale_x_date(date_breaks="6 months",
  #             date_labels="%b-%y") +
  #annotate("text", x=as.yearmon("Jan 2022"), y=36.5, label="Events", color=usaid_blue) +
  #annotate("text", x=as.yearmon("Jan 2022"), y=33.5, label="Fatalities", color=usaid_red) + 
  annotate("text", x="2022-2"), y=36.5, label="Events", color=usaid_blue) +
  annotate("text", x="2022-2"), y=33.5, label="Fatalities", color=usaid_red) + 
  facet_wrap(~event_type) +
  faceted


monType <- d %>%
  group_by(year, mnth_date, event_type) %>%
  summarise(events=n(),
            fatalities=sum(fatalities)) %>%
  as.data.frame() %>%
  mutate(dths=ifelse(is.na(fatalities), 0, fatalities)) %>%
  #qtr2=fortify.zoo(qtr)) %>%
  filter(year>2020,
         mnth_date < as.Date("2024-4-01"),
         event_type!="just") 

head(monType)
str(monType)
frq(monType$mnth_date)


ggplot(monType, aes(mnth_date)) + 
  geom_line(aes(y=fatalities),
            color=usaid_red,
            size=.2,
            alpha=.5) +
  geom_line(aes(y=events),
            color=usaid_blue,
            size=.2,
            alpha=.5) +
  stat_smooth(aes(y=events),
              color=usaid_blue,
              se=F,
              size=.4) + 
  stat_smooth(aes(y=fatalities),
              color=usaid_red,
              se=F,
              size=.4) +
  scale_x_date(date_breaks="1 year",
               date_labels="%Y") +
  labs(x="",
       y="") +
  annotate("text", x=as.Date("2022-01-01"), y=38, label="Events", color=usaid_blue) +
  annotate("text", x=as.Date("2022-01-01"), y=30, label="Fatalities", color=usaid_red) +
  facet_wrap(~event_type) +
  faceted

ggsave("viz/ACLED/Libya/ACLED type month.png",
       device="png",
       type="cairo",
       height=4,
       width=8)

byDay <- d %>%
  group_by(year, date) %>%
  summarise(events=n(),
            fatalities=sum(fatalities)) %>%
  as.data.frame() %>%
  mutate(dths=ifelse(is.na(fatalities), 0, fatalities)) %>%
  filter(year>2020)

byDay

ggplot(byDay, aes(date)) + 
  geom_line(aes(y=events),
            color=usaid_blue,
            size=.2,
            alpha=.5) +
  stat_smooth(aes(y=events),
              color=usaid_blue,
              se=F) +
  geom_line(aes(y=dths),
            color=usaid_red,
            size=.2,
            alpha=.5) +
  stat_smooth(aes(y=dths),
              color=usaid_red,
              se=F) +
  scale_x_date(date_breaks="1 year",
               date_labels="%Y") +
  annotate("text", x=as.Date("2021-08-01"), y=26, label="Events", color=usaid_blue) +
  annotate("text", x=as.Date("2021-08-01"), y=24, label="Fatalities", color=usaid_red) +
  labs(x="",
       y="")

ggsave("viz/ACLED/Libya/LIbya events fatal 2021-2024.png",
       device="png",
       type="cairo",
       height=4,
       width=7)




# by  month region ---- 

regdist

monReg <- d %>%
  group_by(year, mnth_date, region) %>%
  summarise(events=n(),
            fatalities=sum(fatalities)) %>%
  as.data.frame() %>%
  mutate(dths=ifelse(is.na(fatalities), 0, fatalities)) %>%
  #qtr2=fortify.zoo(qtr)) %>%
  filter(year>2020,
         mnth_date < as.Date("2024-4-01")) 

head(monType)
str(monType)
frq(monType$mnth_date)


ggplot(monReg, aes(mnth_date)) + 
  geom_line(aes(y=fatalities),
            color=usaid_red,
            size=.2,
            alpha=.5) +
  geom_line(aes(y=events),
            color=usaid_blue,
            size=.2,
            alpha=.5) +
  stat_smooth(aes(y=events),
              color=usaid_blue,
              se=F,
              size=.4) + 
  stat_smooth(aes(y=fatalities),
              color=usaid_red,
              se=F,
              size=.4) +
  scale_x_date(date_breaks="2 years",
               date_labels="%Y") +
  labs(x="",
       y="") +
  annotate("text", x=as.Date("2022-01-01"), y=38, label="Events", color=usaid_blue) +
  annotate("text", x=as.Date("2022-01-01"), y=34, label="Fatalities", color=usaid_red) +
  facet_wrap(~region) +
  faceted

ggsave("viz/ACLED/Libya/ACLED region month.png",
       device="png",
       type="cairo",
       height=3,
       width=7)









            