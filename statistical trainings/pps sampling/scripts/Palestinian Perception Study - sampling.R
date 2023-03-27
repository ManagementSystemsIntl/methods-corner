# Palestinian Perception Study 
# sampling

source("scripts/00 Palestinian Perception Study - prep.r")

loc <- read_excel("data/sampling/Table of all Palestinian localities and population_updated 2017 Census data.xlsx",
                  sheet="tab29") %>%
  mutate(prob=pop/sum(pop),
         sampled=NA,
         n=NA)

sum(loc$prob)

frq(loc$sub)
frq(loc$governorate)

gov <- loc %>%
  group_by(region, subregion, governorate) %>%
  summarize(pop=sum(pop)) %>%
  mutate(prob=pop/sum(pop)) %>%
  arrange(governorate) %>%
  as.data.frame()

gov
str(gov)
str(mazGov)

gov_key <- data.frame(gov=1:16,
                      gov_lab=gov$governorate)

gov_key
write_csv(gov_key, "data/gov key.csv")

guv <- read_excel("data/sampling/Table of all Palestinian localities and population_updated 2017 Census data.xlsx",
                  sheet="gov") %>%
  arrange(governorate) %>%
  mutate(prob=pop/sum(pop))

guv
sum(guv$prob)

sum(gov$pop!=guv$pop)

reg <- read_excel("data/sampling/Table of all Palestinian localities and population_updated 2017 Census data.xlsx",
                  sheet="region") %>%
  mutate(prob=pop/sum(pop))

reg



set.seed(432)

?sample
?sample_n

samp1 <- sample(loc, 2)

samp1 <- sample_n(loc, 240, weight=prob) %>%
  mutate(sampled=1,
         n=10.5)
samp1



# Mazar cluster selections ---- 

nrth <- read_excel("data/sampling/Palestinian Perception Study - sampling points.xlsx",
                   sheet="North")

cent <- read_excel("data/sampling/Palestinian Perception Study - sampling points.xlsx",
                   sheet="Center")

sth <- read_excel("data/sampling/Palestinian Perception Study - sampling points.xlsx",
                   sheet="South")

gs <- read_excel("data/sampling/Palestinian Perception Study - sampling points.xlsx",
                   sheet="GS")

frq(nrth$governorate)


maz <- bind_rows(list(nrth,
                      cent,
                      sth,
                      gs)) %>%
  mutate(psu = paste(locality, counting_area, sep="-"),
         sampled=1, 
         n=10.5)

maz
243*10.5
2560/243

write_csv(maz, "data/sampling/mazar cluster selections.csv")
write_rds(maz, "data/sampling/mazar cluster selections.rds")


maz <- read_rds("data/sampling/mazar cluster selections.rds")

frq(maz$governorate)
frq(loc$governorate)

out <- loc %>%
  left_join(maz)

# cannot merge with census table right now



# mazar sample

gov
guv

mazGov <- maz %>%
  group_by(region, subregion, governorate) %>%
  summarize(n=round(sum(n),0)) %>%
  as.data.frame() %>%
  arrange(governorate)

%>%
  left_join(gov)

mazGov <- mazGov %>%
  bind_cols(gov[,4:5])

mazGov
str(mazGov)
str(gov)

sum(gov$governorate!=mazGov$governorate)



nLoc <- read_excel("data/sampling/Mazars sampling info.xlsx",
                   sheet="n per locality") %>%
  mutate(area=tolower(area))

frq(nLoc$area)

area_tar <- nLoc %>%
  group_by(area) %>%
  summarize(n=sum(n))

area_tar

write_csv(area_tar, "data/sampling/area targets.csv")


gov_tar <- nLoc %>%
  group_by(governorate) %>%
  summarize(n=sum(n))

gov_tar
write_csv(gov_tar, "data/sampling/gov targets.csv")

reg_tar <- nLoc %>%
  group_by(region) %>%
  summarize(n=sum(n))

reg_tar

write_csv(reg_tar, "data/sampling/region targets.csv")

reg_tar <- read_csv("data/sampling/region targets.csv")

reg_tar <- reg_tar %>%
  mutate(reg=c("Gaza\ntarget", "West Bank\ntarget"))

subreg_tar <- nLoc %>%
  group_by(subregion) %>%
  summarize(n=sum(n))

subreg_tar

# overall data collection targets ---- 

dayOv <- maz %>%
  group_by(date) %>%
  summarize(n=sum(n)) %>%
  mutate(cum_n=cumsum(n),
         day=12:23,
         type="target",
         color=usaid_red)

dayOv
str(dayOv)

ggplot(dayOv, aes(x=day, y=cum_n)) + 
  geom_line(color=usaid_red, size=1, alpha=.8, linetype="dotdash")
#  geom_point(color=usaid_red, size=3.5) +
#  geom_label(aes(label=round(cum_n, 0)), size=3.5, color=usaid_red)

# daily targets by region ---- 

dayReg <- maz %>%
  group_by(date, region) %>%
  summarize(n=sum(n)) %>%
  arrange(date) %>%
  group_by(region) %>%
  mutate(cum_n=cumsum(n),
         type="target",
         color=usaid_red,
         date=as.Date(date)) %>%
  ungroup()

str(dayReg)
dayReg

ggplot(dayReg, aes(date, cum_n, group=region)) + 
  geom_point(color=) + 
  geom_line() + 
  scale_x_date(date_breaks="1 day",
               date_labels="%d") +
  labs(x="May",
       y="",
       title="Data collection progress against targets")

?scale_x_date

dum <- read_excel("data/dummy/Copy of perception_of_usaid_test.xlsx")

names(dum)

b <- runif(100, 0,12)
b



?geom_segment

?scale_x_date




