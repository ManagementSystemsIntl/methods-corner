# Libya GFA
# Fragile states index

fsi23 <- read_excel("data/Fragile states index/FSI-2023-DOWNLOAD.xlsx")
fsi22 <- read_excel("data/Fragile states index/fsi-2022-download.xlsx")
fsi21 <- read_excel("data/Fragile states index/fsi-2021.xlsx")
fsi20 <- read_excel("data/Fragile states index/fsi-2020.xlsx")

names(fsi23)
names(fsi22)

# national reconciliation ---- 

f23_pol <- fsi23 %>%
  select(Country, Year, Rank, Total, 
         "P1: State Legitimacy",
         "P2: Public Services",
         "P3: Human Rights",
         "C2: Factionalized Elites",
         "C3: Group Grievance")

f23_pol
str(f23_pol)

f22_pol <- fsi22 %>%
  select(Country, Year, Rank, Total, 
         "P1: State Legitimacy",
         "P2: Public Services",
         "P3: Human Rights",
         "C2: Factionalized Elites",
         "C3: Group Grievance") %>%
  mutate(Year=2022)

f22_pol
str(f22_pol)

f21_pol <- fsi21 %>%
  select(Country, Year, Rank, Total, 
         "P1: State Legitimacy",
         "P2: Public Services",
         "P3: Human Rights",
         "C2: Factionalized Elites",
         "C3: Group Grievance")

f21_pol
str(f21_pol)

f20_pol <- fsi20 %>%
  select(Country, Year, Rank, Total, 
         "P1: State Legitimacy",
         "P2: Public Services",
         "P3: Human Rights",
         "C2: Factionalized Elites",
         "C3: Group Grievance") %>%
  mutate(Year=2020)

f20_pol
str(f20_pol)

pol <- bind_rows(f23_pol,
                 f22_pol,
                 f21_pol,
                 f20_pol) %>%
  filter(Country=="Libya")

pol

pol2 <- pol %>%
  select(1,2,5:9)

pol2

pol2L <- pol2 %>%
  pivot_longer(cols=3:7,
               names_to="measure",
               values_to="score")

pol2L

ggplot(pol2L, aes(Year, score, color=measure)) +
  geom_point(size=1) +
  #geom_line() + 
  geom_labelpath(aes(label=measure),
                hjust="ymax",
                size=3) +
  scale_color_viridis_d() +
  scale_y_continuous(limits=c(6,10),
                     breaks=6:10,
                     sec.axis=dup_axis()) +
  labs(x="",
       y="",
       caption="Higher scores indicate more fragility") +
  theme(legend.title=element_blank(),
        legend.position="none")

ggsave("viz/Fragile states index/Libya/reconciliation path.png",
       device="png",
       type="cairo",
       height=4,
       width=6)


#  facet_wrap(~measure) +
#  faceted +
#  theme(legend.position="none") +
#  scale_x_continuous(labels=c("", "2021", "", "2023"))



ggplot(pol2L, aes(Year, score, color=measure)) +
  geom_point(size=.8) +
  geom_line() + 
  scale_color_viridis_d() +
  facet_wrap(~measure) +
  faceted +
  theme(legend.position="none") +
  scale_x_continuous(labels=c("", "2021", "", "2023"))

# marginalization ---- 


f23_marg <- fsi23 %>%
  select(Country, Year, Rank, Total, 
         "E2: Economic Inequality",
         "S1: Demographic Pressures",
         "S2: Refugees and IDPs")

f23_marg
str(f23_marg)

f22_marg <- fsi22 %>%
  select(Country, Year, Rank, Total, 
         "E2: Economic Inequality",
         "S1: Demographic Pressures",
         "S2: Refugees and IDPs") %>%
  mutate(Year=2022)

f22_marg
str(f22_marg)

f21_marg <- fsi21 %>%
  select(Country, Year, Rank, Total, 
         "E2: Economic Inequality",
         "S1: Demographic Pressures",
         "S2: Refugees and IDPs")

f21_marg


f20_marg <- fsi20 %>%
  select(Country, Year, Rank, Total, 
         "E2: Economic Inequality",
         "S1: Demographic Pressures",
         "S2: Refugees and IDPs") %>%
  mutate(Year=2020)

f20_marg


marg <- bind_rows(f23_marg,
                 f22_marg,
                 f21_marg,
                 f20_marg) %>%
  filter(Country=="Libya")

marg

marg2 <- marg %>%
  select(1,2,5:7)

marg2

marg2L <- marg2 %>%
  pivot_longer(cols=3:5,
               names_to="measure",
               values_to="score")

marg2L

ggplot(marg2L, aes(Year, score, color=measure)) +
  #geom_point(size=1,
  #           show.legend=F) +
  #geom_label(aes(label=score),
  #           show.legend = F) +
#  geom_line(show.legend=F) + 
  geom_textpath(aes(label=measure),
                show.legend=F,
                hjust="auto",
                size=3) +
  scale_color_viridis_d() +
  scale_y_continuous(limits=c(4,10),
                     breaks=4:10,
                     sec.axis = dup_axis()) +
  labs(x="",
       y="",
       caption="Higher scores indicate more fragility") +
  theme(legend.title=element_blank()) #,
        #plot.background=element_rect(fill="aliceblue"),
        #panel.background=element_rect(fill="aliceblue")) 

ggsave("viz/Fragile states index/Libya/Libya fragile states index - marginalization path.png",
       device="png",
       type="cairo",
       height=4,
       width=4)

scale_color_viridis_d()[1:4]
