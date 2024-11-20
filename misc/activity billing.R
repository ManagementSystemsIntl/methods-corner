
# Dec 2023 ---- 

dat <- read_excel("TT__Floor_Check_Report_061223.xls",
                  sheet="export")

dir <- dat %>%
  filter(type=="Direct" | 
           type=="B & P Project") %>%
  select(1,5,6, 9, 10, 11) %>%
  mutate(wk=dmy(week))

str(dir)

ot <- dir %>%
  group_by(wk) %>%
  summarise(hours=sum(hours))


drwk <- dir %>%
  group_by(wk, task2) %>%
  summarise(hours=sum(hours))

drwk

ggplot(drwk, aes(x=wk, y=hours, fill=task2)) + 
  geom_hline(yintercept=c(0,40), color="grey60", size=1.4, alpha=.6, linetype="dotdash") +
   geom_area(na.rm=T, 
             #position="fill",
             ) +
  #scale_size_binned() +
#  scale_y_continuous(labels=percent_format()) +
  scale_fill_viridis_d() +
  theme(#legend.position="bottom",
        legend.title=element_blank(),
        axis.title.y=element_text(angle=0, vjust=.55)) +
  labs(x="",
       y="Billed\nhours",
       title="Activity utilization, 2023")
       #caption="Dan Killian")

ggsave("Dan Killian activity billing 2023 horiz.png",
       type="cairo",
       device="png",
       height=4, 
       width=9)


# May 2024 ---- 


dat <- read_excel("TT__Floor_Check_Report_030524.xls",
                  sheet="export")

str(dat)

frq(dat$type)

dir <- dat %>%
  filter(type=="Direct" | type=="B & P Project") %>%
  select(1,5,6, 9, 10, 11) %>%
  mutate(wk=dmy(week),
         yr=year(wk)) %>%
  filter(yr==2024) %>%
  arrange(task)

str(dir)
head(dir)

write_csv(dir, "Hours summary May 2024.csv")

dir <- read_csv("Hours summary May 2024.csv") %>%
  as.data.frame()

str(dir)
head(dir)
frq(dir$task2)

ot <- dir %>%
  group_by(wk) %>%
  summarise(hours=sum(hours))

ot

drwk <- dir %>%
  group_by(wk, task2) %>%
  summarise(hours=sum(hours)) %>%
  as.data.frame() %>%
  mutate(week=mdy(wk))

drwk
str(drwk)

?geom_area

ggplot(drwk, aes(x=week, y=hours)) + 
  geom_hline(yintercept=40, color="grey60", size=1.4, alpha=.6, linetype="dotdash") +
  geom_col(aes(fill=task2)) +
  scale_fill_viridis_d() +
  scale_x_date(date_breaks="1 month",
               date_labels="%b") +
  scale_y_continuous(limits=c(0,40)) +
  theme(legend.title=element_blank(),
        axis.title.y=element_text(angle=0, vjust=.55)) +
  labs(x="",
       y="Weekly\nbilled\nhours",
       title="Activity utilization, Jan-Apr 2024")

ggsave("Dan Killian activity billing Jan-Apr 2024 bar.png",
       type="cairo",
       device="png",
       height=4, 
       width=9)



ggplot(drwk, aes(x=week, y=hours, fill=task2)) + 
  geom_hline(yintercept=c(0,40), color="grey60", size=1.4, alpha=.6, linetype="dotdash") +
  geom_area(na.rm=T) +
  scale_fill_viridis_d() +
  theme(legend.title=element_blank(),
        axis.title.y=element_text(angle=0, vjust=.55)) +
  labs(x="",
       y="Billed\nhours",
       title="Activity utilization, 2024")

ggsave("Dan Killian activity billing Jan-Apr 2024 area.png",
       type="cairo",
       device="png",
       height=4, 
       width=9)


