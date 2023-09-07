# MSI analytics white paper

d <- data.frame(y=2012:2022,
                ie=c(NA, NA, NA, NA, 15,10,16,20,14,18,17),
                pe=c(NA, NA, NA, NA, 129, 135, 161, 154, 125, 68, 76),
                msi=c(3, 6, 8, 13, 13, 18, 29, 18, NA, NA, NA))

d

ie <- ggplot(d, aes(x=y)) +
  stat_smooth(aes(y=ie),
              method="lm",
              se=F,
              color="dodgerblue2") +
  geom_point(aes(y=ie),
             fill="dodgerblue2",
             color="white") + 
  geom_label(aes(y=ie,
                 label=ie),
             fill="dodgerblue2",
             color="white",
             label.padding=unit(.18, "lines")) +
  scale_x_continuous(limits=c(2016,2022),
                     breaks=2016:2022) +
  scale_y_continuous(limits=c(8,22),
                     breaks=seq(8,22,2)) +
  labs(title="<span style = 'color:dodgerblue2;'>Impact evaluations</span> on an increasing trend<br>
       <span style = 'color:firebrick2;'>Performance evaluations</span> on a decreasing trend",
       x="",
       y="") +
       #y="Impact\nevaluations") + 
  theme(axis.title.y=element_text(angle=0, vjust=.5),
        plot.title=element_textbox(hjust=0))

ie

pe <- ggplot(d, aes(x=y)) +
  stat_smooth(aes(y=pe),
              method="lm",
              se=F,
              color="firebrick2") +
  geom_label(aes(y=pe,
                 label=pe),
             fill="firebrick2",
             color="white",
             label.padding=unit(.18, "lines")) +
  scale_x_continuous(limits=c(2016, 2022),
                     breaks=2016:2022) +
  scale_y_continuous(limits=c(60,161),
                     breaks=seq(60,160,20)) +
  labs(#title="<span style = 'color:dodgerblue2;'>Impact evaluations</span> on an increasing trend<br>
       #<span style = 'color:firebrick2;'>Performance evaluations</span> on a decreasing trend",
         x="",
       y="") + 
  theme(axis.title.y=element_text(angle=0, vjust=.5))


ie
pe
ie / pe

ggsave("white paper/ie pe trends 2016-2022 fill.png",
       device="png",
       type="cairo",
       height=5,
       width=7)


msi <- ggplot(d, aes(x=y)) +
  stat_smooth(aes(y=msi),
              method="lm",
              se=F,
              color="darkgreen") +
  geom_label(aes(y=msi,
                 label=msi),
             color="darkgreen",
             label.padding=unit(.18, "lines")) +
  scale_x_continuous(limits=c(2012,2019),
                     breaks=2012:2019) +
  scale_y_continuous(limits=c(0,30),
                     breaks=seq(0,30,5)) +
  labs(title="<span style = 'color:dodgerblue2;'>Impact evaluations</span> on an increasing trend<br>
       <span style = 'color:firebrick2;'>Performance evaluations</span> on a decreasing trend",
       x="",
       y="") +
  #y="Impact\nevaluations") + 
  theme(axis.title.y=element_text(angle=0, vjust=.5),
        plot.title=element_textbox(hjust=0))

msi



msi_ie <- 

ggplot(d, aes(x=y)) +
  stat_smooth(aes(y=msi),
              geom="line",
              method="lm",
              se=F,
              color="darkgreen",
              alpha=.8,
              size=1) +
  stat_smooth(aes(y=ie),
              geom="line",
              method="lm",
              se=F,
              color="dodgerblue2",
              alpha=.6,
              size=1) +
  geom_label(aes(y=msi,
                 label=msi),
             fill="darkgreen",
             color="white",
             label.padding=unit(.15, "lines"),
             alpha=.9) +
  geom_label(aes(y=ie,
                 label=ie),
             fill="dodgerblue2",
             color="white",
             label.padding=unit(.15, "lines"),
             alpha=.9) +
  scale_x_continuous(breaks=2012:2022) +
  scale_y_continuous(limits=c(0,30),
                     breaks=seq(0,30,5)) +
  labs(title="Due to differences in count of 2017-2018 IEs, <br>
  <span style = 'color:darkgreen;'>MSI count of IEs (2012-2019)</span> estimate a 
       higher trend than <br>
       <span style='color:dodgerblue2;'>USAID count of IEs (2016-2022)</span>",
       y="Count \nof IEs",
       x="") +
  theme(axis.title.y=element_text(angle=0, vjust=.5),
        plot.title=element_textbox(hjust=0,
                                   size=14))

msi_ie

ggsave("white paper/msi and usaid count of IEs.png",
       device="png",
       type="cairo",
       height=4.4,
       width=7)



# ie quality review ---- 


rev <- read_dta("white paper/IEreview_dataset_use.dta") %>%
  mutate(low=case_when(st_score3==1 ~ 1,
                        TRUE ~ 0))

frq(rev$low)
frq(rev$high)

frq(rev$ft_score2)
frq(rev$ft_score3)

frq(rev$st_score3)


rev2 <- rev %>%
  filter(!is.na(st_score))

lowY <- rev %>%
  group_by(fy) %>%
  summarise(low=mean(low))

lowY

ggplot(lowY, aes(fy, low)) +
  stat_smooth(data=filter(lowY, fy<2017), 
              aes(y=low),
              method="lm", 
              se=F,
              color="darkorchid4") +
  stat_smooth(data=filter(lowY, fy>2014), 
              aes(y=low),
              method="lm", 
              se=F,
              color="darkgoldenrod2") +
  geom_label(data=filter(lowY, fy>2014), aes(label=paste(round(low*100, 0), "%", sep="")),
             fill="darkgoldenrod2",
             color="white") +
  geom_label(data=filter(lowY, fy<2016), 
             aes(label=paste(round(low*100, 0), "%", sep="")),
             fill="darkorchid4",
             color="white") +
  scale_x_continuous(breaks=2012:2019) +
  scale_y_continuous(limits=c(0,.6),
                     breaks=seq(0,.6,.1),
                     labels=percent_format(accuracy=1)) +
  labs(x="",
       y="",
#       y="Percent IEs\nreaching\nquality\nbenchmark",
       title="<span style = 'color:darkorchid4;'>IE quality increasing 2012-2015</span><br>
       <span style = 'color:darkgoldenrod2;'>IE quality flat or decreasing 2015-2019</span>") +
  theme(axis.title.y=element_text(angle=0, vjust=.5),
        plot.title=element_textbox(hjust=0,
                                   size=14))

ggsave("white paper/ie quality over time (MSI 2020).png",
       device="png",
       type="cairo",
       height=4,
       width=7)














