# Bayesian Data Analysis
# NFL point spreads

df <- read_table("data/NFL point spreads.txt") %>%
    mutate(diff=favorite-underdog,
           sprd_diff = diff-spread,
           fav_win = ifelse(diff>0, 1,0),
           upset=ifelse(diff<0, 1,0),
           spread_trunc = ifelse(spread>14,14, spread))
head(df)

lapply(df[,c(1,5,6,10,11)], frq)

spr <- df %>%
    group_by(spread_trunc) %>%
    summarise(win=mean(fav_win),
              n=n(),
              se=std.error(fav_win)) %>%
    mutate(lower=win-1.96*se,
           upper=win+1.96*se,
           upper=ifelse(upper>1, 1, upper))

spr

ggplot(spr, aes(spread_trunc, win)) + 
    geom_errorbar(aes(ymin=lower, ymax=upper),
                  width=0,
                  color="dodgerblue2",
                  size=.8,
                  alpha=.7) + 
    geom_point(color="dodgerblue2",
               size=1.8,
               ) +
    stat_smooth(color="dodgerblue2") +
    scale_x_continuous(breaks=seq(0,20,1)) +
    scale_y_continuous(limits=c(0,1),
                       breaks=seq(0,1,.1),
                       labels=percent_format(accuracy=1)) +
    labs(x="Point spread", #\n(expected points (favorite) - expected points (underdog)",
         y="Win probability\n(favorite)",
         caption="37 spreads greater\nthan 14 coded to 14") +
    theme(axis.title.y=element_text(angle=0, vjust=.5))

lm(fav_win ~ spread + home + as.factor(week),
   data=df) %>%
    summary()






