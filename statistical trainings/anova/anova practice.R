
# ANOVA practice

# prepare data ---- 

set.seed(4632)

grp_a <- rnorm(50, 25 ,12) %>%
    round(0)

#grp_a

grp_b <- rnorm(50, 45, 20) %>%
    round(0)

grp_c <- rnorm(50,55, 20) %>%
    round(0)

grp_d <- rnorm(50, 75, 12) %>%
    round(0)

d <- data.frame(group=rep(letters[1:4], each=50),
                y=c(grp_a, grp_b, grp_c, grp_d))

d
str(d)

g <- 4
N <- 200


describe(d)

describeBy(d$y, d$group)

grnd_mn <- mean(d$y)        
grnd_mn 

mns <- d %>%
    group_by(group) %>%
    summarise(grp_mn=mean(y),
              se=std.error(y))

mns

col <- viridis(4)
col



# distribution of overall data ----

da <- ggplot(d, aes(x=y)) + 
    geom_density(alpha=.3, fill="dodgerblue2", color="dodgerblue2", size=1) +
    scale_x_continuous(limits=c(-20,120),
                       breaks=seq(-20, 120, 20)) +
    theme(axis.text.y=element_blank()) +
    labs(x="",
         y="",
         title="Distribution of data")

# distribution of group data ---- 

grp <- ggplot(d, aes(x=y, group=group, fill=group, color=group)) + 
    geom_density(alpha=.3) +
    scale_x_continuous(limits=c(-20,120),
                       breaks=seq(-20, 120, 20)) +
    scale_color_viridis_d() +
    scale_fill_viridis_d() + 
#    annotate("text", x=mns$grp_mn, y=.0158, label=letters[1:4]) +
    annotate("text", x=c(18, 41, 53, 78), y=.021, label=letters[1:4]) +
    theme(axis.text.y=element_blank(),
          legend.position="none") +
    labs(x="",
         y="",
         title="Distribution of group data")

#library(patchwork)
da + grp

# decompose variance across groups

d <- d %>%
    left_join(mns[,1:2]) %>%
    group_by(group) %>%
    mutate(w_dev=y-mean(y),
           sq_w_dev=w_dev^2) %>%
    ungroup() %>%
    mutate(bet_dev = grp_mn-grnd_mn,
           sq_bet_dev = bet_dev^2,
           grnd_mn = grnd_mn,
           grnd_dev=grnd_mn-y,
           sq_grnd_dev=grnd_dev^2)

d

# ssw sum of squares within groups (distance between observation and its group mean)

ssw <- sum(d$sq_w_dev)
ssw # 44528

ssw_check <- d %>%
    group_by(group) %>%
    summarise(ss = var(y) * 49) %>%
    summarise(sum(ss))

ssw_check # 44528

# ssb sum of squares between (group means from grand mean)

ssb <- sum(d$sq_bet_dev)
ssb # 208513

ssb_check <- mns %>%
    mutate(bet_dev=grp_mn-grnd_mn,
           sq_bet_dev=bet_dev^2) %>%
    summarize(ssb=sum(sq_bet_dev*100))

ssb_check # 208513



ssb <- sum(mns$sq_bet_dev*100)        
ssb # 208513

# sst total sum of squares (observations from grand mean)

mns <- d %>%
    group_by(group) %>%
    summarize(grp_mn=mean(y),
              se=std.error(y)) %>%
    mutate(grnd_dev=grp_mn-grnd_mn,
           sq_grnd_dev=grnd_dev^2)

mns

sst <- sum(d$sq_grnd_dev)
sst 

# create anova table

aov_tab <- data.frame(errors=c("Between","Within","Total"),
                      squares=c("SSB","SSW","SST"),
                      ss_act=c(ssb, ssw, sst),
                      df=c("g-1","N-g","N-1"),
                      df_act = c(4-1, 400-4, 400-1),
                      mn_sq = c("SSB/DFB", "SSW/DFW", ""),
                      F=c("MSB/MSW","","")) %>%
    mutate(mn_sq_act=ss_act/df_act,
           F_act=c(mn_sq_act[1]/mn_sq_act[2], NA, NA))

aov_tab

fstat1 <- round(( ssb/(g-1) ) / ( ssw/(N-g) ), 2) 

fstat1

ssb / (g-1)
ssw / (N-g)

fstat2 <- round(( aov_tab$ss_act[1] / aov_tab$df_act[1] ) / ( aov_tab$ss_act[2] / aov_tab$df_act[2] ), 2)

fstat2 

crit <- qf(p=.05,
           df1=g-1,
           df2=N-g,
           lower.tail=F)

crit

p <- pf(207, g-1, N-g, lower.tail=F)
p

av <- aov(y~group, d)
av
summary(av)
aov_tab
TukeyHSD(av)
ScheffeTest(av)
pairwise.t.test(d$y, d$group, p.adj="holm")

rg <- lm(y~group, d)
summary(rg)
anova(rg)


anov <- lm(y~group, d) %>%
    anova()

anov


        
        
        
# distribution of sample means ---- 

ggplot() +
  stat_function(fun = dnorm, 
                args=list(mean=mns$grp_mn[1],
                          sd=mns$se[1]),
                geom = "polygon",
                color = col[1], 
                fill = col[1], 
                alpha = 0.4) +
  stat_function(fun = dnorm, 
                args=list(mean=mns$grp_mn[2],
                          sd=mns$se[2]),
                geom = "polygon",
                color = col[2], 
                fill = col[2], 
                alpha = 0.4) +
  stat_function(fun = dnorm, 
                args=list(mean=mns$grp_mn[3],
                          sd=mns$se[3]),
                geom = "polygon",
                color = col[3], 
                fill = col[3], 
                alpha = 0.4) +
  stat_function(fun = dnorm, 
                args=list(mean=mns$grp_mn[4],
                          sd=mns$se[4]),
                geom = "polygon",
                color = col[4], 
                fill = col[4], 
                alpha = 0.4) +
  scale_x_continuous(limits=c(0,100),
                     breaks=seq(0,100,20)) +
  theme(axis.text.y=element_blank()) +
  labs(x="",
       y="",
       title="Distribution of sample means")


        
        
        
        
        
        
        
    