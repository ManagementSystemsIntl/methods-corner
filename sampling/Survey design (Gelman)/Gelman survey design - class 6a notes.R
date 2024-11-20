# Gelman survey design
# Class 6a notes
# Cluster sampling with unequal sample sizes

set.seed(4532)

d <- data.frame(cat = rep(letters[1:5], 200),
                y=rnorm(1000, 50, 10)) %>%
  mutate(y2 = case_when(cat=="a" ~ y + rnorm(1, 7, 2),
                        cat=="b" ~ y - rnorm(1, 7, 2),
                        cat=="c" ~ y + rnorm(1,12,3),
                        cat=="d" ~ y - rnorm(1, 12, 3),
                        cat=="e" ~ y + rnorm(1,15,4)),
         srs=sample(0:1, size=1000, replace=T, prob=c(.9,.1)))

clust_srs <- sample(letters[1:5], 2, F)
clust_srs

d <- d %>%
  mutate(clust_selected=case_when(cat=="a" | cat == "d" ~ 1,
                                  TRUE ~ 0)) %>%
  arrange(cat)

a_sel <- sample(0:1, size=200, T, c(.9,.1))
a_sel

d_sel <- sample(0:1, 200, T, c(.9,.1))
d_sel

d <- d %>%
  mutate(clust_srs=c(a_sel, rep(0,200), rep(0,200), d_sel, rep(0,200)))

describe(d)
head(d)
frq(d$cat)
frq(d$srs)
frq(d$clust_selected)
frq(d$clust_srs)


ggplot(d, aes(y)) + 
  geom_density(size=.4,
               color="blue",
               fill="dodgerblue2",
               alpha=.4) + 
  scale_x_continuous(limits=c(0,100),
                     breaks=seq(0,100,10))


describeBy(d$y2, d$cat)
