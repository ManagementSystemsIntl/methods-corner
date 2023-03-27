
set.seed(785893)
cluster <- 1:20
clus_pop <- sample(1:25000, 20, T)
clus_cum <- cumsum(clus_pop)

dat <- data.frame(cluster, clus_pop, clus_cum)
dat

d <- 10 # number of sampled clusters
b <- sum(clus_pop) # total population 294,113 
c <- 300 # number sampled per cluster. To achieve equal probability of selection, number must be uniform across clusters
si <- b/d # sampling interval (clusters / total pop) 29411
rs <- sample(1:si, 1) # random start 19849. This means that cluster 3 is the random start
selected <- c(rs, rs+si, rs+2*si, rs+3*si, rs+4*si, rs+5*si, rs+6*si, rs+7*si, rs+8*si, rs+9*si)
selected

for(i in 1:length(selected)) {
  if selected[i] > dat$clus_cum[i] & < lag(dat$clus_cum[i],1) {
    dat$sampled[i] = 1
    else=0
  }
}

selected2 <- rep(NA, 10)

for(i in 1:length(selected)) {
  selected2[i] = selected[i]+20
}
selected2

for(i in seq_along(selected)) {
  for(j in 1:length(dat$sampled)) {
    if selected[i] > dat$clus_cum[j] & < lag(dat$clus_cum,1)[j] {
      dat$sampled[j] = 1
    }
    else {
      dat$sampled[j] = 0
    }
  }
}


dat$sampled <- c(0,0,1,1,0,1,0,1,1,0,1,0,0,1,0,1,0,1,0,1) # but I'd rather be able to compute this variable rather than do manually
sum(dat$sampled)

# Calculate for each of the sampled clusters the probability of being in each cluster
# prob1 = (a x d) / b
dat$prob1 <- ifelse(dat$sampled==1, (dat$clus_pop*d)/b, 0)
dat$prob1

# Calculate for each of the sampled clusters the probability of each individual being sampled in each cluster
# prob2 = c/a
dat$prob2 <- ifelse(dat$sampled==1, c/dat$clus_pop, 0)
dat$prob2

# Calculate the overall basic weight of an individual being sampled in the population
# base weight = 1 / (prob1*prob2)
dat$wt <- ifelse(dat$sampled==1, 1/(dat$prob1*dat$prob2), 0)
dat$wt

dat
