# PIMMELA 
# sampling explore
# 3-17-24

d <- read_excel("sampling/demo - PIMMELLA/Phils. Health Facilities.xls",
                sheet="Worksheet") %>%
  rename(class=6) %>%
  filter(class=="Government")

head(d)
frq(d$class)

# hypergeometric mean
# n = ( NZ^2pq ) / ( (E^2(N-1) + Z^2pq) )

N <- nrow(d)
z <- qnorm(.925) %>% round(2) # 1.44
p <- .5
q <- .5
e <- .1

1 - .15/2 # .925

num <- N*z^2*p*q
num

denom <- e^2*N-1 + z^2*p*q
denom

n <- num / denom # 212
n

Nsim <- 1000:1e4

numsim <- Nsim*z^2
numsim

denomsim <- e^2*Nsim-1 + z^2*p*q
denomsim

nsim <- num / denom # 212
n

# binomial

numbin <- N*z^2*p*q
numbin

denombin <- 

d %>%
  group_by(`Province Name`) %>% #, `Barangay Name`) %>%
  tally() %>%
  arrange(desc(n))

set.seed(432)
smp <- d %>%
  sample_n(size=250) # with replacement

sample(1:1000, 10)

write_csv(smp, "sampling/demo - PIMMELLA/srs 250 (3-17-24).csv")










