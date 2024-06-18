# PIMMELA 
# health facility sampling explore
# 4-8-24

d <- read_excel("sampling/demo - PIMMELLA/NSCA New Sampling Frame as of April 4.xlsx",
                sheet="Facility List") %>%
  set_names(nm=c("code","name","type","class","subclass","region","province","municipality","barangay"))

frq(d$code)

d %>%
  count(code)

# code variable is unique

frq(d$type)

rhu <- d %>%
  filter(type=="Rural Health Unit")

frq(rhu$region)



# set.seed(432)
# index <- NULL
# 
# for(i in 1:length(samp)) {
#   index <- c(index, sample(1:))
# }

# sample size ---- 

# hypergeometric mean
# n = ( NZ^2pq ) / ( (E^2(N-1) + Z^2pq) )

N <- nrow(d)
N2 <- 4672
z <- qnorm(.925) %>% round(2) # 1.44

?qnorm

p <- .5
q <- .5
e <- .1

1 - .15/2 # .925

num <- N*z^2*p*q
num

denom <- ( e^2 * (N-1) ) + (z^2*p*q)
denom

num/denom # 50.9

n_samp <- round(num / denom, 0) # 51
n_samp

# binomial check

numbin <- z^2*p*q
numbin

denombin <- e^2
denombin

n_bin <- round(numbin / denombin, 0)
n_bin


n_oversamp <- 80

rhu_regions <- rhu %>%
  count(region) %>%
  mutate(wt=n/sum(n),
         sample=round(n_samp*wt, 0),
         oversample=round(n_oversamp*wt, 0))

rhu_regions

regs <- rhu_regions$region %>%
  unlist()
regs

samp <- rhu_regions$sample
samp
sum(samp)

oversamp <- rhu_regions$oversample
oversamp

?strata

rhu2 <- rhu %>%
  arrange(region) %>%
#  as.data.frame() %>%
  rownames_to_column("ID_unit") %>%
  mutate(ID_unit=as.numeric(ID_unit)) %>%
  relocate(ID_unit, .after=region)

str(rhu2)
set.seed(425)

stratindex <- strata(rhu2,
                     stratanames=c("region"),
                     size=samp,
                     method="srswor") %>%
  mutate(selected=1)

head(stratindex)

overstratindex <- strata(rhu2,
                     stratanames=c("region"),
                     size=oversamp,
                     method="srswor") %>%
  mutate(overselected=1) %>%
  select(-Prob, -Stratum)

head(overstratindex)



rhu3 <- rhu2 %>%
  left_join(stratindex) %>%
  mutate(selected=ifelse(is.na(selected),0,selected)) %>%
  select(-Prob, -Stratum)

frq(rhu3$selected)
frq(rhu3$overselected)

write_csv(rhu3, "sampling/demo - PIMMELLA/rural health units stratified by region (4-8-24).csv")


rhu4 <- rhu3 %>%
  left_join(overstratindex) %>%
  mutate(overselected=ifelse(is.na(overselected), 0, overselected))

frq(rhu4$selected)
frq(rhu4$overselected)

write_csv(rhu4, "sampling/demo - PIMMELLA/rural health units stratified by region (4-11-24).csv")




# Example vectors
value_vector <- c("a", "b", "c")
another_vector <- c("a", "d", "e")

# Check if each element of value_vector is in another_vector
# Convert the result to numeric (1 for TRUE, 0 for FALSE)
contained_variable <- as.numeric(value_vector %in% another_vector)

# Print the result
print(contained_variable)




