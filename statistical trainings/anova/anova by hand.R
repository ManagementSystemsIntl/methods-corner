# doing ANOVA by hand
set.seed(5)
score <- round(rnorm(90,70,12),0)
str(score)
describe(score)
hist(score)

dat <- data.frame(score,class,class2)
head(dat)
str(dat)

# summary table by group

datClass <- dat %>%
     group_by(.,class2) %>%
     summarize(n=n(), mean=mean(score), sd=sd(score))

# get total sum of squares
dat$dev <- dat$score-mean(dat$score)
dat$sq <- (score-mean(dat$score))^2

sst <- sum(dat$sq)
sst

# get between group sum of squares

datClass
mean(dat$score)

datClass$sq <- (datClass$n*(datClass$mean-70.22)^2)

ssClass <- sum(datClass$sq)
ssClass

ssR <- sst-ssClass
ssR

sumSq <- c(ssClass, ssR, sst)
sumSq

df <- c((3-1), (90-3), sum(datClass$n)) 
df

meanSq <- sumSq/df
meanSq

f <- meanSq[1]/meanSq[2]
f

p <- pf(.95, 87,2)
p

aovTab <- data.frame(df, sumSq, meanSq, f,p)
aovTab

fit <- lm(score~class2)
anova(fit)

