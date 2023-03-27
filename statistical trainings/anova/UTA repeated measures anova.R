#Import dataset (code below only works if file is in working directory)
weight <- read.csv('weight.csv')
summary(weight)


###setupdata to run ANOVA in car package
triallevels<- c(1,2,3)
trialfactor<-as.factor(triallevels)
trialframe<-data.frame(trialfactor)
trialbind<-cbind(weight$None,weight$Walk,weight$Bike)
trialmodel<-lm(trialbind~1)


###if you have not used these packages previously install:
install.packages("car")
install.packages("heplots")

###Run RM ANOVA test
library(car)
results<-Anova(trialmodel,idata=trialframe,idesign=~trialfactor)
summary(results)

###Calculate partial eta squared
###partial eta squared for traditional RM ANOVA
a <- summary(results)$univariate.tests[2,1]
b <- summary(results)$univariate.tests[2,3]
a/(a+b)
###partial eta squared for multivariate test
library(heplots)
etasq(results,anova=TRUE)

###runposthoc with bonferroni correction
weightnew=stack(weight)
colnames(weightnew)=c("weight","trial")

with(weightnew, pairwise.t.test(x=weight, g=trial, p.adjust.method="bonf", paired=T))