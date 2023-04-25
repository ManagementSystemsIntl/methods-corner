library(rdd)

?RDestimate

x<-runif(1000,-1,1)

cov<-rnorm(1000)

y<-3+2*x+3*cov+10*(x>=0)+rnorm(1000)

a <- RDestimate(y~x)

# Efficiency gains can be made by including covariates

b <- RDestimate(y~x|cov)

summary(b)
plot(b)

setwd("C:/Users/dan.killian/Dropbox/work/MSI/Jordan/CITIES/Inception Report")

mlda <- read_dta("rdd dataset.dta")

mlda$over21 = mlda$agecell>=21
attach(mlda)

# fit two models, the second has a quadratic age term.

fit=lm(all~agecell+over21)
fit2=lm(all~agecell+I(agecell^2)+over21)

# plot the two models fit and fit2, using predicted values.

predfit = predict(fit, mlda)
predfit2=predict(fit2,mlda)

# plotting fit

plot(predfit~agecell,type="l",ylim=range(85,110),
     col="red",lwd=2, xlab="Age",
     ylab= "Death rate from all causes (per 100,000)")

# adding fit2
points(predfit2~agecell,type="l",col="blue")

# adding the data points
points(all~agecell)

ggplot(mlda, aes(x=agecell, y=all, color=over21)) + geom_point(color="black", size=1.2) + stat_smooth(method="lm", se=F) + 
  scale_color_manual(values=c("black","black")) + 
  theme(legend.position="none") + xlab("Age") + ylab("All deaths per 100,000") + 
  ylim(85,105) + geom_vline(xintercept=21, color="darkblue", linetype="dashed")+ 
  ggtitle("Effect of legal alchohol consumption on youth") + 
  annotate("text", x=22.1, y=88, label="Local average treatment effect (LATE) \n~ 9 deaths per 100,000")


?geom_vline

Desc(mlda$all)

a <- RDestimate(all ~ agecell, data=mlda, cutpoint=21)
a
a[1]
a[2]
a[3]
a[4]
a[5]
a[6]
a[7]
a[8]
a[9]
a[10]
a[11]
a[12]

plot(a)

alc <- RDestimate(alcohol ~ agecell, data=mlda, cutpoint=21)
summary(alc)
plot(alc)


ggplot(mlda, aes(x=agecell, y=alcohol, color=over21)) + geom_point(color="black", size=1.2) + stat_smooth(method="lm", se=F) + 
  scale_color_manual(values=c("black","black")) + 
  theme(legend.position="none") + xlab("Age") + ylab("Alcohol-related deaths per 100,000") + 
  geom_vline(xintercept=21, color="darkblue", linetype="dashed") + ylim(0,3) +
ggtitle("Effect of legal alchohol consumption on youth") + 
  annotate("text", x=19.9, y=2.6, label="Local average treatment effect (LATE) \n~ 0.96 deaths per 100,000 (p=.012)")

