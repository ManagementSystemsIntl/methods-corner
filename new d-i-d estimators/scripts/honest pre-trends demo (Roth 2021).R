# honest pre-trends demo

library(pretrends)

#Load the coefficients, covariance matrix, and time periods
beta <- pretrends::HeAndWangResults$beta
sigma <- pretrends::HeAndWangResults$sigma
tVec <- pretrends::HeAndWangResults$tVec
referencePeriod <- -1 #This is the omitted period in the regression
d <- data.frame(t = tVec, beta = beta)
d

#Compute slope that gives us 50% power
slope50 <-
  slope_for_power(sigma = sigma,
                  targetPower = 0.5,
                  tVec = tVec,
                  referencePeriod = referencePeriod)
slope50

pretrendsResults <- 
  pretrends(betahat = beta, 
            sigma = sigma, 
            tVec = tVec, 
            referencePeriod = referencePeriod,
            deltatrue = slope50 * (tVec - referencePeriod))

pretrendsResults$event_plot

pretrendsResults$df_power

pretrendsResults$df_eventplot

pretrendsResults$event_plot_pretest

pretrends(betahat = beta, 
          sigma = sigma, 
          tVec = tVec, 
          referencePeriod = referencePeriod,
          deltatrue = -0.05 * (tVec - referencePeriod)^2)$event_plot_pretest








