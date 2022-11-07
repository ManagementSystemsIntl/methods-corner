# staggered demo

library(staggered)

# demo ---- 

df <- staggered::pj_officer_level_balanced %>% 
  rename(t = period, y = complaints, g = first_trained, i = uid)

head(df)
names(df)

staggered(df = df, estimand = "simple")

staggered(df = df, estimand = "cohort") 

staggered(df = df, estimand = "calendar")

#Calculate event-study coefficients for the first 24 months (month 0 is instantaneous effect)
eventPlotResults <- staggered(df = df, estimand = "eventstudy", eventTime = 0:23)

eventPlotResults %>% head()

eventPlotResults %>% 
  mutate(ymin_ptwise = estimate + 1.96*se,
         ymax_ptwise = estimate - 1.96*se)%>%
  ggplot(aes(x=eventTime, y =estimate)) +
  geom_pointrange(aes(ymin = ymin_ptwise, ymax = ymax_ptwise))+ 
  geom_hline(yintercept =0) +
  xlab("Event Time") + ylab("Estimate") +
  ggtitle("Effect of Procedural Justice Training on Officer Complaints")


#Calculate Callaway and Sant'Anna estimator for the simple weighted average
staggered_cs(df = df, estimand = "simple")

#Calculate Sun and Abraham estimator for the simple weighted average
staggered_sa(df = df, estimand = "simple")


names(starwars)
ggplot(starwars, aes(x=species))


