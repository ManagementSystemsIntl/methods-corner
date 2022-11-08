# bacondecomp demo

library(bacondecomp)

# castle ---- 

cas <- bacondecomp::castle %>%
  relocate(post, .after=year)

castle_bacon <- bacon(l_homicide ~ post,
                  data = bacondecomp::castle,
                  id_var = "state",
                  time_var = "year")

castle_bacon
summary(castle_bacon)

castle_bacon_sum <- castle_bacon %>%
  group_by(type) %>%
  summarize(estimate=weighted.mean(estimate, weight),
            weight=sum(weight))

castle_bacon_sum


#>                       type  weight  avg_est
#> 1 Earlier vs Later Treated 0.05976 -0.00554
#> 2 Later vs Earlier Treated 0.03190  0.07032
#> 3     Treated vs Untreated 0.90834  0.08796

coef_bacon <- sum(castle_bacon$estimate * castle_bacon$weight)

coef_bacon

plot(df_bacon)

print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))
#> [1] "Weighted sum of decomposition = 0.0818"

fit_tw <- lm(l_homicide ~ post + factor(state) + factor(year), 
             data = bacondecomp::castle)
print(paste("Two-way FE estimate =", round(fit_tw$coefficients[2], 4)))

ggplot(castle_bacon) +
  aes(x = weight, y = estimate, shape = factor(type), color=factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type", color="Type") +
  geom_point() + 
  scale_color_viridis_d()

# divorce ---- 

?panelview
?bacondecomp::divorce

div <- bacondecomp::divorce 
%>%
  relocate(post, .after=year)

names(div)
str(div)

div_bacon <- bacon(l_homicide ~ post,
                  data = bacondecomp::castle,
                  id_var = "state",
                  time_var = "year")


# MISTI ---- 

?bacon
?panelview

?distinct
mistifull %>%
  distinct(village) %>%
  tally()

mistifull %>%
  group_by(wave, treat_event) %>%
  summarize(n=n())

# wave 2 14
# wave 3 19 / 33
# wave 4 20 / 53
# wave 5 32 / 85

test <- mistifull %>%
  filter(wave==1 & treat_event==1)


mistifull <- mistifull %>%
  filter(first.treat!=1)

panelview(stab_std ~ treat_event,
          data=mistifull,
          index=c("village","wave"))

mistibacon <- bacon(stab_std ~ treat_event,
                    data=mistifull,
                    id_var="village",
                    time_var="wave")

head(mistibacon)

mistibacon_coef <- sum(mistibacon$estimate * mistibacon$weight)
mistibacon_coef  
  
coef_bacon <- sum(castle_bacon$estimate * castle_bacon$weight)

coef_bacon

mistibacon2 <- bacon(stab_std ~ treat_event + nsp,
                    data=mistifull,
                    id_var="village",
                    time_var="wave")

summary(mistibacon)
mistibacon

ggplot(mistibacon) +
  aes(x = weight, y = estimate, shape = factor(type), color=as.factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type", color="Type") +
  geom_point() + 
  scale_x_continuous(limits=c(0,.3),
                     breaks=seq(0,.3,.05)) +
  scale_color_viridis_d() + 
  faceted + 
  theme(legend.position="bottom",
        legend.title=element_blank())



# misti twfe ---- 

tw <- lm(stab_std ~ treat_event + + nsp + as.factor(village) + as.factor(wave),
         data=mistifull)

summary(tw)
coef(tw)[1:3]
mistibacon_coef


