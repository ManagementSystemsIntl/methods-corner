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
  summarize(estimate=mean(estimate),
            weight=sum(weight))

castle_bacon_sum


#>                       type  weight  avg_est
#> 1 Earlier vs Later Treated 0.05976 -0.00554
#> 2 Later vs Earlier Treated 0.03190  0.07032
#> 3     Treated vs Untreated 0.90834  0.08796

coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)

coef_bacon

plot(df_bacon)

print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))
#> [1] "Weighted sum of decomposition = 0.0818"

fit_tw <- lm(l_homicide ~ post + factor(state) + factor(year), 
             data = bacondecomp::castle)
print(paste("Two-way FE estimate =", round(fit_tw$coefficients[2], 4)))

ggplot(df_bacon) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point()

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

mistibacon2 <- bacon(stab_std ~ treat_event + nsp,
                    data=mistifull,
                    id_var="village",
                    time_var="wave")

summary(mistibacon)
mistibacon

ggplot(mistibacon) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type", color="Type") +
  geom_point()



