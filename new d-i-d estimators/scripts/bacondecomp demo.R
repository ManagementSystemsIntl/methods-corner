# bacondecomp demo

library(bacondecomp)

# demo ---- 

cas <- bacondecomp::castle %>%
  relocate(post, .after=year)

df_bacon <- bacon(l_homicide ~ post,
                  data = bacondecomp::castle,
                  id_var = "state",
                  time_var = "year")

#>                       type  weight  avg_est
#> 1 Earlier vs Later Treated 0.05976 -0.00554
#> 2 Later vs Earlier Treated 0.03190  0.07032
#> 3     Treated vs Untreated 0.90834  0.08796

coef_bacon <- sum(df_bacon$estimate * df_bacon$weight)
coef_bacon



print(paste("Weighted sum of decomposition =", round(coef_bacon, 4)))
#> [1] "Weighted sum of decomposition = 0.0818"

fit_tw <- lm(l_homicide ~ post + factor(state) + factor(year), 
             data = bacondecomp::castle)
print(paste("Two-way FE estimate =", round(fit_tw$coefficients[2], 4)))

ggplot(df_bacon) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point()



# MISTI ---- 

?bacon

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
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point()


