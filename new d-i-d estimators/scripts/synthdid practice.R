# synthdid practice

library(synthdid)

# demo ---- 

# Estimate the effect of California Proposition 99 on cigarette consumption

data('california_prop99')

head(california_prop99)

setup = panel.matrices(california_prop99)
setup

tau.hat = synthdid_estimate(setup$Y, setup$N0, setup$T0)
se = sqrt(vcov(tau.hat, method='placebo'))

sprintf('point estimate: %1.2f', tau.hat)
sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
plot(tau.hat)


# misti full ---- 

?synthdid_estimate

m <- mistifull %>%
  select(village, wave, stab_std, treat_event)

?panel.matrices

setup <- panel.matrices(m)
setup

a <- synthdid_estimate()