# South Sudan resilience panel survey
# inspection/cleaning/exploration


getwd()

source("0 South Sudan resilience panel survey - prep.r")

d <- read_csv("data/daily/mels_resilience_panel_survey_english (5-17-23).csv")

d <- d %>%
  mutate(id = 1:nrow(d)) %>%
  dplyr::select(id, everything())





