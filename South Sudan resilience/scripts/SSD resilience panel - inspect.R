# South Sudan resilience panel survey
# inspection/cleaning/exploration


getwd()

source("0 South Sudan resilience panel survey - prep.r")

<<<<<<< HEAD
d <- read_csv("data/daily/mels_resilience_panel_survey_english (5-16-23).csv")

d <- d %>%
  mutate(id = 1:238) %>%
=======
d <- read_csv("data/daily/mels_resilience_panel_survey_english (5-17-23).csv")

d <- d %>%
  mutate(id = 1:nrow(d)) %>%
>>>>>>> 11b7768760faa0551aa4c4e02cad64333a566399
  dplyr::select(id, everything())





