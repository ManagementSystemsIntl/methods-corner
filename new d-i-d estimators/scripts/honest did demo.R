
# Honest d-i-d demo

library(HonestDiD)
library(pretrends)

data("LWdata_EventStudy")
hon <- LWdata_EventStudy
rm(LWdata_EventStudy)

str(dat)

pre <- length(dat$prePeriodIndices)

post <- length(dat$postPeriodIndices)

dat <- haven::read_dta("data/LWdata_RawData.dta")

names(dat)
head(dat)
