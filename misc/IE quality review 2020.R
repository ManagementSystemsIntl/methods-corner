
getwd()

sc_w <- read_xlsx("Scoring sheets/IE Quality Review - R4 PreTest1 with r templates.xlsx",
                sheet="rtemplate wide")

sc <- read_xlsx("Scoring sheets/IE Quality Review - R4 PreTest1 with r templates.xlsx",
                sheet="rtemplate")

write_csv(sc, "Scoring sheets/scoring sheet long.csv")

dat <- read_dta("IEreview_dataset_use.dta")

write_csv(dat, "IEreview_dataset_use.csv")

frq(dat$type)
frq(dat$sector)
frq(dat$region)
