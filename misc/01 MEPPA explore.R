# MEPPA
# data understand

ogs <- read_excel("data/Our Generation Speaks baseline data.xlsx",
                  sheet="export")

frq(ogs$participation)

names(ogs)

# prepare ---- 

ogs2 <- ogs %>%
  mutate(across() )


# explore ---- 


frq(ogs$sex)
frq(ogs$age_cat)
frq(ogs$identification)
frq(ogs$governorate)
frq(ogs$educ)
frq(ogs$understand_political)
frq(ogs$understand_social)
frq(ogs$understand_economic)
frq(ogs$new_proj)
