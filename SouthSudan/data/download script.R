
#dat <- acled.api(email.address = "email"
#          , access.key = "passcode"
 #         , country = "South Sudan"
 #         , all.variables = TRUE)

#saved the data here
writexl::write_xlsx(dat, path = "SouthSudan/data/SSudan.xlsx")

#create my data object here
events <- read_xlsx("SouthSudan/data/SSudan.xlsx")
