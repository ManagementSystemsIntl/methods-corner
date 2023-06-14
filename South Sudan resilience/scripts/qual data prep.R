# South Sudan resilience
# qual data prep 

kii <- read_excel("data/qual data/South Sudan_Data_June6.xlsx",
                  sheet="KII Data")

c1 <- kii %>%
  select(1:3) %>%
  na.omit() %>%
  set_names(nm=c("id","text","code"))

  rename(code=`Code 1`)

c2 <- kii %>%
  select(1, 2, 4) %>%
  na.omit() %>%
  set_names(nm=c("id","text","code"))

c3 <- kii %>%
  select(1, 2, 5) %>%
  na.omit() %>%
  set_names(nm=c("id","text","code"))

c4 <- kii %>%
  select(1, 2, 6) %>%
  na.omit() %>%
  set_names(nm=c("id","text","code"))


head(c4)

?pivot_longer
?pivot_wider

c1L <- c1 %>%
  group_by(`Code 1`) %>%
  mutate(row=row_number()) %>%
  pivot_wider(#id_cols="KII ID",
              names_from=`Code 1`,
              values_from=Text)

str(c1L)

write_csv(c1L, "data/qual data/KIIs code 1.csv")

c1L_1 <- c1L %>%
  slice(1)

str(c1L_1)

c1 %>%
  dplyr::group_by(`KII ID`, `Code 1`) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 




c1L_txt <- c1 %>%
  pivot_wider(id_cols=Text,
              names_from=`Code 1`,
              values_from=Text)

str(c1L)



c2L <- c2 %>%
  group_by(`Code 2`) %>%
  mutate(row=row_number()) %>%
  pivot_wider(#id_cols="KII ID",
    names_from=`Code 2`,
    values_from=Text)

str(c2L)

write_csv(c2L, "data/qual data/KIIs code 2.csv")

quaL <- c1L %>%
  left_join(c2L)

quaM <- c1L %>%
  full_merge(merge(c1L, c2L)


c3L <- c3 %>%
  pivot_wider(id_cols="KII ID",
              names_from=`Code 3`,
              values_from=Text)

c4L <- c4 %>%
  pivot_wider(id_cols="KII ID",
              names_from=`Code 4`,
              values_from=Text)


quaL <- c1L %>%
  left_join(c2L)

names(quaL)
names(c1L)

quaL_1 <- quaL %>%
  slice(1)

str(quaL_1)

new <- bind_rows(c1, c2, c3, c4) %>%
  group_by(code) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from=code,
              values_from=text)

write_csv(new, "data/qual data/KIIs by code wide.csv")









