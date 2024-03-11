
names(hh_fin)

frq(hh_fin$respondent_type)
frq(hh_fin$Survey_type)
frq(hh_fin$village_status)

vil <- hh_fin %>%
  select(Slno, respondent_type:spill) %>%
  filter(midline==1) %>%
  distinct(village, .keep_all=T) %>%
  arrange(village_status, village_name)

?unique
?distinct


d <- read_sav("data/midline/raw/received data/20190527 Merged Dataset_Midline & Baseline Survey (BPI)_V2.sav")

geor <- d %>%
  select(Slno, district=6, subdistrict=7, union=8, village=9, lat=10, lon=11, vdo=12) %>%
  mutate(village_name=as_character(village)) %>%
  distinct(village, .keep_all=T)

frq(geor$a12)

write_csv(geor, "data/midline/raw/received data/survey villages.csv")

geor <- read_csv("data/midline/raw/received data/survey villages.csv")

coords <- read_csv("data/midline/raw/received data/Household_distance.csv")

coords2 <- coords %>%
  select(id, Slno, unique_hh_, lat=4, lon=5, village)

str(coords2)

coords_dstnct <- coords2 %>%
  group_by(village) %>%
  summarise(lat=mean(lat),
            lon=mean(lon))

vil2 <- vil %>%
  left_join(coords2)

str(vil2)



codes <- read_excel("../bpi/data/VDO codes.xlsx")

codes2 <- codes %>%
  select(1:10, lat=15, lon=14) %>%
  mutate(lon=as.numeric(gsub(",", "", lon)))

str(codes2)

codes3 <- codes2 %>%
  group_by(District, Subdistrict, Union, Village, `Village code`) %>%
  summarise(lat=mean(lat),
            lon=mean(lon)) 

codes4 <- codes3 %>%
  as.data.frame() %>%
  select(village=5, 6, 7)

?str_sub
?replace

vil3 <- vil2 %>%
  left_join(codes4)

viltemp <- vil2 %>%
  select(village, midline:spill)


codes6 <- codes4 %>%
  left_join(viltemp)

head(codes6)

d_sf <- codes6 %>%
  st_as_sf(coords=c(lat="lat", long="lon"),
           crs=4326)

head(d_sf)

?gadm

bang <- gadm(country="BD",
             path="data/mapping")

plot(bang)

bang2 <- gadm(country="BD",
              level=2,
             path="data/mapping")

plot(bang2)

bang3 <- gadm(country="BD",
              level=3,
             path="data/mapping")

plot(bang3)

bang3_r <- read_rds("data/mapping/gadm/gadm41_BGD_3_pk.rds")

bang3_sf <- bang3 %>%
  st_as_sf()

bang0_sf <- st_read("data/mapping/gadm/gadm41_BGD.gpkg")
bang1_sf <- st_read("data/mapping/gadm/gadm41_BGD.gpkg",
                    layer="ADM_ADM_1")
bang2_sf <- st_read("data/mapping/gadm/gadm41_BGD.gpkg",
                    layer="ADM_ADM_2")
bang3_sf <- st_read("data/mapping/gadm/gadm41_BGD.gpkg",
                    layer="ADM_ADM_3")
bang4_sf <- st_read("data/mapping/gadm/gadm41_BGD.gpkg",
                    layer="ADM_ADM_4")

?st_read
?st_as_sf

st_layers("data/mapping/gadm/gadm41_BGD.gpkg")

bang4 <- gadm(country="BD",
              level=4,
              path="data/mapping")


bang_sf <- as_sf(bang)

bang0_box <- st_bbox(d_sf)

ggplot() +
  geom_sf(data=bang3_sf, aes(geometry=geom),
          color="dodgerblue2", 
          alpha=.8,
          size=2) + 
  geom_sf(data=bang2_sf, aes(geometry=geom),
          color="grey60",
          size=4,
          fill=NA) +
  geom_sf(data=d_sf, size=1,
          color="darkgoldenrod2", alpha=.6) + 
  coord_sf(xlim=c(bang0_box[[1]], bang0_box[[3]]),
           ylim=c(bang0_box[[2]], bang0_box[[4]])) +
  #theme_void() +
  theme_bw()







