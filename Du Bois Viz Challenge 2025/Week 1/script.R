# Week 1
# Du Bois Visualization Challenge

# create an object of week 1 data
d <- read.csv("misc/Du Bois Viz Challenge/Week 1/data.csv")

# view dataset
view(d)

# view names of dataset
names(d)

# rename variables of dataset
d <- d %>% 
  rename(year="Year", value="Land.Value..Dollars.")

# create a ggplot of week 1 data
ggplot(d, aes(year, value)) + 
  geom_point(color=usaid_blue,
             size=2.4) + 
  geom_line(color=usaid_blue,
            alpha=.7) +
  scale_y_continuous(label=dollar_format(scale=.000001, suffix="M"),
                     sec.axis=dup_axis()) + 
  labs(x="",
       y="",
       title="Value of land owned by African-Americans in Georgia",
       caption="This is Dan's awesome figure")

# save the ggplot
# note, change file path
ggsave(here("misc/Du Bois Viz Challenge/Week 1/week 1 viz.png"),
       device="png",
       type="cairo",
       height=5,
       width=8)
