# DIDmultiplegt practice
# de Chaisemartin and D'Haultfoeuille (2020)

?did_multiplegt

here()

mistifull <- read_rds("data/MISTI villages measured all waves.rds")

# stability ---- 

a <- did_multiplegt(df=)

a <- did_multiplegt(mistifull,
                    Y="stab_std",
                    G = "village",
                    T = "wave",
                    D = "treat_event",
                    brep=50,
                    placebo=3,
                    dynamic=3)
,
trends_lin = "village")

summary(a)
a

write_rds(a, "models/did_multiplegt/didmultiplegt model 1.rds")

a_d <- data.frame(a) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("measure") %>%
  rename(value=V1)

a_d

?did_multiplegt
didmult_stab2 <- did_multiplegt(mistifull,
                       Y="stab_std",
                       G = "village",
                       T = "wave",
                       D = "treat_event",
                       cluster="village",
                       average_effect="simple",
                       brep=50,
                       placebo=3,
                       dynamic=3)

didmult_stab2

didmult_stab2_dat <- data.frame(didmult_stab2) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column("measure") %>%
  rename(value=V1)

didmult_stab2_dat
str(didmult_stab2_dat)


didmult_stab2out <- data.frame(to_treat=-3:3,
                               stab=didmult_stab2_dat[c(1,4,7,10,14,18,22),2],
                               stab_se=didmult_stab2_dat[c(2,5,8,11,15,19,23),2]) %>%
  mutate(lower=stab-1.96*stab_se,
         upper=stab+1.96*stab_se)

didmult_stab2out

ggplot(didmult_stab2out, aes(to_treat, stab)) + 
  geom_hline(yintercept=0, color="grey60", size=1, alpha=.5) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0, color="dodgerblue", size=1) + 
  geom_label(aes(label=round(stab,3)),
             color="dodgerblue2", size=5) + 
  scale_x_continuous(breaks=-3:3) +
  scale_y_continuous(breaks=seq(-.5,1.5,.25)) +
  labs(x="", y="", title="MISTI dynamic treatment effects on stability\nusing de Chaisemartin & D'Haultfoeuille (2020)",
       caption="did_multiplegt")

ggsave("viz/didmult_stab2.png",
       type="cairo",
       device="png",
       height=4,
       width=7)













