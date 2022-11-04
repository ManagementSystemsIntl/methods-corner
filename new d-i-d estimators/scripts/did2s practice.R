# did2s practice

mistiful <- mistiful %>%
  mutate(time_to_treat=case_when(treat_event==1 & wave==1 ~ ))

# demo ---- 

data("df_het")

head(df_het)

?did2s

  # event study

es <- did2s(df_het,
            yname = "dep_var", first_stage = ~ 0 | state + year, 
            second_stage = ~i(rel_year, ref=c(-1, Inf)), treatment = "treat", 
            cluster_var = "state")


es
iplot(es)

# MISTI ---- 

st <- did2s(mistifull, 
            yname="stab_std",
            first_stage = ~0 | village + wave,
            second_stage="treat",
            treatment="treat",
            cluster_var="village")

st
iplot(st)
summary(st)
str(st)

esttable(st)

?feols


res <- did2s(mistifull, 
            yname="resil_std",
            first_stage = ~0 | village + wave,
            second_stage="treat",
            treatment="treat",
            cluster_var="village")

res


