use atrisk, clear
describe

** Estimate
qui gsem (alcohol truant vandalism theft weapon <- , logit), lclass(C 1)
estimates store c1
qui gsem (alcohol truant vandalism theft weapon <- , logit), lclass(C 2)
estimates store c2
qui gsem (alcohol truant vandalism theft weapon <- , logit), lclass(C 3) iter(500) 
estimates store c3
qui gsem (alcohol truant vandalism theft weapon <- , logit), lclass(C 4) iter(1000)
*convergence not achieved

estimates stats c*


** Probabilities
estimates restore c2
estat lcprob

estimates restore c3
estat lcprob


** Means
estimates restore c2
estat lcmean

estimates restore c3
estat lcmean

*Class characteristics plot
estimates restore c3
estat lcmean
marginsplot, scheme(S2mono) ///
  title("Class characteristics") xtitle("") ///
  xlabel(1 "alcohol" 2 "truant" 3 "vandalism" 4 "theft" 5 "weapon", angle(45)) ///
  legend(order(4 "Class 1 (90%)" 5 "Class 2 (8%)" 6 "Class 3 (2%)") rows(1)) 

  
** Predict class membership
estimates restore c3
predict cpr*, classposteriorpr
egen maxpr = rowmax(cpr*)
generate predclass = 1 if cpr1==maxpr
replace predclass = 2 if cpr2==maxpr
replace predclass = 3 if cpr3==maxpr
sort id
list id alcohol truant vandalism theft weapon cp* maxpr predclass in 30/45, noobs sep(0)

* Classification matrix
label variable cpr1 "Class1pr"
label variable cpr2 "Class2pr"
label variable cpr3 "Class3pr"
table predclass, statistic(mean cpr1  cpr2 cpr3)


** Goodness of Fit
estat lcgof


** Adding covariates to the model
qui gsem (alcohol truant vandalism theft weapon <- , logit) (C <- i.male age), ///
lclass(C 3) startvalues(classpr cpr1 cpr2 cpr3)

qui margins male, at(age=(13/18)) predict(classpr class(1)) ///
predict(classpr class(2)) predict(classpr class(3))

marginsplot, by(male) ytitle("Probability of class membership")  	///
  byopts(title("Predicted latent class probabilities with 95% CI")) 	///
  legend(order(4 "Class 1" 5 "Class 2" 6 "Class 3") rows(1))
