use did_exit_6, replace

xtset id year

bysort year: tab w

sort id year

list y w year in 133/168, sep(6)

list y w year in 91/126, sep(6)

* Without covariate and constant effect:

reg y w i.year d4_inf d4_5 d4_6 d5_inf d5_6 d6_inf, vce(cluster id)

* Heterogeneous effects:

reg y c.d4_inf#c.f04 c.d4_inf#c.f05 c.d4_inf#c.f06 ///
	c.d4_6#c.f04 c.d4_6#c.f05 c.d4_6#c.f06 ///
	c.d4_5#c.f04 c.d4_5#c.f05 c.d4_5#c.f06 ///
	c.d5_inf#c.f05 c.d5_inf#c.f06 c.d5_6#c.f05 c.d5_6#c.f06 c.d6_inf#c.f06 ///
	i.year d4_inf d4_5 d4_6 d5_inf d5_6 d6_inf, vce(cluster id)
	
* TWFE is the same:
	
xtreg y c.d4_inf#c.f04 c.d4_inf#c.f05 c.d4_inf#c.f06 ///
	c.d4_6#c.f04 c.d4_6#c.f05 c.d4_6#c.f06 ///
	c.d4_5#c.f04 c.d4_5#c.f05 c.d4_5#c.f06 ///
	c.d5_inf#c.f05 c.d5_inf#c.f06 c.d5_6#c.f05 c.d5_6#c.f06 c.d6_inf#c.f06 ///
	i.year, fe vce(cluster id)

qui sum x if d4_inf
gen x_dm4_inf = x - r(mean)
qui sum x if d4_5
gen x_dm4_5 = x - r(mean)
qui sum x if d4_6
gen x_dm4_6 = x - r(mean)
qui sum x if d5_inf
gen x_dm5_inf = x - r(mean)
qui sum x if d5_6
gen x_dm5_6 = x - r(mean)
qui sum x if d5_inf
gen x_dm6_inf = x - r(mean)


reg y c.d4_inf#c.f04 c.d4_inf#c.f05 c.d4_inf#c.f06 ///
	c.d4_6#c.f04 c.d4_6#c.f05 c.d4_6#c.f06 ///
	c.d4_5#c.f04 c.d4_5#c.f05 c.d4_5#c.f06 ///
	c.d5_inf#c.f05 c.d5_inf#c.f06 c.d5_6#c.f05 c.d5_6#c.f06 c.d6_inf#c.f06 ///
	c.d4_inf#c.f04#c.x_dm4_inf c.d4_inf#c.f05#c.x_dm4_inf c.d4_inf#c.f06#c.x_dm4_inf ///
	c.d4_6#c.f04#c.x_dm4_6 c.d4_6#c.f05#c.x_dm4_6 c.d4_6#c.f06#c.x_dm4_6 ///
	c.d4_5#c.f04#c.x_dm4_5 c.d4_5#c.f05#c.x_dm4_5 c.d4_5#c.f06#c.x_dm4_5 ///
	c.d5_inf#c.f05#c.x_dm5_inf c.d5_inf#c.f06#c.x_dm5_inf ///
	c.d5_6#c.f05#c.x_dm5_6 c.d5_6#c.f06#c.x_dm5_6 c.d6_inf#c.f06#c.x_dm6_inf ///
	i.year i.year#c.x ///
	d4_inf d4_5 d4_6 d5_inf d5_6 d6_inf x ///
	c.d4_inf#c.x c.d4_5#c.x c.d4_6#c.x c.d5_inf#c.x c.d5_6#c.x c.d6_inf#c.x, vce(cluster id)

* Same if include only post-treatment dummies and interactions with x:
	
reg y c.d4_inf#c.f04 c.d4_inf#c.f05 c.d4_inf#c.f06 ///
	c.d4_6#c.f04 c.d4_6#c.f05 c.d4_6#c.f06 ///
	c.d4_5#c.f04 c.d4_5#c.f05 c.d4_5#c.f06 ///
	c.d5_inf#c.f05 c.d5_inf#c.f06 c.d5_6#c.f05 c.d5_6#c.f06 c.d6_inf#c.f06 ///
	c.d4_inf#c.f04#c.x_dm4_inf c.d4_inf#c.f05#c.x_dm4_inf c.d4_inf#c.f06#c.x_dm4_inf ///
	c.d4_6#c.f04#c.x_dm4_6 c.d4_6#c.f05#c.x_dm4_6 c.d4_6#c.f06#c.x_dm4_6 ///
	c.d4_5#c.f04#c.x_dm4_5 c.d4_5#c.f05#c.x_dm4_5 c.d4_5#c.f06#c.x_dm4_5 ///
	c.d5_inf#c.f05#c.x_dm5_inf c.d5_inf#c.f06#c.x_dm5_inf ///
	c.d5_6#c.f05#c.x_dm5_6 c.d5_6#c.f06#c.x_dm5_6 c.d6_inf#c.f06#c.x_dm6_inf ///
	f04 f05 f06 c.f04#c.x c.f05#c.x c.f06#c.x ///
	d4_inf d4_5 d4_6 d5_inf d5_6 d6_inf x ///
	c.d4_inf#c.x c.d4_5#c.x c.d4_6#c.x c.d5_inf#c.x c.d5_6#c.x c.d6_inf#c.x, vce(cluster id)

* RE and TWFE are the same:
	
xtreg y c.d4_inf#c.f04 c.d4_inf#c.f05 c.d4_inf#c.f06 ///
	c.d4_6#c.f04 c.d4_6#c.f05 c.d4_6#c.f06 ///
	c.d4_5#c.f04 c.d4_5#c.f05 c.d4_5#c.f06 ///
	c.d5_inf#c.f05 c.d5_inf#c.f06 c.d5_6#c.f05 c.d5_6#c.f06 c.d6_inf#c.f06 ///
	c.d4_inf#c.f04#c.x_dm4_inf c.d4_inf#c.f05#c.x_dm4_inf c.d4_inf#c.f06#c.x_dm4_inf ///
	c.d4_6#c.f04#c.x_dm4_6 c.d4_6#c.f05#c.x_dm4_6 c.d4_6#c.f06#c.x_dm4_6 ///
	c.d4_5#c.f04#c.x_dm4_5 c.d4_5#c.f05#c.x_dm4_5 c.d4_5#c.f06#c.x_dm4_5 ///
	c.d5_inf#c.f05#c.x_dm5_inf c.d5_inf#c.f06#c.x_dm5_inf ///
	c.d5_6#c.f05#c.x_dm5_6 c.d5_6#c.f06#c.x_dm5_6 c.d6_inf#c.f06#c.x_dm6_inf ///
	i.year i.year#c.x ///
	d4_inf d4_5 d4_6 d5_inf d5_6 d6_inf x ///
	c.d4_inf#c.x c.d4_5#c.x c.d4_6#c.x c.d5_inf#c.x c.d5_6#c.x c.d6_inf#c.x, re vce(cluster id)
	
xtreg y c.d4_inf#c.f04 c.d4_inf#c.f05 c.d4_inf#c.f06 ///
	c.d4_6#c.f04 c.d4_6#c.f05 c.d4_6#c.f06 ///
	c.d4_5#c.f04 c.d4_5#c.f05 c.d4_5#c.f06 ///
	c.d5_inf#c.f05 c.d5_inf#c.f06 c.d5_6#c.f05 c.d5_6#c.f06 c.d6_inf#c.f06 ///
	c.d4_inf#c.f04#c.x_dm4_inf c.d4_inf#c.f05#c.x_dm4_inf c.d4_inf#c.f06#c.x_dm4_inf ///
	c.d4_6#c.f04#c.x_dm4_6 c.d4_6#c.f05#c.x_dm4_6 c.d4_6#c.f06#c.x_dm4_6 ///
	c.d4_5#c.f04#c.x_dm4_5 c.d4_5#c.f05#c.x_dm4_5 c.d4_5#c.f06#c.x_dm4_5 ///
	c.d5_inf#c.f05#c.x_dm5_inf c.d5_inf#c.f06#c.x_dm5_inf ///
	c.d5_6#c.f05#c.x_dm5_6 c.d5_6#c.f06#c.x_dm5_6 c.d6_inf#c.f06#c.x_dm6_inf ///
	i.year i.year#c.x, fe vce(cluster id)
	
