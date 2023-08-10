#delimit ;

cap log close ;

set trace off ;
set more off ;
clear ;
set seed 10103 ;

cap prog drop runme ; 
prog def runme ;


gendata ;
/* create variables used for estimation */
/* event time dummies */

forvalues i = 0/20 { ;
	gen D_p`i' = (etime == `i') ;
	gen D_m`i' = (etime == -1 * `i') ;
	summ D_p`i' ;
	local dropme = (r(mean) == 0) ;
	if `dropme' == 1 { ;
		drop D_p`i'  ;
	} ;
	summ D_m`i' ;
	local dropme = (r(mean) == 0) ;
	if `dropme' == 1 { ;
		drop D_m`i'  ;
	} ;
} ;

drop D_m0  ;

gen byte group2 = E_i == 11 ;
gen trend_group2 = t * group2 ;

summ ; 

egen meany = mean(y) , by(t treated) ;

/* 1 normalize event time -1 to be zero */
constraint define 1 D_m1 = 0 ;

/* 2, unit FE's among all average to zero */
constraint define 2 1.i + 2.i + 3.i + 4.i + 5.i + 
	6.i + 7.i + 8.i + 9.i + 10.i = 0 ;

/* 7 pre-treatment ES dummies average to zero */
constraint define 7 D_m10 + D_m9 + D_m8 + D_m7 + D_m6 + D_m5 + D_m4 + 
	D_m3 + D_m2 + D_m1 = 0 ; 
	

cnsreg y D_m* D_p* 
	ibn.t ibn.i , cluster(i) nocons constraints(2 1 )  collinear ;
matrix myb_ES1 = e(b) ;
matrix myV_ES1 = e(V) ;

cnsreg y D_m* D_p* 
	ibn.t ibn.i , cluster(i) nocons constraints(2 7 )  collinear ;
matrix myb_ES2 = e(b) ;
matrix myV_ES2 = e(V) ;


tempfile main ;
save `main' ;

/* now get the results ready to plot out */

*set trace on ;
tempfile pooled ;
forvalues i = 0/10 { ;
	drop _all ;
	set obs 2 ;
	gen label = "m`i'" in 1 ;
	replace label = "p`i'" in 2 ;

	gen etime = -1 * `i' in 1 ;
	replace etime = `i' in 2 ;

	foreach j in 1 2 { ;
		gen cf_m`j' = .	 ;	

		/* step function for treatment effect */		
		gen truth_m`j' = (0) in 1 ;	
		replace truth_m`j' = (1) in 2 ;
	
		local myb = 0 ;
		cap local myb = myb_ES`j'[1,"D_m`i'"] ;
		gen ES_b_m`j' = `myb' in 1 ;
		local myv = . ;
		cap local myv = myV_ES`j'["D_m`i'","D_m`i'"] ;
		local myse = sqrt(`myv') ;
		gen ES_se_m`j' = `myse' in 1 ;

		local myb = 0 ;
		cap local myb = myb_ES`j'[1,"D_p`i'"] ;
		replace ES_b_m`j' = `myb' in 2 ;
		local myv = . ;
		cap local myv = myV_ES`j'["D_p`i'","D_p`i'"] ;
		local myse = sqrt(`myv') ;
		replace ES_se_m`j' = `myse' in 2 ;
	} ;
	
		
	capture append using `pooled' ;
	save `pooled' , replace ;
} ;	




drop if label == "m0" ;
drop if label == "p10" ;
sort etime ;
save `pooled' , replace ;


local note1 "gam(-1)=0" ;
local note2 "Avg(gam(-))=0." ;
local note3 "Each gam(-)=0." ;

forvalues i = 1/2 { ;

	gen top_`i' = ES_b_m`i' + 1.96 * ES_se_m`i' ; 
	gen bot_`i' = ES_b_m`i' - 1.96 * ES_se_m`i' ; 

	graph twoway (connected ES_b_m`i' truth_m`i' etime ,  msize(medsmall medium) 
		msymbol(o oh) lpattern(solid dot) )
		(line cf_m`i' etime , lpattern(dash) )
		(line top_`i' bot_`i' etime, lpattern(dash dash) lcolor(brown brown))
		, legend(off) xline(-0.5) 
		note("`i': `note`i''") 
		name(g`i' , replace) yscale(range(-3,3.5)) yline(0 , lpattern(dash) lcolor(black)) ;
} ;

graph combine g1 g2 , ti("We can still see pre-trends") 
	note("All models: Avg(alpha)=0; DiD data structure. T=20, E1=11.") ;
graph export figures/apdx_fig_A05.png , replace ;


end ;

cap prog drop gendata ;
program define gendata ;

/*****************
	MAIN DGP OPTIONS
	
/* Possible Treatment Effect Types */
	1 - zero TE
	2 - Step fn TE
	3 - Ramp-up Forever
	4 - Ramp-up Plateau
	5 - AR(1) type

/* Distribution of E_i */
	T = 19; E_i ~ U(6,14)

/* Are there Never-treated units ?? */
	Yes; No
	
/*  Y0 ("signal" for potential outcomes) dynamics */
	1 - Base: Y0 = 0
	2 - Levels variation: Y0_i,t = E_i
	3 - trends variation: Y0_i,t = t * E_i

	END OF MAIN OPTIONS
******************/

drop _all ;

/*
/* classic ES data structure */
set obs 9 ;									/* number of units */
gen i = _n ;
gen E_i = 5 + i ;
gen treated = 1 ;							/* all units treated */
expand 20 ;									/* number of time periods */
*/

set obs 10 ;									/* number of units */
gen i = _n ;

/* NxT DiD data structure */
gen treated = i > 5 ;							/* half of units treated */
gen E_i = 11 if treated == 1 ;

/*
/* 2 treatment dates data structure */
gen treated = 1 ;							/* half of units treated */
gen E_i = 10 if i <= 5 ;
replace E_i = 11 if i > 5 ;
*/

expand 20 ;									/* number of time periods */

sort i ;
qui by i: gen t = _n ;

xtset i t ;

/* make variables that determine the DGP */
gen D = (t == E_i) ; 						/* the event "pulse" */
gen etime = (t - E_i) ;						/* event time */

gen TE = 1 * (etime >= 0) ;				/* step function treatment effect */
*gen TE = (etime >= 0) * (etime+1) ;				/* endless ramp function for treatment effect */
replace TE = 0 if E_i == . ;

gen treated_post = etime >= 0 ;

*gen Y0_pure = 0 + 1 * treated ;								/* offset counterfactual */
gen Y0_pure = 4 * treated -  0.15 * treated * t ;		/* treated have a pre-trend ... */
*gen Y0_pure = 4 * treated +  0.01 * treated * (t-10) * (E_i - 9);	/* pre-trend based on E_i ... */

gen eps = sqrt(0.07) * rnormal() ;
gen actual = Y0_pure + TE * treated ;		
gen y = actual + eps ;						/* observed Y */


end ;


runme ;


