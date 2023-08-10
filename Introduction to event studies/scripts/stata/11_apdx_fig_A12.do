#delimit ;

cap log close ;

set trace off ;
set more off ;
clear ;
set seed 101 ;

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

sort t ;	
graph twoway 
	(connected meany t if treated == 1,  msize(medsmall) 
		msymbol(o) lpattern(solid) )
	(connected meany t if treated == 0,  msize(medium) 
		msymbol(oh) lpattern(dot) )
		, legend(off) xline(9.5) 
		note("Raw means for treated and control.") 
		name(g0 , replace) ;


/* 1 normalize event time -1 to be zero */
constraint define 1 D_m1 = 0 ;

/* 2, unit FE's among all average to zero */
constraint define 2 1.i + 2.i + 3.i + 4.i + 5.i + 
	6.i + 7.i + 8.i + 9.i + 10.i = 0 ;
/* 3, unit FE's among treated average to zero */
constraint define 3 6.i + 7.i + 8.i + 9.i + 10.i = 0 ;

/* 4, 5 calendar time trends for either "treated" or "untreated" average to zero */
constraint define 4 1.i#c.t + 2.i#c.t + 3.i#c.t + 4.i#c.t + 5.i#c.t = 0 ;
constraint define 5 6.i#c.t + 7.i#c.t + 8.i#c.t + 9.i#c.t + 10.i#c.t = 0 ;

/* 6 unit-specific time trends average to zero */
constraint define 6  1.i#c.t + 2.i#c.t + 3.i#c.t + 4.i#c.t + 5.i#c.t + 
	6.i#c.t + 7.i#c.t + 8.i#c.t + 9.i#c.t + 10.i#c.t = 0 ;

/* 7 pre-treatment ES dummies average to zero */
constraint define 7 D_m10 + D_m9 + D_m8 + D_m7 + D_m6 + D_m5 + D_m4 + 
	D_m3 + D_m2 + D_m1 = 0 ; 
	
/* 8 pre-treatment ES dummies have no trend */
constraint define 8 (10-5.5)*D_m10 + (9-5.5)*D_m9 + (8-5.5)*D_m8 + (7-5.5)*D_m7 
	+ (6-5.5)*D_m6 + (5-5.5)*D_m5 + (4-5.5)*D_m4 + 
	(3-5.5)*D_m3 + (2-5.5)*D_m2 + (1-5.5)*D_m1 = 0 ; 

/* 9 first two ES dummies are the same */
constraint define 9 D_m10 = D_m9 ;

/* 10 last two ES dummies are the same */ 
constraint define 10 D_p10 = D_p9 ;

/* 11 first three ES dummies are the same */
constraint define 11 D_m9 = D_m8 ;

/* 12 last three ES dummies are the same */
constraint define 12 D_p9 = D_p8 ;

/* 13 no concavit for after end-point */
constraint define 13 D_p8 - 2 * D_p9 + D_p10 = 0 ;

/* 14 no concavit for penultimate after end-point */
constraint define 14 D_p7 - 2 * D_p8 + D_p9 = 0 ;

/* 15 no concavit for after end-point */
constraint define 15 D_m8 - 2 * D_m9 + D_m10 = 0 ;

/* 16 no concavit for penultimate after end-point */
constraint define 16 D_m7 - 2 * D_m8 + D_m9 = 0 ;



/* 2,7+: first three dummies are the same */	
cnsreg y D_m* D_p* trend_group2
	ibn.t ibn.i , nocons constraints(2 7 9 11)  collinear ;
matrix myb_ES1 = e(b) ;

/* 2,7+: first three dummies are the same , last three are the same */	
cnsreg y D_m* D_p* trend_group2
	ibn.t ibn.i , nocons constraints(2 7 9 10 11 12)  collinear ;
matrix myb_ES2 = e(b) ;

/* 2,7+: no pre-trend in ES dummies, first two dums are the same */	
cnsreg y D_m* D_p* trend_group2
	ibn.t ibn.i , nocons constraints(2 7 8 9 )  collinear ;
matrix myb_ES3 = e(b) ;

/* 2,7+: no pre-trend in ES dummies, last three dums have no concavity */	
cnsreg y D_m* D_p* trend_group2
	ibn.t ibn.i , nocons constraints(2 7 8 13 )  collinear ;
matrix myb_ES4 = e(b) ;



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

	foreach j in 1 2 3 4 { ;
		gen cf_m`j' = .	 ;	
		gen truth_m`j' = (0) in 1 ;	/* endless ramp function for treatment effect */
		replace truth_m`j' = (`i' + 1) in 2 ;	/* endless ramp function for treatment effect */
	
		local myb = 0 ;
		cap local myb = myb_ES`j'[1,"D_m`i'"] ;
		gen ES_b_m`j' = `myb' in 1 ;

		local myb = 0 ;
		cap local myb = myb_ES`j'[1,"D_p`i'"] ;
		replace ES_b_m`j' = `myb' in 2 ;
	} ;
	
		
	capture append using `pooled' ;
	save `pooled' , replace ;
} ;	




drop if label == "m0" ;
sort etime ;
save `pooled' , replace ;

list ;


local note1 "gam(-10)=gam(-9)=gam(-8)" ;
local note2 "gam(-10)=gam(-9)=gam(-8); gam(+10)=gam(+9)=gam(+8)" ;
local note3 "gam(-10)=gam(-9); pre-gammas have 0 trend" ;
local note4 "pre-gammas have 0 trend; g8 g9 g10 have no concavity." ;

forvalues i = 1/4 { ;
	graph twoway (connected ES_b_m`i' truth_m`i' etime ,  msize(medsmall medium) 
		msymbol(o oh) lpattern(solid dot) )
		(line cf_m`i' etime , lpattern(dash) )
		, legend(off) xline(-0.5) 
		note("`i': `note`i''") 
		name(g`i' , replace) ;
} ;

graph combine g1 g2 g3 g4 , ti("Add control for unit-type-trend") 
	note("All models: Avg(alpha)=0; Avg(gam(-))=0.  T=20, E1=10, E2=11.") ;
graph export figures/apdx_fig_A12.png , replace ;


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
*gen treated = i > 5 ;							/* half of units treated */
*gen E_i = 11 if treated == 1 ;

/* 2 treatment dates data structure */
gen treated = 1 ;							/* half of units treated */
gen E_i = 10 if i <= 5 ;
replace E_i = 11 if i > 5 ;


expand 20 ;									/* number of time periods */

sort i ;
qui by i: gen t = _n ;

xtset i t ;

/* make variables that determine the DGP */
gen D = (t == E_i) ; 						/* the event "pulse" */
gen etime = (t - E_i) ;						/* event time */

/* gen TE = 1 * (etime >= 0) ;					/* step function treatment effect */
*/
gen TE = (etime >= 0) * (etime+1) ;				/* endless ramp function for treatment effect */
replace TE = 0 if E_i == . ;

gen treated_post = etime >= 0 ;

*gen Y0_pure = 0 ;										/* simplest counterfactual */
*gen Y0_pure = 4 * treated +  0.3 * treated * t ;		/* treated have a pre-trend ... */
gen Y0_pure = 4 * treated +  0.1 * treated * (t-10) * (E_i - 9);	/* pre-trend based on E_i ... */

gen eps = sqrt(0.2) * rnormal() ;
gen actual = Y0_pure + TE * treated ;		
gen y = actual + eps ;						/* observed Y */


end ;


runme ;

