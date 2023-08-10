#delimit ;

cap log close ;

set trace off ;
set more off ;
clear ;
set seed 101 ;

cap prog drop runme ; 
prog def runme ;

/******************
	MAIN ESTIMATION OPTIONS

	1 - Panel FE DiD
	2 - Basic ES model
	3 - ES w/ unit trends
	4 - ES with pre-trends; Idea #1
	5 - ES with pre-trends; Idea #2
	6 - ES with pooled event times
	7 - ES with dropped end-points
	
	Variations, when there are never-treated units
		(a) Include them; (b) exclude them
	
*******************/

gendata ;
summ ;
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



/* 1 unit FE's among treated / untreated average to zero */
constraint define 1 1.i + 2.i + 3.i + 4.i + 5.i + 
	6.i + 7.i + 8.i + 9.i + 10.i = 0 ;

/* 2 pre-treatment ES dummies average to zero */
constraint define 2 D_m10 + D_m9 + D_m8 + D_m7 + D_m6 + D_m5 + D_m4 + 
	D_m3 + D_m2 + D_m1 = 0 ; 
	
/* 3 -1 also equals 0 */
constraint define 3 D_m1 = 0 ; 

/* 4 pre-treatment ES dummies average to zero */
constraint define 4 D_m10 = 0 ; 

/* 5 -10 = -9 */
constraint define 5 D_m10 = D_m9 ; 

/* 6 pre-treatment ES dummies average to zero */
constraint define 6 D_m1 = D_m2 ; 

/* 7 -10 = -1*/
constraint define 7 D_m10 = D_m1 ; 

/* 8 pre-treatment ES dummies have no trend */
constraint define 8 (10-5.5)*D_m10 + (9-5.5)*D_m9 + (8-5.5)*D_m8 + (7-5.5)*D_m7 
	+ (6-5.5)*D_m6 + (5-5.5)*D_m5 + (4-5.5)*D_m4 + 
	(3-5.5)*D_m3 + (2-5.5)*D_m2 + (1-5.5)*D_m1 = 0 ; 
	
/* 9 post-treatment ES dummies 8=9*/
constraint define 9 D_p8 = D_p9 ; 

/* 10 no curvature */
constraint define 10 D_m10 - D_m9 = D_m9 - D_m8 ; 
	
/* 11 more no curvature */
constraint define 11 D_m9 - D_m8 = D_m8 - D_m7 ; 
	
/* 12 more no curvature */
constraint define 12 D_m8 - D_m7 = D_m7 - D_m6 ; 
	
/* 13 -10 = -9 */
constraint define 13 D_m9 = D_m8 ; 

/* 14 -10 = -9 */
constraint define 14 D_m8 = D_m7 ; 

/* 14 -10 = -9 */
constraint define 15 D_m7 = D_m6 ; 

	
/* ES model, no trends */
cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i  , constraints(1 2) nocons collinear ;
matrix myb_ES1 = e(b) ; 
matrix myV_ES1 = e(V) ;

/* ES model, add trends, it will crash */
*cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i  trend_group2 , constraints(1 2) nocons collinear ;

/* ES model, add trends, 8=9*/
cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i  trend_group2 , constraints(1 2 9) nocons collinear ;
matrix myb_ES2 = e(b) ; 
matrix myV_ES2 = e(V) ;

/* ES model, add trends, -10 = -9 */
cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i  trend_group2 , constraints(1 2 5) nocons collinear ;
matrix myb_ES3 = e(b) ; 
matrix myV_ES3 = e(V) ;


/* ES model, add trends, -1 = -2 */
cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i  trend_group2 , constraints(1 2 6) nocons collinear ;
matrix myb_ES4 = e(b) ; 
matrix myV_ES4 = e(V) ;

/* ES model, add trends, -10 = -1 */
cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i  trend_group2 , constraints(1 2 7) nocons collinear ;
matrix myb_ES5 = e(b) ; 
matrix myV_ES5 = e(V) ;

/* ES model, force zero pre-trend */
cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i  trend_group2 , constraints(1 2 8) nocons collinear ;
matrix myb_ES6 = e(b) ; 
matrix myV_ES6 = e(V) ;

di 1 ;
/* ES model, 1 step no-curvature */
/* crashes! 
cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i  trend_group2 , constraints(1 2 10) nocons collinear ;
/* ES model, many steps no-curvature */
ALSO crashes!
cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i  trend_group2 , constraints(1 2 10 11 12) nocons collinear ;
*/


cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i  trend_group2 , constraints(1 2 5 13 ) nocons collinear ;
matrix myb_ES7 = e(b) ; 
matrix myV_ES7 = e(V) ;
di 2 ;


cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i  trend_group2 , constraints(1 2 5 13 14 ) nocons collinear ;
matrix myb_ES8 = e(b) ; 
matrix myV_ES8 = e(V) ;

di 3 ;

/* ES model, */
cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i  trend_group2 , constraints(1 2 5 13 14 15) nocons collinear ;
matrix myb_ES9 = e(b) ; 
matrix myV_ES9 = e(V) ;





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

	foreach j in 1 2 3 4 5 6 7 8 9 { ;
		/* models with trends */
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
drop if label == "p10" ;
sort etime ;
save `pooled' , replace ;

gen truth = (etime >= 0) * (etime+1) ;			/* endless ramp function for treatment effect */
replace truth = . if abs(etime + 0.5) < 0.05 ;

list ;

local note1 "No unit-specific trends" ;
local note2 "Unit trends; +8 = +9" ;
local note3 "Unit trends; -10 = -9" ;
local note4 "Unit trends; -1 = -2" ;
local note5 "Unit trends; -10 = -1" ;
local note6 "Unit trends; zero pre-trend coeffs" ;
local note7 "Unit trends; -10 = -9 = -8 " ;
local note8 "Unit trends; -10 = -9 = -8 = -7 " ;
local note9 "Unit trends; -10 = -9 = -8 = -7 = -6" ;


forvalues i = 1/9 { ;
	graph twoway (connected ES_b_m`i' truth etime ,  msize(medsmall large) 
		msymbol(o oh) lpattern(solid dash)) 
		, legend(off) xline(-0.5) 
		note("`i': `note`i''") 
		name(g`i' , replace) ;
} ;

graph combine g1 g3 g4 g2 g5 g6 g7 g8 g9,
	note("DiD Data structure. T=20, E=11.  DGP has No trends.") ;
graph export figures/apdx_fig_A11.png , replace ;


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

*gen TE = 1 * (etime >= 0) ;				/* step function treatment effect */
gen TE = (etime >= 0) * (etime+1) ;				/* endless ramp function for treatment effect */
replace TE = 0 if E_i == . ;

gen treated_post = etime >= 0 ;

gen Y0_pure = 0 + 1 * treated ;								/* offset counterfactual */
*gen Y0_pure = 4 * treated +  0.3 * treated * t ;		/* treated have a pre-trend ... */
*gen Y0_pure = 4 * treated +  0.01 * treated * (t-10) * (E_i - 9);	/* pre-trend based on E_i ... */

gen eps = sqrt(0.3) * rnormal() ;
gen actual = Y0_pure + TE * treated ;		
gen y = actual + eps ;						/* observed Y */


end ;


runme ;


