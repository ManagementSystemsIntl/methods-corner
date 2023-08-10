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
drop D_m0 D_m1 ;

gen timeholder = t ;

/* DD model, no trends */
reg y treated_post i.t i.i ;
matrix myb_DD = e(b) ;
matrix myV_DD = e(V) ;

/* DD model, unit-specific trends */
reg y treated_post i.t i.i i.i#c.t ;
matrix myb_DD_trend = e(b) ;
matrix myV_DD_trend = e(V) ;

/* ES model, no trends */
reg y D_m* D_p* i.t i.i ;
reg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	i.t i.i ;
matrix myb_ES = e(b) ;
matrix myV_ES = e(V) ;


/* ES model, naively adding trends; 1 extra degree of collinearity 
	Note, 10 trends added (1/unit) , but model DOF only goes up by 8
	time dummies are collinear; also interaction of D* terms */
reg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	i.t i.i i.i#c.t ;

/* Let's choose what to omit.  Each model is equivalent, but they produce different
	ES-type graphs */
reg y /* D_m10 */ D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	i.t i.i i.i#c.t ;

reg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 /* D_m2 */
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	i.t i.i i.i#c.t ;

reg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 
	D_p0 /* D_p1 */ D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9
	i.t i.i i.i#c.t ;

reg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 
	D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 /* D_p9 */
	i.t i.i i.i#c.t ;


tempfile main ;
save `main' ;

/* now get the results ready to plot out */

local dd_shift = 0.1 ;
local dd_pre = `dd_shift' ;		/* later replace this with avg. yhat pre period, or something */
local dd_post = `dd_shift' + myb_DD[1,"treated_post"] ;

local dd_pre_trend = 2 * `dd_shift' ;
local dd_post_trend = 2 * `dd_shift' + myb_DD_trend[1,"treated_post"] ;

tempfile pooled ;
forvalues i = 0/10 { ;
	drop _all ;
	set obs 2 ;
	gen label = "m`i'" in 1 ;
	replace label = "p`i'" in 2 ;
	
	gen etime = -1 * `i' in 1 ;
	replace etime = `i' in 2 ;

	local myb = . ;
	cap local myb = myb_ES[1,"D_m`i'"] ;
	gen ES_b = `myb' in 1 ;

	local myb = . ;
	cap local myb = myb_ES[1,"D_p`i'"] ;
	replace ES_b = `myb' in 2 ;

	gen DD_b = `dd_pre' in 1 ;
	replace DD_b = `dd_post' in 2 ;
	
	gen DD_b_trend = `dd_pre_trend' in 1 ;
	replace DD_b_trend = `dd_post_trend' in 2 ;
	
	capture append using `pooled' ;
	save `pooled' , replace ;
} ;

drop _all ;
set obs 2 ;
gen etime = -0.51 in 1 ;
replace etime = -0.49 in 2 ;
gen DD_b = `dd_pre' in 1 ;
replace DD_b = `dd_post' in 2 ;
gen DD_b_trend = `dd_pre_trend' in 1 ;
replace DD_b_trend = `dd_post_trend' in 2 ;
capture append using `pooled' ;


drop if label == "m0" ;
drop if label == "p10" ;
replace ES_b = 0 if label == "m1" ;
sort etime ;
save `pooled' , replace ;

/*
use `main' if treated == 1 ;
summ yhat* ;
collapse yhat_dd yhat_es , by(etime) ;
sort etime ;
merge 1:1 etime using `pooled' ;
sort etime ;
*/

gen truth = (etime >= 0) * (etime+1) ;			/* endless ramp function for treatment effect */
replace truth = . if abs(etime +0.5) < 0.05 ;

list ;

graph twoway 
	(connected ES_b truth etime ,  msize(medsmall large) msymbol(o oh) lpattern(solid dash)) 
	(line DD_b DD_b_trend etime , lwidth(medthick))
	, legend(off) xline(-0.5) 
	note("DD models with unit-specific trends can get messed up when TE trends") ;
graph export figures/apdx_fig_A16.png , replace ;

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
expand 19 ;									/* number of time periods */
*/

/* NxT DiD data structure */
set obs 10 ;									/* number of units */
gen i = _n ;
gen treated = i > 5 ;							/* half of units treated */
gen E_i = 11 if treated == 1 ;
expand 20 ;									/* number of time periods */

sort i ;
qui by i: gen t = _n ;

xtset i t ;

/* make variables that determine the DGP */
gen D = (t == E_i) ; 						/* the event "pulse" */
gen Y0_pure = 0 ;							/* simplest counterfactual */
gen etime = (t - E_i) ;						/* event time */

/* gen TE = 1 * (etime >= 0) ;					/* step function treatment effect */
*/
gen TE = (etime >= 0) * (etime+1) ;				/* endless ramp function for treatment effect */
replace TE = 0 if E_i == . ;

gen treated_post = etime >= 0 ;

gen eps = sqrt(0.05) * rnormal() ;
gen actual = Y0_pure + TE * treated ;		
gen y = actual + eps ;						/* observed Y */

/* create variables used for estimation */
/* event time dummies */
forvalues i = 0/10 { ;
	gen D_p`i' = (etime == `i') ;
	gen D_m`i' = (etime == -1 * `i') ;
} ;
drop D_p10 ;
/*
/*	End points */
gen D_p4 = etime >= 4 ;
gen D_m4 = etime <= -4 ;
*/

end ;


runme ;


