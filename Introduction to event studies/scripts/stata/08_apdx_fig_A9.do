#delimit ;

cap log close ;

set trace off ;
set more off ;
clear ;
set seed 108 ;

cap prog drop runme ; 
prog def runme ;

#delimit;

gendata;
summ ; 

egen meany = mean(y) , by(t treated) ;

sort t ;	

/* 2, unit FE's among all average to zero */
constraint define 2 1.i + 2.i + 3.i + 4.i + 5.i + 
	6.i + 7.i + 8.i + 9.i + 10.i = 0 ;

/* 7 pre-treatment ES dummies average to zero */
constraint define 7 D_m10 + D_m9 + D_m8 + D_m7 + D_m6 + D_m5 + D_m4 + 
	D_m3 + D_m2 + D_m1 = 0 ; 

#delimit; 
/*9-18 for pooling #1, model 2*/
constraint define 9 D_m10=D_m9;
constraint define 10 D_m8=D_m7;
constraint define 11 D_m6=D_m5;
constraint define 12 D_m4=D_m3;
constraint define 13 D_m2=D_m1;
constraint define 14 D_p0=D_p1;
constraint define 15 D_p2=D_p3;
constraint define 16 D_p4=D_p5;
constraint define 17 D_p6=D_p7;
constraint define 18 D_p8=D_p9;

/*21-32 for pooling #2, model 3 */
#delimit;
constraint define 21 D_m1=D_m2;
constraint define 22 D_m1=D_m3;

constraint define 23 D_m4=D_m5;
constraint define 24 D_m4=D_m6;

constraint define 25 D_m7=D_m8;
constraint define 26 D_m7=D_m9;

constraint define 27 D_p0=D_p1;
constraint define 28 D_p2=D_p1;

constraint define 29 D_p3=D_p4;
constraint define 30 D_p3=D_p5;

constraint define 31 D_p6=D_p7;
constraint define 32 D_p6=D_p8;

/*41-54 Pooling multiple dummies case 2 , model 4*/
#delimit;
constraint define 41 D_m1=D_m2;
constraint define 42 D_m2=D_m3;
constraint define 43 D_m3=D_m4;

constraint define 44 D_m5=D_m6;
constraint define 45 D_m6=D_m7;
constraint define 46 D_m7=D_m8;

constraint define 47 D_m9=D_m10;

constraint define 48 D_p0=D_p1;
constraint define 49 D_p1=D_p2;
constraint define 50 D_p2=D_p3;

constraint define 51 D_p4=D_p5;
constraint define 52 D_p5=D_p6;
constraint define 53 D_p6=D_p7;

constraint define 54 D_p8=D_p9;



/* no trends. Normalized so that Dm1 = 0 */
#delimit;
cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 
	D_m1 D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i , nocons constraints(7 2)  collinear ;
matrix myb_ES1 = e(b) ;
#delimit;
mat V=e(V); /* place e(V) in V*/
mat myse_ES1=J(1,50,-9999); /* create empty matrix for standard errors*/
forval i=1/50 {;
mat myse_ES1[1,`i']=sqrt(V[`i',`i']); /* convert the variances into the se one at a time*/
};
matrix colnames myse_ES1= D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
                          1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20   
						  1 2 3 4 5 6 7 8 9 10 
						  ;

/* no trends. Normalized so that Dm1 = 0 and pairwise pooling of dummies */
#delimit;
cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 
	D_m1 D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i , nocons constraints(7 2 9-18)  collinear ;
matrix myb_ES2 = e(b) ;
#delimit;
mat V=e(V); /* place e(V) in V*/
mat myse_ES2=J(1,50,-9999); /* create empty matrix for standard errors*/
forval i=1/50 {;
mat myse_ES2[1,`i']=sqrt(V[`i',`i']); /* convert the variances into the se one at a time*/
};
matrix colnames myse_ES2= D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
                          1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20   
						  1 2 3 4 5 6 7 8 9 10 
						  ;

/* no trends. Normalized so that Dm1 = 0 and pooling of multiple dummies (four slope coefficients)*/
#delimit;
cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 
	D_m1 D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i , nocons constraints(7 2 21-32)  collinear ;
matrix myb_ES3 = e(b) ;
#delimit;
mat V=e(V); /* place e(V) in V*/
mat myse_ES3=J(1,50,-9999); /* create empty matrix for standard errors*/
forval i=1/50 {;
mat myse_ES3[1,`i']=sqrt(V[`i',`i']); /* convert the variances into the se one at a time*/
};
matrix colnames myse_ES3= D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
                          1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20   
						  1 2 3 4 5 6 7 8 9 10 
						  ;
/* no trends. Normalized so that Dm1 = 0 and pooling of multiple dummies (five slope coefficients)*/
#delimit;
cnsreg y D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 
	D_m1 D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
	ibn.t ibn.i , nocons constraints(7 2 41-54)  collinear ;
matrix myb_ES4 = e(b) ;
#delimit;
mat V=e(V); /* place e(V) in V*/
mat myse_ES4=J(1,50,-9999); /* create empty matrix for standard errors*/
forval i=1/50 {;
mat myse_ES4[1,`i']=sqrt(V[`i',`i']); /* convert the variances into the se one at a time*/
};
matrix colnames myse_ES4= D_m10 D_m9 D_m8 D_m7 D_m6 D_m5 D_m4 D_m3 D_m2 D_m1 D_p0 D_p1 D_p2 D_p3 D_p4 D_p5 D_p6 D_p7 D_p8 D_p9 
                          1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20   
						  1 2 3 4 5 6 7 8 9 10 
						  ;
						  


tempfile main ;
save `main' ;

/* now get the results ready to plot out */

*set trace on ;
#delimit;
tempfile pooled ;
forvalues i = 0/10 { ;
	drop _all ;
	set obs 2 ;
	gen label = "m`i'" in 1 ;
	replace label = "p`i'" in 2 ;
	gen etime = -1 * `i' in 1 ;
	replace etime = `i' in 2 ;

	foreach j in 1 2 3 4 5 6 7{ ;
		gen cf_m`j' = .	 ;	
		gen truth_m`j' = (0) in 1 ;	/* endless ramp function for treatment effect */
		replace truth_m`j' = (`i' + 1) in 2 ;	/* endless ramp function for treatment effect */
	
	
		local myb = 0 ;
		matrix define tmp = (0) ;
		cap matrix define tmp = myb_ES`j'[1,"D_m`i'"] ;
		local myb = tmp[1,1] ;
*		cap local myb = myb_ES`j'[1,"D_m`i'"] ;
		gen ES_b_m`j' = `myb' in 1 ;
	
	
		local myse=0;
		matrix define tmp= (0);
		cap matrix define tmp = myse_ES`j'[1,"D_m`i'"] ;
        local myse = tmp[1,1] ;
		gen ES_se_m`j' = `myse' in 1 ;
		
		
		local myb = 0 ;
		matrix define tmp = (0) ;
		cap matrix define tmp = myb_ES`j'[1,"D_p`i'"] ;
		local myb = tmp[1,1] ;
*		cap local myb = myb_ES`j'[1,"D_p`i'"] ;
		replace ES_b_m`j' = `myb' in 2 ;
		
		local myse=0;
		matrix define tmp= (0);
		cap matrix define tmp = myse_ES`j'[1,"D_p`i'"] ;
        local myse = tmp[1,1] ;
		replace ES_se_m`j' = `myse' in 2 ;

	} ;
	

	capture append using `pooled' ;
	
	save `pooled' , replace ;
} ;


drop if label == "m0" ;
drop if label == "p10" ;
sort etime ;

foreach i in 1 2 3 4 5 6 7{;
	*replace truth_m`i'=6 if truth_m`i'>6; /* activate for ramp then plateau*/
	*replace truth_m`i'=1 if etime>=0; /*activate for step function*/
    replace truth_m`i'= (0.7* truth_m`i'[_n-1]) if etime>=1; /*activate for AR treatment effect*/
};

save `pooled' , replace ;

list ;

local note1 "Standard ES" ;
local note2 "Coefficients pooled in groups of 2" ;
local note3 "Coefficients pooled in groups of 3" ;
local note4 "Coefficients pooled in groups of 4" ;


/*Compute 95 CI for all models*/
#delimit;
forvalues i = 1/7 { ;
gen low_b`i'= ES_b_m`i' - ((invttail(199, 0.025))*ES_se_m`i');
gen high_b`i'= ES_b_m`i' + ((invttail(199, 0.025))*ES_se_m`i');
};

/*Standard ES model with different pooling periods and confidence intervals*/
#delimit;
forvalues i=1/4 {;
graph twoway (connected ES_b_m`i' etime if etime<=-1, sort lcolor(navy%100) mcolor(navy%100) 
				msize(medsmall medium)  msymbol(o) lpattern(solid) ) 
             (connected ES_b_m`i' etime if etime>=0, sort lcolor(navy%100) mcolor(navy%100) 
				msize(medsmall medium)  msymbol(o) lpattern(solid) ) 
             (connected truth_m`i' etime, sort lcolor(maroon%100) mcolor(maroon%100) 
				msize(vsmall)  msymbol(o) lpattern(solid) ) 
			 (line high_b`i' etime, sort lcolor(navy%50) lpattern(dash)) 
			 (line low_b`i' etime, sort lcolor(navy%50) lpattern(dash))
		     (line cf_m`i' etime , lpattern(dash) )
		     , legend(off) xline(-0.5) yline(0)
			  yscale(range(-1 1.6))
		     note("`note`i''") 
		     name(g2`i' , replace)
		     ;
};
graph combine g21 g22 g23 g24 ,
note("Standard ES model with different pooling of time periods and CI") ;
graph export figures/apdx_fig_A09.png , replace ;


end ;
#delimit;
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
gen etime = (t - E_i) ;						/* event time */

/*
/* 1) Step function treatment effect*/
gen TE = 1 * (etime >= 0) ;	


/* 2) Endless ramp function for treatment effect */
gen TE  = (etime >= 0) * (etime+1) ;				
replace TE = 0 if E_i == . ;


/* 3) Ramp then plateau treatment effect*/
gen TE= (etime >= 0) * (etime+1) ;	
replace TE = 6 if etime >=5 ;			
replace TE= 0 if E_i == . ;
*/

/* 4) TE follows AR1*/
sort i etime;
gen TE= 1 * etime==0;
replace TE= (0.7* TE[_n-1]) if etime>=1;
replace TE = 0 if E_i == . ;

gen treated_post = etime >= 1 ;

gen Y0_pure = 0 ;							/* simplest counterfactual */
*gen Y0_pure = 4 * treated +  0.3 * treated * t ;			/* treated have a pre-trend ... */

gen eps = sqrt(.2) * rnormal(0, 1) ;
gen actual = Y0_pure + TE * treated ;		
gen y = actual + eps ;						/* observed Y */

/* create variables used for estimation */
/* event time dummies */
forvalues i = 0/10 { ;
	gen D_p`i' = (etime == `i') ;
	gen D_m`i' = (etime == -1 * `i') ;
} ;
drop D_p10 D_m0  ;
/*
/*	End points */
gen D_p4 = etime >= 4 ;
gen D_m4 = etime <= -4 ;
*/

end ;

runme ;



