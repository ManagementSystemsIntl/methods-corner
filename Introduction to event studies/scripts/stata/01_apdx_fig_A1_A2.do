#delimit;
clear all;
set seed 101 ;
set more off;

/* NxT DiD data structure */
set obs 100 ;								/* number of units */
gen i = _n ;
gen treated = i > 50 ;						/* half of units treated */
gen E_i = 11 if treated == 1 ;
expand 20 ;									/* number of time periods */

label var E_i "Event Date";
sort i ;
qui by i: gen t = _n ;
xtset i t ;

graph set window fontface "Helvetica";


/* make variables that determine the DGP */
gen D = (t == E_i) ; 						/* the event "pulse" */
gen etime = (t - E_i) ;						/* event time */



/*Graph 4*/
#delimit;
replace E_i=16 if treated==0;

replace E_i=5 if treated==1 & i>=51 & i<=61;
replace E_i=6 if treated==1 & i>=62 & i<=68;
replace E_i=7 if treated==1 & i>=69 & i<=70;
replace E_i=10 if treated==1 & i>=71 & i<=75;
replace E_i=11 if treated==1 & i>=76 & i<=90;
replace E_i=12 if treated==1 & i>=91;

 
twoway  (histogram E_i if treated==1, discrete frequency start(4) barwidth(0.8) xscale(range(0 17)) yscale(range(0 1200))   color(cranberry%100)) 
        (histogram E_i if treated==0, discrete frequency start(4) barwidth(0.8) xscale(range(0 17)) yscale(range(0 1200))   color(navy%40)),  
	    title("Distribution of Treated and Never Treated Units by Event Date", 
	    color(black) size(v.small)) legend(off)
        xlabel(  5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11" 12 "12" 16 "NA")
		text(1100 15.5 "never treated", size(small))
		name(histogram4a , replace)
	    ;	


 /*CDF 4*/		
preserve;
sort E_i;
cumul E_i, gen(cumul);
keep if cumul <0.50;
keep cumul E_i;
#delimit;
forvalues i=1/4{;
set obs `=_N+1' ;
replace E_i= `i' if E_i==.;
replace cumul=0 if cumul==.;
};
forvalues i=13/15{;
set obs `=_N+1' ;
replace E_i= `i' if E_i==.;
replace cumul=0.5 if cumul==.;
};

sort E_i cumul;
twoway (line cumul E_i, sort),  
	    title("Cumulative Distribution of Units by Event Date",
		color(black) size(v.small)) legend(off)
		ytitle(CDF)
		yscale(range(0 1)) ylabel(0(0.1)1)
        xlabel( 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11" 12 "12" 13 "13" 14 "14" 15 "15")
		name(CDF4, replace)
	    ;
restore;

/*Graph 5*/
#delimit;
replace treated=1;

replace E_i=5 if treated==1 & i>=1 & i<=20;
replace E_i=6 if treated==1 & i>=21 & i<=32;
replace E_i=7 if treated==1 & i>=33 & i<=37;
replace E_i=10 if treated==1 & i>=38 & i<=58;
replace E_i=11 if treated==1 & i>=59 & i<=64;
replace E_i=12 if treated==1 & i>=65 & i<=90;
replace E_i=15 if treated==1 & i>90;

twoway  (histogram E_i if treated==1, discrete frequency start(4) barwidth(0.8) xscale(range(0 17)) yscale(range(0 800))   color(cranberry%100)),  
	    title("Distribution of Treated and Never Treated Units by Event Date", 
	    color(black) size(v.small)) legend(off)
        xlabel(  5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11" 12 "12" 13 "13" 14 "14" 15 "15" )
		ylabel(0(100)800)
		name(histogram5 , replace)
	    ;	

/*CDF 5*/	
#delimit;
preserve;
sort E_i;
cumul E_i, gen(cumul);
keep cumul E_i;
#delimit;
forvalues i=1/4{;
set obs `=_N+1' ;
replace E_i= `i' if E_i==.;
replace cumul=0 if cumul==.;
};
forvalues i=15/16{;
set obs `=_N+1' ;
replace E_i= `i' if E_i==.;
replace cumul=1 if cumul==.;
};
#delimit;
sort E_i cumul;
twoway (line cumul E_i, sort),  
	    title("Cumulative Distribution of Units by Event Date",
		color(black) size(v.small)) legend(off)
		ytitle(CDF)
		yscale(range(0 1)) ylabel(0(0.1)1)
        xlabel( 1 "1" 2 "2" 3 "3" 4 "4" 5 "5" 6 "6" 7 "7" 8 "8" 9 "9" 10 "10" 11 "11" 12 "12" 13 "13" 14 "14" 15 "15" 16 "16")
		name(CDF5, replace)
	    ;
restore;




/*taking graphs together*/
graph combine histogram5 CDF5 ,
iscale(0.6)
note("Probability and cumulative distribution functions") ;
graph export figures/apdx_fig_A01.png , replace ;

graph combine histogram4a CDF4 ,
iscale(0.6)
note("Probability and cumulative distribution functions") ;
graph export figures/apdx_fig_A02.png , replace ;

