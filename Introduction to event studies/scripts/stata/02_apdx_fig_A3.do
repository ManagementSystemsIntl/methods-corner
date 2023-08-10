#delimit ;

cap log close ;

set trace off ;
set more off ;
clear all ;
set seed 101 ;

cap prog drop runme ; 
prog def runme ;
args list ctrl trends ;

qui gendata "`list'" `ctrl' `trends' ;

qui forvalues i = 0/20 { ;
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

egen unittype = group(E_i) , miss ;
qui tab unittype , gen(udum) ;
qui tab t , gen(tdum) ;

unab vars : D_* udum* tdum* ;
mata: count() ;
local c1 = count[1,1] ;
local c2 = count[1,2] ;
local c3 = count[1,3] ;

di "`list'" _column(20) `c1' _column(25) `c2'  
	_column(35) `c3' ;

end ;

mata: ;

void count() 
{  ;
	v = st_local("vars") ;
	X = st_data(.,v) ;

	mycols = cols(X) ;
	myrank = rank(X) ;
	gap = mycols - myrank ;
	output = (mycols,myrank,gap) ;
	st_matrix("count",output) ;
} ;
end ;


cap prog drop gendata ;
program define gendata ;
args list ctrl trends ;

drop _all ;

local numobs = 48 ;
local treatedunittypes = wordcount("`list'") ;

/* "ctrl" is 0/1 whether we have control (untreated) units */
/* if we have them, put half of data into control units */
local numtreatedobs = round(`numobs' / (1 + `ctrl')) ;
local numuntreatedobs = `numobs' - `numtreatedobs' ;


local obspertype = floor(`numtreatedobs' /
	`treatedunittypes') ;
forvalues i = 1/`treatedunittypes' { ;
	local eventdate_`i' = word("`list'",`i') ;
} ;



set obs `numobs' ;							/* number of units */
gen i = _n ;


gen treated = i > `numuntreatedobs' ;		/* treated units */

gen E_i = . ;
forvalues i = 1/`treatedunittypes' { ;
	local start = (`i'-1)*`obspertype' + `numuntreatedobs' + 1 ;
	local stop = (`i')*`obspertype' + `numuntreatedobs' ;
	replace E_i = `eventdate_`i'' if i >= `start' & i <= `stop' ;
} ;
replace E_i = `eventdate_`treatedunittypes'' if i > `stop' ;
tab E_i , miss ;

expand 20 ;							/* number of time periods */

sort i ;
qui by i: gen t = _n ;

xtset i t ;

/* make variables that determine the DGP */
gen etime = (t - E_i) ;						/* event time */

/* gen TE = 1 * (etime >= 0) ;					/* step function treatment effect */
*/
gen TE = (etime >= 0) * (etime+1) ;				/* endless ramp function for treatment effect */
replace TE = 0 if E_i == . ;

gen Y0_pure = 0 ;										/* simplest counterfactual */
*gen Y0_pure = 4 * treated +  0.3 * treated * t ;		/* treated have a pre-trend ... */
*gen Y0_pure = 4 * treated +  0.1 * treated * (t-10) * (E_i - 9);	/* pre-trend based on E_i ... */

gen eps = sqrt(0.2) * rnormal() ;
gen actual = Y0_pure + TE * treated ;		
gen y = actual + eps ;						/* observed Y */


end ;


cap prog drop bigone ;
prog def bigone ;

runme "10 11" 0 0 ;

forvalues i = 4/16 { ;
	runme "4 16 `i' " 0 0 ;
	local rest_m3_`i' = count[1,3] - 2 ;
	runme "4 16 `i' 10" 0 0 ;
	local rest_m4_`i' = count[1,3] - 2 ;
} ;

drop _all ;
set obs 7 ;
gen E_i = _n + 3 ;
qui gen units3 = . ;
qui gen units4 = . ;
qui forvalues i = 4/10 { ;
	replace units3 = `rest_m3_`i'' if E_i == `i' ;
	replace units4 = `rest_m4_`i'' if E_i == `i' ;
} ;

gen E4_i = E_i + 0.15 ;
graph twoway (scatter units3 E_i , msymbol(o) msize(medsmall)) 
	(scatter units4 E4_i , msymbol(triangle) msize(medsmall)) 
	,
	ti("Extra parameter restrictions required") 
	xtitle("Location of E_3") 
	note("# restrictions needed beyond DiD restrictions. No control units. T=20. E_1 = 4, E_2 = 16."
	"Circles show 3-unit-types, as fn of event date of 3rd type. Triangles show 4-unit-types, with E_4 = 10.") 
	;
graph export figures/apdx_fig_A03.png , replace ;
	
	
end ;



bigone ;


