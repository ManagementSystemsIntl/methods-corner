********************************************************************************	
	* 	Gustavo Castillo
	*	This do file: 
		* Cleans data to prepare it to make graphs; and
		* Makes graphs
	*	May 11, 2021						
********************************************************************************

******* Step-by-step guide to making graphs on Stata ***********

*1. Set working DIRECTORIES 	
global dirfolder "C:\Users\gustavo.castillo\Desktop\Arab Spring STATA\" 

*2. IMPORT data
use "$dirfolder\wgidataset-fixed.dta", clear

*3. Cleaning data - CREATE variables
encode countryname, generate(countryname1) label(countryname)
order countryname1, after (countryname)

*3.1 Cleaning data - DROP unnecessary variables
keep if countryname1==196 | countryname1==110 | countryname1==58
keep if year>2007

* Tunisia = 196 | Libya = 110 | Egypt = 58

*Now that the data is cleaned, we are ready to make the Voice and Accountability graph

*1. Establish the basic plot

graph twoway (scatter vae year if countryname1==196) (scatter vae year if countryname1==110) (scatter vae year if countryname1==58) 

*2. Connect coordinates and include color
graph twoway (scatter vae year if countryname1==196, connect(direct) color(yellow)) (scatter vae year if countryname1==110, connect(direct) color(green)) (scatter vae year if countryname1==58, connect(direct) color(purple))

*3. Add vertical line to set a break point
graph twoway (scatter vae year if countryname1==196, connect(direct) color(yellow)) (scatter vae year if countryname1==110, connect(direct) color(green)) (scatter vae year if countryname1==58, connect(direct) color(purple) tline(2010, lc(gray)) tlabel(2010 `""Arab" "Spring""', add labsize(*.75)))

*4. Sequence x-axis and label values 
graph twoway (scatter vae year if countryname1==196, connect(direct) color(yellow)) (scatter vae year if countryname1==110, connect(direct) color(green) xtitle("") xlabel(2008(2)2020,grid)) (scatter vae year if countryname1==58, connect(direct) color(purple) tline(2010, lc(gray)) tlabel(2010 `""Arab" "Spring""', add labsize(*.75)))

*5. Orient and label y-axis, establish a grid
graph twoway (scatter vae year if countryname1==196, connect(direct) color(yellow)) (scatter vae year if countryname1==110, connect(direct) color(green) ytitle("Index" "Score", orient(horizontal)) ylabel(,angle(0)) xtitle("") xlabel(2008(2)2020,grid) ylabel(,grid)) (scatter vae year if countryname1==58, connect(direct) color(purple) tline(2010, lc(gray)) tlabel(2010 `""Arab" "Spring""', add labsize(*.75)))

*6. Create a legend
graph twoway (scatter vae year if countryname1==196, connect(direct) color(yellow)) (scatter vae year if countryname1==110, connect(direct) color(green) ytitle("Index" "Score", orient(horizontal)) ylabel(,angle(0)) xtitle("") xlabel(2008(2)2020,grid) ylabel(,grid) text(0.4 2019.5 "Tunisia") text(-1.55 2019.5 "Libya") text(-1.30 2019.5 "Egypt")) (scatter vae year if countryname1==58, connect(direct) color(purple) tline(2010, lc(gray)) tlabel(2010 `""Arab" "Spring""', add labsize(*.75)) legend(label(1 "Tunisia") label(2 "Libya") label(3 "Egypt") col(3)))

* Add a title - FINAL Version
graph twoway (scatter vae year if countryname1==196, connect(direct) color(yellow)) (scatter vae year if countryname1==110, connect(direct) color(green) title("Voice and Accountability") ytitle("Index" "Score", orient(horizontal)) ylabel(,angle(0)) xtitle("") xlabel(2008(2)2020,grid) ylabel(,grid) text(0.4 2019.5 "Tunisia") text(-1.55 2019.5 "Libya") text(-1.30 2019.5 "Egypt")) (scatter vae year if countryname1==58, connect(direct) color(purple) tline(2010, lc(gray)) tlabel(2010 `""Arab" "Spring""', add labsize(*.75)) legend(label(1 "Tunisia") label(2 "Libya") label(3 "Egypt") col(3)))

* EXPORT graph
graph export VoiceandAccountability.png