set more off
tempfile tempf
local dir1 "C:/Users/`c(username)'/The University of Manchester Dropbox/Evangelos Kontopantelis/zP_drive/Evan/Informatics/North-South divide/2025_all-causes/"
local dir2 "`dir1'/data/"
local dir3 "`dir1'/shared/"
local dir4 "`dir3'/past_paper/"
local dir5 "`dir3'/export/"
capture mkdir "`dir3'"
capture mkdir "`dir4'"
capture mkdir "`dir5'"
cd "`dir1'"

local midagelst="0.5 3 7.5 12.5 17.5 22.5 27.5 32.5 37.5 42.5 47.5 52.5 57.5 62.5 67.5 72.5 77.5 82.5 87.5"
matrix mat1 = J(19,2,.) 
matrix colnames mat1 = IRRpeople SEpeople
matrix mat2 = J(19,2,.) 
matrix colnames mat2 = IRRmale SEmale
matrix mat3 = J(19,2,.) 
matrix colnames mat3 = IRRfemale SEfemale
//get data
forvalues yr=1981(1)2024 {
	qui use "`dir2'All_cause_dataset.dta",clear
	forvalues y=1(1)`=wordcount("`midagelst'")' {
		local lmage = word("`midagelst'",`y')
		//both sexes
		qui poisson deaths north if midage==`lmage' & year==`yr', exposure(population)
		matrix mat1[`y',1]=exp(_b[north])
		matrix mat1[`y',2]=exp(_b[north])*_se[north]
		//male
		qui poisson deaths north if midage==`lmage' & male==1 & year==`yr', exposure(population)
		matrix mat2[`y',1]=exp(_b[north])
		matrix mat2[`y',2]=exp(_b[north])*_se[north]
		//female
		qui poisson deaths north if midage==`lmage' & male==0 & year==`yr', exposure(population)
		matrix mat3[`y',1]=exp(_b[north])
		matrix mat3[`y',2]=exp(_b[north])*_se[north]		
	}
	drop _all
	qui svmat mat1, names(col)
	qui svmat mat2, names(col)
	qui svmat mat3, names(col)
	qui egen agecat=seq()
	qui gen year=`yr'
	capture save `tempf'
	if _rc!=0 {
		append using `tempf'
		qui save `tempf', replace
	}
}
qui use `tempf', clear

//editing and labelling
label var agecat "Age category"
label define agelbl 1 "<1" 2 "01-04" 3 "05-09" 4 "10-14" 5 "15-19" 6 "20-24" 7 "25-29" /*
*/ 8 "30-34" 9 "35-39" 10 "40-44" 11 "45-49" 12 "50-54" 13 "55-59" 14 "60-64" 15 "65-69" /*
*/ 16 "70-74" 17 "75-79" 18 "80-84" 19 "85+"
label val agecat agelbl
foreach x in IRRpeople IRRmale IRRfemale {
	replace `x'=100*(`x'-1)
}
label var IRRpeople "% all excess mortality in the North"
label var IRRmale "% male excess mortality in the North"
label var IRRfemale "% female excess mortality in the North"

//all people
replace IRRpeople = -20 if IRRpeople < -20
replace IRRpeople = 80 if IRRpeople > 80
twoway contour IRRpeople agecat year, zlabel(-20(10)80) format(%5.0f) ///
	ccuts(-20(10)80) ///
	ccolors(navy blue cyan green lime yellow gold orange red cranberry maroon ) ///
	interp(none) ///
	xscale(range(1981 2024)) xlabel(1981 1985(5)2020 2024,angle(45) labsize(3)) xtitle("Year", size(medlarge)) ///
	yscale(range(1 19)) ylabel(1(1)19,valuelabel angle(0) labsize(2.4)) ytitle("Age category", size(medlarge)) ///
	|| function y=int((x-1981)/5)+3, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+5, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+7, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+9, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+11, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+13, range(1981 2015) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+15, range(1981 2005) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+17, range(1981 1995) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+19, range(1981 1985) color(black) lstyle(foreground) clwidth(*0.5) ///		
	|| function y=int((x-1981)/5)+1, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)-1, range(1991 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)-3, range(2001 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)-5, range(2011 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)-7, range(2021 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	, legend(off)
*graph save "`dir4'16-contour_all.gph", replace
graph export "`dir4'16-contour_all.png", width(1200) replace
graph export "`dir4'16-contour_all.eps", replace


//male
replace IRRmale = -20 if IRRmale < -20
replace IRRmale = 80 if IRRmale > 80
twoway contour IRRmale agecat year, zlabel(-20(10)80) format(%5.0f)  ///
	ccuts(-20(10)80) ///
	ccolors(navy blue cyan green lime yellow gold orange red cranberry maroon ) ///
	interp(none) ///
	xscale(range(1981 2024)) xlabel(1981 1985(5)2020 2024,angle(45) labsize(3)) xtitle("Year", size(medlarge)) ///
	yscale(range(1 19)) ylabel(1(1)19,valuelabel angle(0) labsize(2.4)) ytitle("Age category", size(medlarge)) ///
	|| function y=int((x-1981)/5)+3, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+5, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+7, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+9, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+11, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+13, range(1981 2015) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+15, range(1981 2005) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+17, range(1981 1995) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+19, range(1981 1985) color(black) lstyle(foreground) clwidth(*0.5) ///		
	|| function y=int((x-1981)/5)+1, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)-1, range(1991 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)-3, range(2001 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)-5, range(2011 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)-7, range(2021 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	, legend(off)
*graph save "`dir4'16-contour_male.gph", replace
graph export "`dir4'16-contour_male.png", width(1200) replace
graph export "`dir4'16-contour_male.eps", replace

//female
replace IRRfemale = -20 if IRRfemale < -20
replace IRRfemale = 80 if IRRfemale > 80
twoway contour IRRfemale agecat year, zlabel(-20(10)80) format(%5.0f)  ///
	ccuts(-20(10)80) ///
	ccolors(navy blue cyan green lime yellow gold orange red cranberry maroon ) ///
	interp(none) ///
	xscale(range(1981 2024)) xlabel(1981 1985(5)2020 2024,angle(45) labsize(3)) xtitle("Year", size(medlarge)) ///
	yscale(range(1 19)) ylabel(1(1)19,valuelabel angle(0) labsize(2.4)) ytitle("Age category", size(medlarge)) ///
	|| function y=int((x-1981)/5)+3, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+5, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+7, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+9, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+11, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+13, range(1981 2015) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+15, range(1981 2005) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+17, range(1981 1995) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)+19, range(1981 1985) color(black) lstyle(foreground) clwidth(*0.5) ///		
	|| function y=int((x-1981)/5)+1, range(1981 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)-1, range(1991 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)-3, range(2001 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)-5, range(2011 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	|| function y=int((x-1981)/5)-7, range(2021 2024) color(black) lstyle(foreground) clwidth(*0.5) ///
	, legend(off)
*graph save "`dir4'16-contour_female.gph", replace
graph export "`dir4'16-contour_female.png", width(1200) replace
graph export "`dir4'16-contour_female.eps", replace
