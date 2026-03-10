capture program drop plotdescr
program plotdescr
	local dir1 "C:/Users/`c(username)'/The University of Manchester Dropbox/Evangelos Kontopantelis/zP_drive/Evan/Informatics/North-South divide/2025_all-causes/"
	local dir2 "`dir1'/data/"
	local dir3 "`dir1'/shared/"
	local dir4 "`dir3'/graphs/"
	local dir5 "`dir3'/export/"
	capture mkdir "`dir3'"
	capture mkdir "`dir4'"
	capture mkdir "`dir5'"
	cd "`dir1'"
	use "`dir2'All_cause_dataset.dta", clear
	set more off
	preserve
	
	//calculate descriptives
	tempfile tempf
	//total population and mean age
	qui gen meanage=midage*population
	qui collapse (sum) population meanage, by(north year)
	replace meanage=meanage/population
	label var population "Total population"
	label var meanage "Mean age"
	qui save `tempf', replace	
	restore, preserve
	//%females
	qui keep if male==0
	qui collapse (sum) population, by(north year)
	qui rename population femperc
	label var femperc "% Females"
	qui merge 1:1 north year using `tempf'
	qui drop _merge
	qui replace femperc=femperc/population*100
	qui save `tempf', replace	
	restore, preserve
	//% age groups
	qui keep if agecat>=6 & agecat<=10
	qui collapse (sum) population, by(north year)
	qui rename population popperc1
	label var popperc1 "% Aged 25 to 49"
	qui merge 1:1 north year using `tempf'
	qui drop _merge
	qui replace popperc1=popperc1/population*100
	qui save `tempf', replace	
	restore, preserve
	qui keep if agecat>=11 & agecat<=14
	qui collapse (sum) population, by(north year)
	qui rename population popperc2
	label var popperc2 "% Aged 50 to 69"
	qui merge 1:1 north year using `tempf'
	qui drop _merge
	qui replace popperc2=popperc2/population*100
	qui save `tempf', replace	
	restore, preserve
	qui keep if agecat>=15 & agecat<=18
	qui collapse (sum) population, by(north year)
	qui rename population popperc3
	label var popperc3 "% Aged 70 or over"
	qui merge 1:1 north year using `tempf'
	qui drop _merge
	qui replace popperc3=popperc3/population*100
	qui save `tempf', replace	
	restore, not
	
	//population size
	graph twoway line population year if north==0, lcolor(blue) /*
	*/ || line population year if north==1, lcolor(maroon) /*
	*/ legend(order(1 "South" 2 "North")) /*
	*/ ytitle("Population size") /*
	*/ ylabel(, format(%7.0f) labsize(2.5) angle(horizontal)) /*
	*/ xtitle("Year") /*
	*/ xlabel(1981 1985 1990 1995 2000 2005 2010 2015 2020 2024, labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4)
	*graph save "`dir4'/2-descr_population.gph", replace
	graph export "`dir4'/2-descr_population.png", width(1200) replace
	window manage close graph

	//percentage females
	graph twoway line femperc year if north==0, lcolor(blue) /*
	*/ || line femperc year if north==1, lcolor(maroon) /*
	*/ legend(order(1 "South" 2 "North")) /*
	*/ ytitle("% Females") /*
	*/ ylabel(, format(%4.1f) labsize(2.5) angle(horizontal)) /*
	*/ xtitle("Year") /*
	*/ xlabel(1981 1985 1990 1995 2000 2005 2010 2015 2020 2024, labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4)
	*graph save "`dir4'/2-descr_percfem.gph", replace
	graph export "`dir4'/2-descr_percfem.png", width(1200) replace
	window manage close graph

	//mean age
	graph twoway line meanage year if north==0, lcolor(blue) /*
	*/ || line meanage year if north==1, lcolor(maroon) /*
	*/ legend(order(1 "South" 2 "North")) /*
	*/ ytitle("Mean age") /*
	*/ ylabel(, format(%4.1f) labsize(2.5) angle(horizontal)) /*
	*/ xtitle("Year") /*
	*/ xlabel(1981 1985 1990 1995 2000 2005 2010 2015 2020 2024, labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4)
	*graph save "`dir4'/2-descr_meanage.gph", replace
	graph export "`dir4'/2-descr_meanage.png", width(1200) replace
	window manage close graph
	
	//percentage ages
	graph twoway line popperc1 year if north==0, lcolor(blue) /*
	*/ || line popperc1 year if north==1, lcolor(maroon)  /*
	*/ || line popperc2 year if north==0, lcolor(blue) lpattern(dash) /*
	*/ || line popperc2 year if north==1, lcolor(maroon) lpattern(dash) /*
	*/ || line popperc3 year if north==0, lcolor(blue) lpattern(dot) /*
	*/ || line popperc3 year if north==1, lcolor(maroon) lpattern(dot) /*	
	*/ legend(order(1 "South, aged 25-49" 2 "North, aged 25-49" 3 "South, aged 50-69" 4 "North, aged 50-69" 5 "South, aged 70+" 6 "North, aged 70+")) /*
	*/ ytitle("%") /*
	*/ ylabel(, format(%3.0f) labsize(2.5) angle(horizontal)) /*
	*/ xtitle("Year") /*
	*/ xlabel(1981 1985 1990 1995 2000 2005 2010 2015 2020 2024, labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4)
	*graph save "`dir4'/2-descr_percage.gph", replace
	graph export "`dir4'/2-descr_percage.png", width(1200) replace
	window manage close graph	
	
end
		
		