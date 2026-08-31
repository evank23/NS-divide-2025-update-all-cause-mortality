capture program drop plotsmr
program plotsmr
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

	//restrict to under 75 (=1) or not (=0)?
	local restv="`1'"
	if "`restv'"=="" {
		local rstr="all"
		local scat=0
		local ecat=18
	}
	else if "`restv'"=="1" {
		local rstr="und75"
		local scat=0
		local ecat=15
		qui drop if midage>=75
	}
	else if "`restv'"=="2" {
		local rstr="25-44"
		local scat=6
		local ecat=9
		qui drop if midage<25 | midage>45
	}
	else if "`restv'"=="3" {
		local rstr="25-34"
		local scat=6
		local ecat=7
		qui drop if midage<25 | midage>35
	}
	else if "`restv'"=="4" {
		local rstr="35-44"
		local scat=8
		local ecat=9
		qui drop if midage<35 | midage>45
	}
	else if "`restv'"=="5" {
		local rstr="45-54"
		local scat=10
		local ecat=11
		qui drop if midage<45 | midage>55
	} 	
	else if "`restv'"=="6" {
		local rstr="50-74"		
		local scat=11
		local ecat=15
		qui drop if midage<50 | midage>75
	}
	else if "`restv'"=="7" {
		local rstr="25-49"		
		local scat=6
		local ecat=10
		qui drop if midage<25 | midage>50
	}	
	tempfile tempf
	qui save `tempf', replace
	
	//calculate SMRs (across England)
	qui use `tempf',clear
	drop ageband midage
	rename deaths deaths_
	rename population population_
	collapse (sum) deaths_ population_, by(agecat year male)
	reshape wide deaths@_ population@_, i(year male) j(agecat)
	reshape wide deaths* population*, i(year) j(male)
	//calculate mortality counts across all England, to relate to 100k deaths (Add up to 100k)
	scalar fden=0
	scalar mden=0
	forvalues i=`scat'(1)`ecat' {
		//numerators
		qui sum population`i'_0
		scalar num`i'f=r(sum)
		qui sum population`i'_1
		scalar num`i'm=r(sum)
		//denominators
		scalar fden = fden + num`i'f
		scalar mden = mden + num`i'm
	}
	forvalues i=`scat'(1)`ecat' {
		//not rounding this time
		scalar p`i'_0=(10000*num`i'f/fden)
		scalar p`i'_1=(10000*num`i'm/mden)	
	}	
	//calculate SMR into variable
	qui gen SMR=0
	foreach x in 0 1 {
		forvalues i=`scat'(1)`ecat' {
			qui replace SMR=SMR+((deaths`i'_`x'/population`i'_`x')*p`i'_`x')/2
		}
	}
	label var SMR "Standardised mortality rate, by agegroup and sex"
	qui egen allD=rowtotal(deaths*)		
	label var allD "Deaths"
	qui egen allpop=rowtotal(deaths*)		
	label var allpop "Population"	
    /*calculate CI for SMR*/
    qui gen SMRlo = SMR - invnormal(.975)*SMR/sqrt(allD)
	label var SMRlo "SMR lower 95% CI"
    qui gen SMRup = SMR + invnormal(.975)*SMR/sqrt(allD)	
	label var SMRup "SMR upper 95% CI"
	order year allD allpop SMR*
	qui compress
	save "`dir5'SMRdata_`rstr'1.dta", replace
	
	qui use `tempf',clear
	//calculate SMRs (north and south)
	drop ageband midage
	rename deaths deaths_
	rename population population_
	reshape wide deaths@_ population@_, i(year male north) j(agecat)
	reshape wide deaths* population*, i(year north) j(male)
	//calculate mortality counts across all England, to relate to 100k deaths (Add up to 100k)
	scalar fden=0
	scalar mden=0
	forvalues i=`scat'(1)`ecat' {
		//numerators
		qui sum population`i'_0
		scalar num`i'f=r(sum)
		qui sum population`i'_1
		scalar num`i'm=r(sum)
		//denominators
		scalar fden = fden + num`i'f
		scalar mden = mden + num`i'm
	}
	forvalues i=`scat'(1)`ecat' {
		//not rounding this time
		scalar p`i'_0=(10000*num`i'f/fden)
		scalar p`i'_1=(10000*num`i'm/mden)	
	}	
	//calculate SMR into variable
	qui gen SMR=0
	foreach x in 0 1 {
		forvalues i=`scat'(1)`ecat' {
			qui replace SMR=SMR+((deaths`i'_`x'/population`i'_`x')*p`i'_`x')/2
		}
	}
	label var SMR "Standardised mortality rate, by agegroup and sex"
	qui egen allD=rowtotal(deaths*)		
	label var allD "All deaths"
	qui egen allpop=rowtotal(deaths*)		
	label var allpop "Population"		
    /*calculate CI for SMR*/
    qui gen SMRlo = SMR - invnormal(.975)*SMR/sqrt(allD)
	label var SMRlo "SMR lower 95% CI"
    qui gen SMRup = SMR + invnormal(.975)*SMR/sqrt(allD)	
	label var SMRup "SMR upper 95% CI"
	order year north allD allpop SMR*
	qui compress
	save "`dir5'SMRdata_`rstr'2.dta", replace

	//to shade between xlines
	qui sum SMRup
	if `=round(ceil(r(max)),10)'>r(max) {
		local smax=round(ceil(r(max)),10)
	}
	else {
			local smax=round(ceil(r(max))+10,10)
	}
	qui sum SMRlo
	local smin=max(round(floor(r(min)),100),0) 
	gen c1=`smax' if (year>=1973 & year<=1975)
	gen c2=`smax' if (year>=1980 & year<=1981)
	gen c3=`smax' if (year>=1990 & year<=1991)	
	gen c4=`smax' if (year>=2008 & year<=2009)
	gen c5=`smax' if (year>=2020 & year<=2022)
	//SMR plots with CI	
	//overall
	graph twoway area c1 year, bcolor(sandb%30) || area c2 year, bcolor(sandb%30) /*
	*/ || area c3 year, bcolor(sandb%30) || area c4 year, bcolor(sandb%30) || area c5 year, bcolor(sandb%30) /*
	*/ || rarea SMRlo SMRup year if north==0, color(gs14) /*
	*/ || line SMR year if north==0, lcolor(blue) /*
	*/ || rarea SMRlo SMRup year if north==1, color(gs14) || line SMR year if north==1, lcolor(maroon) /*
	*/ legend(order(7 "South" 9 "North" 6 "95% CI" 1 "Stress periods")) /*
	*/ ytitle("Standardised mortality rate (per 10,000)") /*
	// yscale(range(`smin' `smax'))
	*/ ylabel(, format(%3.0f) labsize(2.5) angle(horizontal)) /*
	*/ xtitle("year") /*
	*/ xlabel(1981 1985 1990 1995 2000 2005 2010 2015 2020 2024, labsize(2.2) angle(45)) /*
	*/ plotregion(margin(tiny)) ylabel(,nogextend)
	*graph save "`dir4'/0-SMR_`rstr'.gph", replace
	graph export "`dir4'/0-SMR_`rstr'.png", width(1200) replace
	window manage close graph
end
