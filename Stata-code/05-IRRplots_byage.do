capture program drop plotsagegrp
program plotsagegrp
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
	
	local syear="`1'"
	local eyear="`2'"
	local midagelst="0.5 3 7.5 12.5 17.5 22.5 27.5 32.5 37.5 42.5 47.5 52.5 57.5 62.5 67.5 72.5 77.5 82.5 87.5"
	matrix mat1 = J(19,2,.) 
	matrix colnames mat1 = IRRpeople SEpeople
	matrix mat2 = J(19,2,.) 
	matrix colnames mat2 = IRRmale SEmale
	matrix mat3 = J(19,2,.) 
	matrix colnames mat3 = IRRfemale SEfemale

	//get data
	forvalues y=1(1)`=wordcount("`midagelst'")' {
		local lmage = word("`midagelst'",`y')
		//both sexes
		qui poisson deaths north if midage==`lmage' & year>=`syear' & year <=`eyear', exposure(population)
		matrix mat1[`y',1]=exp(_b[north])
		matrix mat1[`y',2]=exp(_b[north])*_se[north]
		//male
		qui poisson deaths north if midage==`lmage' & male==1 & year>=`syear' & year <=`eyear', exposure(population)
		matrix mat2[`y',1]=exp(_b[north])
		matrix mat2[`y',2]=exp(_b[north])*_se[north]
		//female
		qui poisson deaths north if midage==`lmage' & male==0 & year>=`syear' & year <=`eyear', exposure(population)
		matrix mat3[`y',1]=exp(_b[north])
		matrix mat3[`y',2]=exp(_b[north])*_se[north]		
	}
	clear
	qui svmat mat1, names(col)
	qui svmat mat2, names(col)
	qui svmat mat3, names(col)
	qui egen agecat=seq()

	//raw plots with CI	
	//overall
	gen l95CIpeople=IRRpeople-1.96*SEpeople
	gen u95CIpeople=IRRpeople+1.96*SEpeople
	graph twoway rarea l95CIpeople u95CIpeople agecat, color(gs14) || line IRRpeople agecat, lcolor(green) /*
	*/ legend(order(2 "People(`syear'-`eyear')" 1 "95% CI")) /*
	*/ ytitle("IRR") /*
	*/ xtitle("age group") /*
	*/ ylabel(0.6(0.3)1.5, format(%3.1f) labsize(2.5) angle(horizontal)) /*
	*/ xlabel(1 "<1" 2 "1-4" 3 "5-9" 4 "10-14" 5 "15-19" 6 "20-24" 7 "25-29" 8 "30-34" 9 "35-39" 10 "40-44" /*
	*/ 11 "45-49" 12 "50-54" 13 "55-59" 14 "60-64" 15 "65-69" 16 "70-74" 17 "75-79" 18 "80-84" 19 "85+", labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4)
	*graph save "`dir4'/0-IRR_people_`syear'-`eyear'.gph", replace
	graph export "`dir4'/0-IRR_people_`syear'-`eyear'.png", width(1200) replace
	window manage close graph
	//gender split
	gen l95CImale=IRRmale-1.96*SEmale
	gen u95CImale=IRRmale+1.96*SEmale
	gen l95CIfemale=IRRfemale-1.96*SEfemale
	gen u95CIfemale=IRRfemale+1.96*SEfemale	
	graph twoway rarea l95CImale u95CImale agecat, color(gs14) || rarea l95CIfemale u95CIfemale agecat, color(gs14) /*
	*/ || line IRRmale agecat, lcolor(blue) || line IRRfemale agecat, lcolor(maroon) /*
	*/ legend(order(3 "Males(`syear'-`eyear')" 4 "Females(`syear'-`eyear')" 1 "95% CI")) /*
	*/ ytitle("IRR") /*
	*/ xtitle("age group") /*	
	*/ ylabel(0.6(0.3)1.5, format(%3.1f) labsize(2.5) angle(horizontal)) /*
	*/ xlabel(1 "<1" 2 "1-4" 3 "5-9" 4 "10-14" 5 "15-19" 6 "20-24" 7 "25-29" 8 "30-34" 9 "35-39" 10 "40-44" /*
	*/ 11 "45-49" 12 "50-54" 13 "55-59" 14 "60-64" 15 "65-69" 16 "70-74" 17 "75-79" 18 "80-84" 19 "85+", labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4)
	*graph save "`dir4'/0-IRR_bygender_`syear'-`eyear'.gph", replace
	graph export "`dir4'/0-IRR_bygender_`syear'-`eyear'.png", width(1200) replace
	window manage close graph
	qui export excel "`dir5'graphexport.xlsx", firstrow(variables) sheet("IRR_byage_`syear'-`eyear'") sheetreplace
end

//Figure 2 in main paper: 4 time periods across ages
capture program drop plotsagegrp2
program plotsagegrp2
	local dir1 "C:/Users/`c(username)'/The University of Manchester Dropbox/Evangelos Kontopantelis/zP_drive/Evan/Informatics/North-South divide/2025_all-causes/"
	local dir2 "`dir1'/data/"
	local dir3 "`dir1'/shared/"
	local dir4 "`dir3'/graphs/"
	capture mkdir `dir3'
	capture mkdir `dir4'
	cd "`dir1'"
	use "`dir2'All_cause_dataset.dta", clear
	set more off
	
	local prefx = "people"
	if "`1'"!="" {
		local prefx = "`1'"
		if "`prefx'" == "male" {
			keep if male==1
		}
		if "`prefx'" == "female" {
			keep if male==0
		}		
	}
	
	set more off
	local syear1=1981
	local eyear1=1989
	local syear2=1990
	local eyear2=1999
	local syear3=2000
	local eyear3=2009
	local syear4=2010
	local eyear4=2019
	local syear5=2020
	local eyear5=2024	
	local midagelst="0.5 3 7.5 12.5 17.5 22.5 27.5 32.5 37.5 42.5 47.5 52.5 57.5 62.5 67.5 72.5 77.5 82.5 87.5"
	matrix mat1 = J(19,10,.) 
	matrix colnames mat1 = IRRpeople1 IRRpeople2 IRRpeople3 IRRpeople4 IRRpeople5 SEpeople1 SEpeople2 SEpeople3 SEpeople4 SEpeople5

	//get data
	forvalues i=1(1)5 {
		forvalues y=1(1)`=wordcount("`midagelst'")' {
			local lmage = word("`midagelst'",`y')
			//both sexes
			qui poisson deaths north if midage==`lmage' & year>=`syear`i'' & year <=`eyear`i'', exposure(population)
			matrix mat1[`y',`i']=exp(_b[north])
			matrix mat1[`y',`=5+`i'']=exp(_b[north])*_se[north]
		}
	}
	clear
	qui svmat mat1, names(col)
	qui egen agecat=seq()

	//raw plots with CI	
	//overall
	forvalues i=1(1)5 {
		gen l95CIpeople`i'=IRRpeople`i'-1.96*SEpeople`i'
		gen u95CIpeople`i'=IRRpeople`i'+1.96*SEpeople`i'
		replace l95CIpeople`i'=(l95CIpeople`i'-1)*100
		replace u95CIpeople`i'=(u95CIpeople`i'-1)*100
		replace IRRpeople`i'=(IRRpeople`i'-1)*100
	}	
	graph twoway rarea l95CIpeople1 u95CIpeople1 agecat, color(gs14) /*
	*/ || rarea l95CIpeople2 u95CIpeople2 agecat, color(gs14) /*
	*/ || rarea l95CIpeople3 u95CIpeople3 agecat, color(gs14) /*
	*/ || rarea l95CIpeople4 u95CIpeople4 agecat, color(gs14) /*
	*/ || rarea l95CIpeople5 u95CIpeople5 agecat, color(gs14) /*	
	*/ || line IRRpeople1 agecat, lcolor(blue) /*
	*/ || line IRRpeople2 agecat, lcolor(red) /*
	*/ || line IRRpeople3 agecat, lcolor(green) /*
	*/ || line IRRpeople4 agecat, lcolor(purple) /*
	*/ || line IRRpeople5 agecat, lcolor(dkorange) /*	
	*/ legend(rows(1) size(2.5)) /*
	*/ legend(pos(6)) /*
	*/ legend(order(6 "`syear1'-`eyear1'" 7 "`syear2'-`eyear2'" 8 "`syear3'-`eyear3'" 9 "`syear4'-`eyear4'" 10 "`syear5'-`eyear5'" 1 "95% CI")) /*
	*/ ytitle("% `prefx' northern excess mortality") /*
	*/ xtitle("age group") /*
	*/ ylabel(-10(10)50, format(%3.0f) labsize(2.5) angle(horizontal)) /*
	*/ xlabel(1 "<1" 2 "1-4" 3 "5-9" 4 "10-14" 5 "15-19" 6 "20-24" 7 "25-29" 8 "30-34" 9 "35-39" 10 "40-44" /*
	*/ 11 "45-49" 12 "50-54" 13 "55-59" 14 "60-64" 15 "65-69" 16 "70-74" 17 "75-79" 18 "80-84" 19 "85+", labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4)
	*graph save "`dir4'/0-IRR_people_5timeperiods.gph", replace
	graph export "`dir4'/0-IRR_`prefx'_5timeperiods.png", width(1200) replace
	*window manage close graph
end

