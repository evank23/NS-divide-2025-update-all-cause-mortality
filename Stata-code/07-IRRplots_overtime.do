capture program drop plotsyear
program plotsyear
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

	local sage="`1'"
	local eage="`2'"
	local midagelst="0.5 3 7.5 12.5 17.5 22.5 27.5 32.5 37.5 42.5 47.5 52.5 57.5 62.5 67.5 72.5 77.5 82.5 87.5"
	local srtagelst="0 1 5 10 15 20 25 30 35 40 45 50 55 60 65 70 75 80 85"
	local endagelst="1 4 9 14 19 24 29 34 39 44 49 54 59 64 69 74 79 84 90"
	matrix mat1 = J(44,2,.) 
	matrix colnames mat1 = IRRpeople SEpeople
	matrix mat2 = J(44,2,.) 
	matrix colnames mat2 = IRRmale SEmale
	matrix mat3 = J(44,2,.) 
	matrix colnames mat3 = IRRfemale SEfemale
	qui xi i.midage
	//start end of age groups and strings
	local strmidage=word("`midagelst'",`sage')
	local endmidage=word("`midagelst'",`eage')
	local lblstr= "aged " + word("`srtagelst'",`sage') + "-" + word("`endagelst'",`eage')
	if `sage'==1 {
		local lblstr= "aged " + word("`endagelst'",`eage') + " or under"
	}		
	if `sage'==1 & `eage'==1 {
		local lblstr="aged <1" 
	}	
	if `eage'==19 {
		local lblstr="aged " + word("`srtagelst'",`sage') + "+"
	}
	if `sage'==1 & `eage'==19 {
		local lblstr="all ages"
	}
	local savstr=word("`srtagelst'",`sage') + "_" + word("`endagelst'",`eage')
	
	//get data
	forvalues yr=1981(1)2024 {
		//both sexes (adjusted for any sex and age imbalance between north and south)
		qui poisson deaths north male _Imidage_2 _Imidage_3 _Imidage_4 _Imidage_5 _Imidage_6 _Imidage_7 _Imidage_8 /*
		*/ _Imidage_9 _Imidage_10 _Imidage_11 _Imidage_12 _Imidage_13 _Imidage_14 _Imidage_15 _Imidage_16 _Imidage_17 /*
		*/ _Imidage_18 _Imidage_19 if year==`yr' & midage>=`strmidage' & midage<=`endmidage', exposure(population)
		matrix mat1[`=`yr'-1980',1]=exp(_b[north])
		matrix mat1[`=`yr'-1980',2]=exp(_b[north])*_se[north]
		//male
		qui poisson deaths north male _Imidage_2 _Imidage_3 _Imidage_4 _Imidage_5 _Imidage_6 _Imidage_7 _Imidage_8 /*
		*/ _Imidage_9 _Imidage_10 _Imidage_11 _Imidage_12 _Imidage_13 _Imidage_14 _Imidage_15 _Imidage_16 _Imidage_17 /*
		*/ _Imidage_18 _Imidage_19 if year==`yr' & midage>=`strmidage' & midage<=`endmidage' & male==1, exposure(population)
		matrix mat2[`=`yr'-1980',1]=exp(_b[north])
		matrix mat2[`=`yr'-1980',2]=exp(_b[north])*_se[north]
		//female
		qui poisson deaths north male _Imidage_2 _Imidage_3 _Imidage_4 _Imidage_5 _Imidage_6 _Imidage_7 _Imidage_8 /*
		*/ _Imidage_9 _Imidage_10 _Imidage_11 _Imidage_12 _Imidage_13 _Imidage_14 _Imidage_15 _Imidage_16 _Imidage_17 /*
		*/ _Imidage_18 _Imidage_19 if year==`yr' & midage>=`strmidage' & midage<=`endmidage' & male==0, exposure(population)
		matrix mat3[`=`yr'-1980',1]=exp(_b[north])
		matrix mat3[`=`yr'-1980',2]=exp(_b[north])*_se[north]		
	}
	clear
	qui svmat mat1, names(col)
	qui svmat mat2, names(col)
	qui svmat mat3, names(col)
	qui egen year=seq()

	//raw plots with CI	
	//overall
	gen l95CIpeople=IRRpeople-1.96*SEpeople
	gen u95CIpeople=IRRpeople+1.96*SEpeople
	foreach x in IRRpeople l95CIpeople u95CIpeople {
		qui replace `x'=(`x'-1)*100
	}		
	graph twoway rarea l95CIpeople u95CIpeople year, color(gs14) || line IRRpeople year, lcolor(green) /*
	*/ legend(order(2 "People(`lblstr')" 1 "95% CI")) /*
	*/ ytitle("Percentage northern excess mortality") /*
	*/ xtitle("Year") /*
	*/ ylabel(, format(%3.0f) labsize(2.5) angle(horizontal)) /*
	*/ xlabel(1 "1981" 10 "1990" 15 "1995" 20 "2000" 25 "2005" 30 "2010" 35 "2015" 40 "2020" 44 "2024" /*
	*/ , labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4)
	*graph save "`dir4'/1-IRR_people_year_`savstr'.gph", replace
	graph export "`dir4'/1-IRR_people_year_`savstr'.png", width(1200) replace
	window manage close graph
	//gender split
	gen l95CImale=IRRmale-1.96*SEmale
	gen u95CImale=IRRmale+1.96*SEmale
	gen l95CIfemale=IRRfemale-1.96*SEfemale
	gen u95CIfemale=IRRfemale+1.96*SEfemale	
	graph twoway rarea l95CImale u95CImale year, color(gs14) || rarea l95CIfemale u95CIfemale year, color(gs14) /*
	*/ || line IRRmale year, lcolor(blue) || line IRRfemale year, lcolor(maroon) /*
	*/ legend(order(3 "Males(`lblstr')" 4 "Females(`lblstr')" 1 "95% CI")) /*
	*/ ytitle("IRR") /*
	*/ xtitle("Year") /*	
	*/ ylabel(1.0(0.1)1.5, format(%3.1f) labsize(2.5) angle(horizontal)) /*
	*/ xlabel(1 "1981" 10 "1990" 15 "1995" 20 "2000" 25 "2005" 30 "2010" 35 "2015" 40 "2020" 44 "2024" /*
	*/ , labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4)
	*graph save "`dir4'/1-IRR_bygender_year_`savstr'.gph", replace
	graph export "`dir4'/1-IRR_bygender_year_`savstr'.png", width(1200) replace
	window manage close graph
	qui export excel "`dir5'graphexport.xlsx", firstrow(variables) sheet("IRR_ovtime_`strmidage'-`endmidage'") sheetreplace
end
