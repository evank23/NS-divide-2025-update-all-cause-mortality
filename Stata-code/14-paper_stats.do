capture program drop stats1
program stats1
	local dir1 "C:/Users/`c(username)'/The University of Manchester Dropbox/Evangelos Kontopantelis/zP_drive/Evan/Informatics/North-South divide/2025_all-causes/"
	local dir2 "`dir1'/data/"
	local dir3 "`dir1'/shared/"
	local dir4 "`dir3'/past_paper/"
	local dir5 "`dir3'/export/"
	capture mkdir "`dir3'"
	capture mkdir "`dir4'"
	capture mkdir "`dir5'"
	cd "`dir1'"
	use "`dir2'All_cause_dataset.dta", clear
	set more off
	
	tempfile tempf tempg temph0 temph1
	qui save `tempf', replace
	capture log close _all
	qui log using "`dir4'xlog14-paper_stats", name(log1) smcl replace

	//population size ratios
	collapse (sum) population deaths, by(year north male)
	qui sum population if year==1981 & north==0 & male==0
	local pop_f_s_1981=r(mean)
	qui sum population if year==1981 & north==1 & male==0
	local pop_f_n_1981=r(mean)
	local temp1 = `pop_f_n_1981'/`pop_f_s_1981'
	di as text "N:S ratio for females, 1981:" as result _col(30) %5.3f `temp1'
	qui sum population if year==1981 & north==0 & male==1
	local pop_m_s_1981=r(mean)
	qui sum population if year==1981 & north==1 & male==1
	local pop_m_n_1981=r(mean)
	local temp2 = `pop_m_n_1981'/`pop_m_s_1981'
	di as text "N:S ratio for males, 1981:" as result _col(30) %5.3f `temp2'
	qui sum population if year==2024 & north==0 & male==0
	local pop_f_s_2024=r(mean)
	qui sum population if year==2024 & north==1 & male==0
	local pop_f_n_2024=r(mean)
	local temp3 = `pop_f_n_2024'/`pop_f_s_2024'
	di as text "N:S ratio for females, 2024:" as result _col(30) %5.3f `temp3'
	qui sum population if year==2024 & north==0 & male==1
	local pop_m_s_2024=r(mean)
	qui sum population if year==2024 & north==1 & male==1
	local pop_m_n_2024=r(mean)
	local temp4 = `pop_m_n_2024'/`pop_m_s_2024'
	di "N:S ratio for males, 2024:" as result _col(30) %5.3f `temp4'

	//demographics
	qui use `tempf', clear
	qui keep if year<=1985
	tab year
	qui gen temp=midage*population
	collapse (sum) population temp, by(north male)
	qui gen medage=temp/population
	list
	qui use `tempf', clear
	qui keep if year>=2020
	tab year
	qui gen temp=midage*population
	collapse (sum) population temp, by(north male)
	qui gen medage=temp/population
	list

	//table 1
	local str1 "1981 2024"
	local str2 "1981 2024"
	forvalues iter=1(1)`=wordcount("`str1'")' {
		qui use `tempf', clear
		qui keep if year>=`=word("`str1'",`iter')' & year<=`=word("`str2'",`iter')'
		collapse (sum) population (first) ageband, by(agecat north male)
		foreach x in 0 1 {
			foreach y in 0 1 {
				qui sum population if north==`x' & male==`y'
				local den`x'`y'=r(sum)
			}
		}
		qui gen perc=.
		foreach x in 0 1 {
			foreach y in 0 1 {
				qui replace perc=100*population/`den`x'`y'' if north==`x' & male==`y'
			}
		}
		format perc %5.2f
		rename perc perc`iter'
		rename population population`iter'
		capture save `tempg'
		if _rc!=0 {
			qui merge 1:1 agecat ageband north male using `tempg'
			qui drop _merge
			qui save `tempg', replace
		}
	}
	order agecat ageband north male perc* population*
	sort north male agecat
	qui save `tempg', replace
	format perc1 %8.4f
	format perc2 %8.4f
	list agecat north male perc*, sepby(north male)

	//population pyramids
	local strx0 "South"
	local strx1 "North"
	foreach x in 0 1 {
		qui use `tempg', clear
		qui keep if north==`x'
		qui drop perc*
		reshape wide population*, i(agecat) j(male)
		order agecat ageband north
		//average population rather than aggregate
		forvalues y=1(1)`=wordcount("`str1'")' {
			local tp=`=word("`str2'",`y')'-`=word("`str1'",`y')'+1
			qui replace population`y'0=population`y'0/`tp'
			qui replace population`y'1=population`y'1/`tp'
		}
		qui save `temph`x'', replace
		//graphs
		gen zero=0
		forvalues y=1(1)`=wordcount("`str1'")' {
			qui replace population`y'1=-population`y'1
			qui replace population`y'0=population`y'0/100000
			qui replace population`y'1=population`y'1/100000
			twoway bar population`y'1 agecat, horizontal || bar population`y'0 agecat, horizontal /*
			*/ || scatter agecat zero, mlabel(agecat) mlabcolor(black) msymbol(none) /*
			*/ title("Male and Female English population, `strx`x''") /*
			*/ subtitle("Annual average from `=word("`str1'",`y')' to `=word("`str2'",`y')'") /*
			*/ xtitle("Population in hundeds of thousands") ytitle("") yscale(noline) ylabel(none) /*
			*/ xlabel(-10 "10" -8 "8" -6 "6" -4 "4" -2 "2" 0(2)10) /*
			*/ legend(off) text(15 -8 "Male") text(15 8 "Female")
			*graph save "`dir4'poppyr_`strx`x''_`=word("`str1'",`y')'to`=word("`str2'",`y')'.gph", replace
			graph export "`dir4'poppyr_`strx`x''_`=word("`str1'",`y')'to`=word("`str2'",`y')'.png", width(1200) replace
		}
	}
	window manage close graph

	//death rates
	qui use `tempf', clear
	collapse (sum) deaths population, by(year north male)
	gen drate=deaths/population*100
	gen sterror=sqrt(drate*(100-drate)/population)
	drop deaths population
	reshape wide drate sterror, i(year north) j(male) 
	reshape wide drate* sterror*, i(year) j(north) 
	label var drate00 "Death rate: females/South"
	label var drate01 "Death rate: females/North"
	label var drate10 "Death rate: males/South"
	label var drate11 "Death rate: males/North"
	rename drate00 drateFS
	rename drate01 drateFN
	rename drate10 drateMS
	rename drate11 drateMN
	order year drate*
	list year drate*, clean

	//excess deaths
	qui use `tempf', clear
	poisson deaths i.north male i.agecat, exposure(population) irr
	//deaths assuming all data correspond to 1) the South 2) the North
	margins i.north
	//multiply difference * 50 years = 44250 cases

	//Table 2 - trend around recessions for under 75s
	qui use `tempf', clear
	qui keep if agecat<=15
	poisson deaths i.north male i.agecat if year>=1981 & year<=1990, exposure(population) irr
	poisson deaths i.north male i.agecat if year>=1991 & year<=2000, exposure(population) irr
	poisson deaths i.north male i.agecat if year>=2001 & year<=2010, exposure(population) irr
	poisson deaths i.north male i.agecat if year>=2011 & year<=2020, exposure(population) irr
	poisson deaths i.north male i.agecat if year>=2011 & year<=2024, exposure(population) irr

	//Figure 2
	matrix mat1 = J(44,4,.)
	matrix colnames mat1 = year IRR loIRR upIRR
	matrix mat2 = J(44,7,.)
	matrix colnames mat2 = year dS lodS updS dN lodN updN
	matrix mat3 = J(44,5,.)
	matrix colnames mat3 = year dD lodD updD plSE		
	qui use `tempf', clear
	qui keep if agecat<=15
	xi i.midage
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Premature mortality (Aged under 75)"
			di as text "Year" _col(10) "IRR" _col(20) "loIRR" _col(30) "upIRR" _col(40) "deathsS" /*
			*/ _col(50) "lodthS" _col(60) "updthS" _col(70) "deathsN" _col(80) "lodthN" _col(90) "updthN" /*
			*/ _col(100) "diff" _col(110) "lodiff" _col(120) "updiff" _col(130) "poolSE" _col(140) "deathsN"
		}
		qui poisson deaths i.north male _Imidage_2 _Imidage_3 _Imidage_4 _Imidage_5 _Imidage_6 _Imidage_7 _Imidage_8 /*
		*/ _Imidage_9 _Imidage_10 _Imidage_11 _Imidage_12 _Imidage_13 _Imidage_14 _Imidage_15 _Imidage_16  if year==`yr', exposure(population) irr
		matrix mat1[`=`yr'-1980',1]=`yr'
		matrix mat1[`=`yr'-1980',2]=exp(_b[1.north])
		matrix mat1[`=`yr'-1980',3]=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]
		matrix mat1[`=`yr'-1980',4]=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]	
		di as result "`yr'" _col(10) %6.3f `=exp(_b[1.north])' _col(20) %6.3f `=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]' /*
		*/ _col(30) %6.3f `=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]' _continue
		//for margins I need interaction terms
		*local yr=1981
		qui poisson deaths i.north male i.north#i.agecat if year==`yr', exposure(population) irr
		qui margins i.north#i.agecat
		matrix A=r(table)
		//get overall numbers for north and south and pool SEs
		local sumS=0
		forvalues i=1(1)16 {
			local sumS=`sumS'+A[1,`i']
		}
		local plSES=sqrt((A[2,1]^2+A[2,2]^2+A[2,3]^2+A[2,4]^2+A[2,5]^2+A[2,6]^2+A[2,7]^2+A[2,8]^2+A[2,9]^2+A[2,10]^2+A[2,11]^2+A[2,12]^2+A[2,13]^2+A[2,14]^2+A[2,15]^2+A[2,16]^2)/16)
		local sumN=0
		forvalues i=17(1)32 {
			local sumN=`sumN'+A[1,`i']
		}
		*di `sumS' " " `sumN'
		local plSEN=sqrt((A[2,17]^2+A[2,18]^2+A[2,19]^2+A[2,20]^2+A[2,21]^2+A[2,22]^2+A[2,23]^2+A[2,24]^2+A[2,25]^2+A[2,26]^2+A[2,27]^2+A[2,28]^2+A[2,29]^2+A[2,30]^2+A[2,31]^2+A[2,32]^2)/16)	
		matrix mat2[`=`yr'-1980',1]=`yr'
		matrix mat2[`=`yr'-1980',2]=`sumS'
		matrix mat2[`=`yr'-1980',3]=`sumS'-invnormal(0.975)*`plSES'
		matrix mat2[`=`yr'-1980',4]=`sumS'+invnormal(0.975)*`plSES'
		matrix mat2[`=`yr'-1980',5]=`sumN'
		matrix mat2[`=`yr'-1980',6]=`sumN'-invnormal(0.975)*`plSEN'
		matrix mat2[`=`yr'-1980',7]=`sumN'+invnormal(0.975)*`plSEN'	
		//difference
		matrix mat3[`=`yr'-1980',1]=`yr'
		matrix mat3[`=`yr'-1980',2]=`sumN'-`sumS'
		local poolse=sqrt((`plSES'^2+`plSES'^2)/2)
		matrix mat3[`=`yr'-1980',3]=mat3[`=`yr'-1980',2]-invnormal(0.975)*`poolse'	
		matrix mat3[`=`yr'-1980',4]=mat3[`=`yr'-1980',2]+invnormal(0.975)*`poolse'
		matrix mat3[`=`yr'-1980',5]=`poolse'
		//number of northern deaths
		qui sum deaths if year==`yr' & north==1
		local totnrth=r(sum)
		di _col(40) %6.0f `sumS' _col(50) %6.0f mat2[`=`yr'-1980',3] _col(60) %6.0f mat2[`=`yr'-1980',4] /*
		*/ _col(70) %6.0f `sumN' _col(80) %6.0f mat2[`=`yr'-1980',6] _col(90) %6.0f mat2[`=`yr'-1980',7] /*
		*/ _col(100) %6.0f mat3[`=`yr'-1980',2] _col(110) %6.0f mat3[`=`yr'-1980',3] _col(120) %6.0f mat3[`=`yr'-1980',4] /*
		*/ _col(130) %4.2f mat3[`=`yr'-1980',5] _col(140) %6.0f `totnrth'
	}
	clear
	qui svmat mat1, names(col)
	qui drop year
	qui svmat mat2, names(col)
	qui drop year 
	qui svmat mat3, names(col)
	order year
	//Fig2A
	//to shade between xlines
	foreach x in IRR upIRR loIRR {
		qui replace `x'=(`x'-1)*100
	}	
	qui sum upIRR
	local max=ceil(10*r(max))
	local max=`max'/10
	local xstr1 "1980 1990 2008 2020"
	local xstr2 "1981 1991 2009 2022"
	capture drop c1 c2 c3 c4
	forvalues i=1(1)4 {
		gen c`i'=`max' if (year>=`=word("`xstr1'",`i')' & year<=`=word("`xstr2'",`i')')
	}
	graph twoway area c1 year, bcolor(sandb%30) || area c2 year, bcolor(sandb%30) /*
	*/ || area c3 year, bcolor(sandb%30) || area c4 year, bcolor(sandb%30) /*	
	*/ || rarea loIRR upIRR year, color(gs14) || line IRR year, lcolor(forest_green) /*
	*/ legend(order(6 "People under 75" 5 "95% CI")) /*
	*/ ytitle("Percentage northern excess mortality") /*
	*/ xtitle("Year") /*
	*/ ylabel(, format(%3.0f) labsize(2.5) angle(horizontal)) /*
	*/ xlabel(1981 1991 2001 2011 2021 2024, labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4)
	*graph save "`dir4'/Fig2A-IRR_people_year_`savstr'.gph", replace
	*graph export "`dir4'/Fig2A-IRR_people_year_`savstr'.png", width(1200) replace
	window manage close graph
	//Fig2B
	//to shade between xlines
	qui sum updN
	local max=ceil(r(max)/10000)
	local max=`max'*10000
	qui drop c1 c2 c3 c4
	forvalues i=1(1)4 {
		gen c`i'=`max' if (year>=`=word("`xstr1'",`i')' & year<=`=word("`xstr2'",`i')')
	}
	graph twoway area c1 year, bcolor(sandb%30) || area c2 year, bcolor(sandb%30) /*
	*/ || area c3 year, bcolor(sandb%30) || area c4 year, bcolor(sandb%30) /*			
	*/ || rarea lodS updS year, color(gs14) || rarea lodN updN year, color(gs14) /*
	*/ || line dS year, lcolor(blue) || line dN year, lcolor(maroon) /*	
	*/ legend(order(7 "South (aged<75)" 8 "North (aged<75)" 5 "95% CI" 1 "Stress periods")) /*
	*/ ytitle("Estimated number of deaths (adjusted)", size(small)) /*
	*/ xtitle("Year") /*	
	*/ ylabel(, format(%3.0f) labsize(2.3) angle(horizontal)) /*
	*/ xlabel(1981 1991 2001 2011 2021 2024, labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4) /*
	*/ plotregion(margin(tiny)) ylabel(,nogextend)
	*graph save "`dir4'/Fig2B-estdeaths_byregion.gph", replace
	graph export "`dir4'/Fig2B-estdeaths_byregion.png", width(1200) replace

	//Figure 4
	qui use `tempf', clear
	xi i.midage	
	matrix mat1 = J(44,7,.)
	matrix colnames mat1 = year IRR2549 loIRR2549 upIRR2549 IRR5074 loIRR5074 upIRR5074
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Overall by year"
			di as text "Year" _col(10) "IRR 25-49" _col(20) "loIRR 25-49" _col(30) "upIRR 25-49" _col(40) "IRR 50-74" /*
			*/ _col(50) "loIRR 50-74" _col(60) "upIRR 50-74"
		}
		qui poisson deaths i.north male _Imidage_7 _Imidage_8 _Imidage_9 _Imidage_10 _Imidage_11 /*
		*/ if year==`yr' & midage>=25 & midage<=50, exposure(population) irr
		matrix mat1[`=`yr'-1980',1]=`yr'
		matrix mat1[`=`yr'-1980',2]=exp(_b[1.north])
		matrix mat1[`=`yr'-1980',3]=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]
		matrix mat1[`=`yr'-1980',4]=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]	
		di as result "`yr'" _col(10) %6.3f `=exp(_b[1.north])' _col(20) %6.3f `=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]' /*
		*/ _col(30) %6.3f `=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]' _continue
		qui poisson deaths i.north male _Imidage_12 _Imidage_13 _Imidage_14 _Imidage_15 _Imidage_16 /*
		*/ if year==`yr' & midage>=50 & midage<=75, exposure(population) irr
		matrix mat1[`=`yr'-1980',5]=exp(_b[1.north])
		matrix mat1[`=`yr'-1980',6]=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]
		matrix mat1[`=`yr'-1980',7]=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]	
		di _col(40) %6.3f `=exp(_b[1.north])' _col(50) %6.3f `=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]' /*
		*/ _col(60) %6.3f `=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]'
	}
	clear
	qui svmat mat1, names(col)
	qui drop year
	qui svmat mat2, names(col)
	order year
	//Fig4
	//to shade between xlines
	foreach x in IRR2549 loIRR2549 upIRR2549 IRR5074 loIRR5074 upIRR5074 {
		qui replace `x'=(`x'-1)*100
	}	
	qui sum upIRR2549
	local max=r(max)
	forvalues i=1(1)4 {
		gen c`i'=`max' if (year>=`=word("`xstr1'",`i')' & year<=`=word("`xstr2'",`i')')
	}
	graph twoway area c1 year, bcolor(sandb%30) || area c2 year, bcolor(sandb%30) /*
	*/ || area c3 year, bcolor(sandb%30) || area c4 year, bcolor(sandb%30) /*			
	*/ || rarea loIRR2549 upIRR2549 year, color(gs14) || rarea loIRR5074 upIRR5074 year, color(gs14) /*
	*/ || line IRR2549 year, lcolor(dkorange) || line IRR5074 year, lcolor(ebblue) /*	
	*/ legend(order(7 "aged 25-49" 8 "aged 50-74" 5 "95% CI" 1 "Stress periods")) /*
	*/ ytitle("Percentage northern excess mortality", size(medsmall)) /*
	*/ xtitle("Year") /*	
	*/ ylabel(, format(%3.0f) labsize(2.5) angle(horizontal)) /*
	*/ xlabel(1981 1991 2001 2011 2021 2024, labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4) /*
	*/ plotregion(margin(tiny)) ylabel(,nogextend)
	*graph save "`dir4'/Fig4-IRR_otherage_byregion.gph", replace
	graph export "`dir4'/Fig4-IRR_otherage_byregion.png", width(1200) replace

	//Additional Figure 4s
	qui use `tempf', clear
	xi i.midage	
	matrix mat1 = J(44,10,.)
	matrix colnames mat1 = year IRR2534 loIRR2534 upIRR2534 IRR3544 loIRR3544 upIRR3544 IRR4554 loIRR4554 upIRR4554
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Overall by year"
			di as text "Year" _col(10) "IRR2534" _col(20) "lo2534" _col(30) "up2534" _col(40) "IRR3544" /*
			*/ _col(50) "lo3544" _col(60) "up3544" _col(70) "IRR4554" _col(80) "lo4554" _col(90) "up4554"
		}
		qui poisson deaths i.north male _Imidage_7 _Imidage_8  /*
		*/ if year==`yr' & midage>=25 & midage<=34, exposure(population) irr
		matrix mat1[`=`yr'-1980',1]=`yr'
		matrix mat1[`=`yr'-1980',2]=exp(_b[1.north])
		matrix mat1[`=`yr'-1980',3]=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]
		matrix mat1[`=`yr'-1980',4]=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]	
		di as result "`yr'" _col(10) %6.3f `=exp(_b[1.north])' _col(20) %6.3f `=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]' /*
		*/ _col(30) %6.3f `=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]' _continue
		qui poisson deaths i.north male _Imidage_9 _Imidage_10 /*
		*/ if year==`yr' & midage>=35 & midage<=44, exposure(population) irr
		matrix mat1[`=`yr'-1980',5]=exp(_b[1.north])
		matrix mat1[`=`yr'-1980',6]=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]
		matrix mat1[`=`yr'-1980',7]=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]	
		di _col(40) %6.3f `=exp(_b[1.north])' _col(50) %6.3f `=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]' /*
		*/ _col(60) %6.3f `=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]' _continue
		qui poisson deaths i.north male _Imidage_11 _Imidage_12 /*
		*/ if year==`yr' & midage>=45 & midage<=54, exposure(population) irr
		matrix mat1[`=`yr'-1980',8]=exp(_b[1.north])
		matrix mat1[`=`yr'-1980',9]=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]
		matrix mat1[`=`yr'-1980',10]=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]	
		di _col(70) %6.3f `=exp(_b[1.north])' _col(80) %6.3f `=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]' /*
		*/ _col(90) %6.3f `=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]'
	}
	clear
	qui svmat mat1, names(col)
	qui drop year
	qui svmat mat2, names(col)
	order year
	//Fig4a
	//to shade between xlines
	foreach x in IRR2534 IRR3544 IRR4554 upIRR2534 upIRR3544 upIRR4554 loIRR2534 loIRR3544 loIRR4554 {
		qui replace `x'=(`x'-1)*100
	}
	local max=0
	foreach x in upIRR2534 upIRR3544 upIRR4554 {
		qui sum `x'
		if r(max)>`max' {
			local max=r(max)
		}
	}
	forvalues i=1(1)4 {
		gen c`i'=60 if (year>=`=word("`xstr1'",`i')' & year<=`=word("`xstr2'",`i')')
		gen c`i'x=-20 if (year>=`=word("`xstr1'",`i')' & year<=`=word("`xstr2'",`i')')		
	}
	graph twoway area c1 year, bcolor(sandb%30) || area c2 year, bcolor(sandb%30) /*
	*/ || area c3 year, bcolor(sandb%30) || area c4 year, bcolor(sandb%30) /*	
	*/ || area c1x year, bcolor(sandb%30) || area c2x year, bcolor(sandb%30) /*
	*/ || area c3x year, bcolor(sandb%30) || area c4x year, bcolor(sandb%30) /*
	*/ || rarea loIRR2534 upIRR2534 year, color(gs14) || rarea loIRR3544 upIRR3544 year, color(gs14) /*
	*/ || rarea loIRR4554 upIRR4554 year, color(gs14) || line IRR2534 year, lcolor(dkorange) /*
	*/ || line IRR3544 year, lcolor(ebblue) || line IRR4554 year, lcolor(green) /*	
	*/ legend(order(12 13 14 9 1)) /*
	*/ legend(rows(1) lab(12 "aged 25-34") lab(13 "aged 35-44") lab(14 "aged 45-54")) /*
	*/ legend(rows(2) lab(9 "95% CI") lab(1 "Stress periods")) /*
	*/ ytitle("Percentage northern excess mortality", size(medsmall)) /*
	*/ xtitle("Year") /*	
	*/ ylabel(, format(%3.0f) labsize(2.5) angle(horizontal)) /*
	*/ xlabel(1981 1991 2001 2011 2021 2024, labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4) /*
	*/ plotregion(margin(tiny)) ylabel(,nogextend)
	*graph save "`dir4'/Fig4a-IRR_otherage_byregion.gph", replace
	graph export "`dir4'/Fig4a-IRR_otherage_byregion.png", width(1200) replace
	//Fig4b
	//to shade between xlines
	local max=0
	foreach x in upIRR2534 upIRR3544 {
		qui sum `x'
		if r(max)>`max' {
			local max=r(max)
		}
	}
	*drop c*
	*forvalues i=1(1)4 {
		*gen c`i'=`max' if (year>=`=word("`xstr1'",`i')' & year<=`=word("`xstr2'",`i')')
	*}
	graph twoway area c1 year, bcolor(sandb%30) || area c2 year, bcolor(sandb%30) /*
	*/ || area c3 year, bcolor(sandb%30) || area c4 year, bcolor(sandb%30) /*
	*/ || area c1x year, bcolor(sandb%30) || area c2x year, bcolor(sandb%30) /*
	*/ || area c3x year, bcolor(sandb%30) || area c4x year, bcolor(sandb%30) /*	
	*/ || rarea loIRR2534 upIRR2534 year, color(gs14) || rarea loIRR3544 upIRR3544 year, color(gs14) /*
	*/ || line IRR2534 year, lcolor(dkorange) /*
	*/ || line IRR3544 year, lcolor(ebblue) /*	
	*/ legend(order(11 12 9 1)) /*
	*/ legend(rows(1) lab(11 "aged 25-34") lab(12 "aged 35-44")) /*
	*/ legend(rows(2) lab(9 "95% CI") lab(1 "Stress periods")) /*
	*/ ytitle("Percentage northern excess mortality", size(medsmall)) /*
	*/ xtitle("Year") /*	
	*/ ylabel(, format(%3.0f) labsize(2.5) angle(horizontal)) /*
	*/ xlabel(1981 1991 2001 2011 2021 2024, labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4) /*
	*/ plotregion(margin(tiny)) ylabel(,nogextend)
	*graph save "`dir4'/Fig4b-IRR_otherage_byregion.gph", replace
	graph export "`dir4'/Fig4b-IRR_otherage_byregion.png", width(1200) replace

	//all age groups relevant to Figure4 - age groups of 10 years roughly
	di _newline(5)
	qui use `tempf', clear
	local str1 "00 15 25 35 45 55 65 75"
	local str2 "14 24 34 44 54 64 74 90"
	xi i.midage	
	forvalues i=1(1)`=wordcount("`str1'")' {
		matrix mat`=word("`str1'",`i')' = J(44,4,.)
		matrix colnames mat`=word("`str1'",`i')' = year IRR loIRR upIRR
	}

	forvalues i=1(1)`=wordcount("`str1'")' {
		forvalues yr=1981(1)2024 {
			if `yr'==1981 {	
				di as text _newline(2) "Overall by year: `=word("`str1'",`i')' to `=word("`str2'",`i')'"
				di as text "Year" _col(10) "IRR" _col(20) "loIRR" _col(30) "upIRR"
			}
			qui poisson deaths i.north male _Imidage_* if year==`yr' & midage>=`=word("`str1'",`i')' & midage<=`=word("`str2'",`i')', exposure(population) irr
			matrix mat`=word("`str1'",`i')'[`=`yr'-1980',1]=`yr'
			matrix mat`=word("`str1'",`i')'[`=`yr'-1980',2]=exp(_b[1.north])
			matrix mat`=word("`str1'",`i')'[`=`yr'-1980',3]=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]
			matrix mat`=word("`str1'",`i')'[`=`yr'-1980',4]=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]	
			di as result "`yr'" _col(10) %6.3f `=exp(_b[1.north])' _col(20) %6.3f `=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]' /*
			*/ _col(30) %6.3f `=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]'
		}
	}
	clear
	qui svmat mat1, names(col)
	qui drop year
	qui svmat mat2, names(col)
	order year

	//all age groups relevant to Figure4 - age groups of 15 years roughly
	di _newline(5)
	qui use `tempf', clear
	local str1 "00 15 25 30 45 60 75"
	local str2 "14 29 44 44 59 74 90"
	xi i.midage	
	forvalues i=1(1)`=wordcount("`str1'")' {
		matrix mat`=word("`str1'",`i')' = J(44,4,.)
		matrix colnames mat`=word("`str1'",`i')' = year IRR loIRR upIRR
	}

	forvalues i=1(1)`=wordcount("`str1'")' {
		forvalues yr=1981(1)2024 {
			if `yr'==1981 {
				di as text _newline(2) "Overall by year: `=word("`str1'",`i')' to `=word("`str2'",`i')'"
				di as text "Year" _col(10) "IRR" _col(20) "loIRR" _col(30) "upIRR"
			}
			qui poisson deaths i.north male _Imidage_* if year==`yr' & midage>=`=word("`str1'",`i')' & midage<=`=word("`str2'",`i')', exposure(population) irr
			matrix mat`=word("`str1'",`i')'[`=`yr'-1980',1]=`yr'
			matrix mat`=word("`str1'",`i')'[`=`yr'-1980',2]=exp(_b[1.north])
			matrix mat`=word("`str1'",`i')'[`=`yr'-1980',3]=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]
			matrix mat`=word("`str1'",`i')'[`=`yr'-1980',4]=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]	
			di as result "`yr'" _col(10) %6.3f `=exp(_b[1.north])' _col(20) %6.3f `=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]' /*
			*/ _col(30) %6.3f `=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]'
		}
	}
	clear
	qui svmat mat1, names(col)
	qui drop year
	qui svmat mat2, names(col)
	order year

	qui log close _all
	qui log2html "`dir4'xlog14-paper_stats", replace ti("Paper statistics")
end

