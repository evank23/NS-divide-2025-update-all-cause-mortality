//nbreg sens analysis
capture program drop stats3
program stats3
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
	qui log using "`dir4'xlog17-paper_stats_nbreg_sens", name(log1) smcl replace

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
	nbreg deaths i.north male i.agecat, exposure(population) irr
	//deaths assuming all data correspond to 1) the South 2) the North
	margins i.north
	//multiply difference * 50 years = 44250 cases

	//Table 2 - trend around recessions for under 75s
	qui use `tempf', clear
	qui keep if agecat<=15
	nbreg deaths i.north male i.agecat if year>=1981 & year<=1990, exposure(population) irr
	nbreg deaths i.north male i.agecat if year>=1991 & year<=2000, exposure(population) irr
	nbreg deaths i.north male i.agecat if year>=2001 & year<=2010, exposure(population) irr
	nbreg deaths i.north male i.agecat if year>=2011 & year<=2020, exposure(population) irr
	nbreg deaths i.north male i.agecat if year>=2011 & year<=2024, exposure(population) irr

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
		qui nbreg deaths i.north male _Imidage_2 _Imidage_3 _Imidage_4 _Imidage_5 _Imidage_6 _Imidage_7 _Imidage_8 /*
		*/ _Imidage_9 _Imidage_10 _Imidage_11 _Imidage_12 _Imidage_13 _Imidage_14 _Imidage_15 _Imidage_16  if year==`yr', exposure(population) irr
		matrix mat1[`=`yr'-1980',1]=`yr'
		matrix mat1[`=`yr'-1980',2]=exp(_b[1.north])
		matrix mat1[`=`yr'-1980',3]=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]
		matrix mat1[`=`yr'-1980',4]=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]	
		di as result "`yr'" _col(10) %6.3f `=exp(_b[1.north])' _col(20) %6.3f `=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]' /*
		*/ _col(30) %6.3f `=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]' _continue
		//for margins I need interaction terms
		*local yr=1981
		qui nbreg deaths i.north male i.north#i.agecat if year==`yr', exposure(population) irr
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

	//all age groups relevant to Figure4 - age groups of 10 years roughly
	di _newline(5)
	qui use `tempf', clear
	local str1 "35 45 55 75"
	local str2 "44 54 64 90"
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
			qui nbreg deaths i.north male _Imidage_* if year==`yr' & midage>=`=word("`str1'",`i')' & midage<=`=word("`str2'",`i')', exposure(population) irr
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
**# Bookmark #2
	local str1 "25 30 45 60 75"
	local str2 "44 44 59 74 90"
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
			qui nbreg deaths i.north male _Imidage_* if year==`yr' & midage>=`=word("`str1'",`i')' & midage<=`=word("`str2'",`i')', exposure(population) irr
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

	qui use `tempf', clear

	//proportions aged 65 or over and 25-44
	qui gen age2=0
	qui replace age2=1 if agecat>=14
	collapse (sum) deaths population, by (age2 north year)	
	local str0 "South"
	local str1 "North"
	//65 or over
	forvalues rg=0(1)1 {
		foreach yr in 1981 2024 {	
			qui sum population if year==`yr' & north==`rg'
			local den=r(mean)
			qui sum population if year==`yr' & north==`rg' & age2==1
			local num=r(mean)
			local rate=100*`num'/`den'
			di "Proportion of aged 65 or over, `str`rg'' `yr': " %4.1f `rate'
		}
	}
	//25-44 by sex
	di
	qui use `tempf', clear
	qui gen age2=0
	qui replace age2=1 if agecat>=6 & agecat<=9
	collapse (sum) deaths population, by (age2 north year male)	
	local sx0 "Females"
	local sx1 "Males"	
	forvalues sx=0(1)1 {
		forvalues rg=0(1)1 {
			foreach yr in 1981 2024 {	
				qui sum population if year==`yr' & north==`rg' & male==`sx'
				local den=r(mean)
				qui sum population if year==`yr' & north==`rg' & male==`sx' & age2==1
				local num=r(mean)
				local rate=100*`num'/`den'
				di "`sx`rg'' aged 25-44 `yr', `str`rg'': " %4.1f `rate'
			}
		}
	}

	//premature mortality
	di
	qui use `tempf', clear
	qui gen age2=0
	qui replace age2=1 if agecat<=15
	collapse (sum) deaths population, by (age2 north year)	
	local str0 "South"
	local str1 "North"
	forvalues rg=0(1)1 {
		foreach yr in 1981 2000 2020 2024 {	
			qui sum population if year==`yr' & north==`rg' & age2==1
			local den=r(mean)
			qui sum deaths if year==`yr' & north==`rg' & age2==1
			local num=r(mean)
			local rate=100*`num'/`den'
			di "Premature (<75) mortality rates, `str`rg'' `yr': " %4.2f `rate'
		}
	}
	
	//premature mortality again
	local max=0
	local min=100
	forvalues yr=1981(1)2024 {		
		forvalues rg=0(1)1 {
			qui sum population if year==`yr' & north==`rg' & age2==1
			local den=r(mean)
			qui sum deaths if year==`yr' & north==`rg' & age2==1
			local num=r(mean)
			local rate`rg'`yr'=100*`num'/`den'
		}
		local rated`yr' = `rate1`yr''-`rate0`yr''
		if `rated`yr''>`max' {
			local max = `rated`yr''
			local yrmax=`yr'
		}
		if `rated`yr''<`min' {
			local min = `rated`yr''
			local yrmin=`yr'			
		}		
	}
	di _newline(2) as text "Premature (<75) mortality rates. Max N-S diff in `yrmax': " as result %4.3f `max'
	di as text "Premature (<75) mortality rates. Min N-S diff in `yrmin': " as result %4.3f `min'
	di as text year _col(10) as text "South" _col(20) as text "North" _col(30) as text "North-South"
	forvalues yr=1981(1)2024 {
		di _col(2) as res `yr' _col(10) as res %4.2f `rate0`yr'' _col(20) as res %4.2f `rate1`yr'' _col(30) as res %4.3f `rated`yr''	
	}	
	
	di _newline(2) as text "Gaps in premature SMRs"
	qui use "`dir5'SMRdata_und752.dta", clear
	keep year north allD SMR SMRlo SMRup
	reshape wide allD SMR SMRlo SMRup, i(year) j(north)
	gen diff= SMR1- SMRlo0
	list year diff, clean
	

	qui log close _all
	qui log2html "`dir4'xlog17-paper_stats_nbreg_sens", replace ti("Paper statistics (sens)")

end
	