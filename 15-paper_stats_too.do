capture program drop stats2
program stats2
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
	qui log using "`dir4'xlog15-paper_stats2", name(log1) smcl replace

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
	
	//number of additional deaths for specific age groups
	//25 to 34
	matrix mat1 = J(44,4,.)
	matrix colnames mat1 = year IRR loIRR upIRR
	matrix mat2 = J(44,7,.)
	matrix colnames mat2 = year dS lodS updS dN lodN updN
	matrix mat3 = J(44,5,.)
	matrix colnames mat3 = year dD lodD updD plSE
	qui use `tempf', clear
	xi i.midage
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Aged 25 to 34 by year"
			di as text "Year" _col(10) "IRR" _col(20) "loIRR" _col(30) "upIRR" _col(40) "deathsS" /*
			*/ _col(50) "lodthS" _col(60) "updthS" _col(70) "deathsN" _col(80) "lodthN" _col(90) "updthN" /*
			*/ _col(100) "diff" _col(110) "lodiff" _col(120) "updiff" _col(130) "poolSE" _col(140) "deathsN"
		}
		qui poisson deaths i.north male _Imidage_7 _Imidage_8 if year==`yr' & inlist(agecat,6,7), exposure(population) irr
		matrix mat1[`=`yr'-1980',1]=`yr'
		matrix mat1[`=`yr'-1980',2]=exp(_b[1.north])
		matrix mat1[`=`yr'-1980',3]=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]
		matrix mat1[`=`yr'-1980',4]=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]	
		di as result "`yr'" _col(10) %6.3f `=exp(_b[1.north])' _col(20) %6.3f `=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]' /*
		*/ _col(30) %6.3f `=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]' _continue
		//for margins I need interaction terms
		qui poisson deaths i.north male i.north#i.agecat if year==`yr' & inlist(agecat,6,7), exposure(population) irr
		qui margins i.north#i.agecat
		matrix A=r(table)
		//get overall numbers for north and south and pool SEs
		local sumS=0
		forvalues i=1(1)2 {
			local sumS=`sumS'+A[1,`i']
		}
		local plSES=sqrt((A[2,1]^2+A[2,2]^2)/2)
		local sumN=0
		forvalues i=3(1)4 {
			local sumN=`sumN'+A[1,`i']
		}
		local plSEN=sqrt((A[2,3]^2+A[2,4]^2)/2)	
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
		//number of northern deathts
		qui sum deaths if year==`yr' & north==1 & inlist(agecat,6,7)
		local totnrth=r(sum)		
		di _col(40) %6.0f `sumS' _col(50) %6.0f mat2[`=`yr'-1980',3] _col(60) %6.0f mat2[`=`yr'-1980',4] /*
		*/ _col(70) %6.0f `sumN' _col(80) %6.0f mat2[`=`yr'-1980',6] _col(90) %6.0f mat2[`=`yr'-1980',7] /*
		*/ _col(100) %6.0f mat3[`=`yr'-1980',2] _col(110) %6.0f mat3[`=`yr'-1980',3] _col(120) %6.0f mat3[`=`yr'-1980',4] /*
		*/ _col(130) %4.2f mat3[`=`yr'-1980',5] _col(140) %6.0f `totnrth'		
	}
	/*clear
	qui svmat mat1, names(col)
	qui drop year
	qui svmat mat2, names(col)
	qui drop year 
	qui svmat mat3, names(col)
	order year*/
	//35 to 44
	matrix mat1 = J(44,4,.)
	matrix colnames mat1 = year IRR loIRR upIRR
	matrix mat2 = J(44,7,.)
	matrix colnames mat2 = year dS lodS updS dN lodN updN
	matrix mat3 = J(44,5,.)
	matrix colnames mat3 = year dD lodD updD plSE	
	qui use `tempf', clear
	xi i.midage
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Aged 35 to 44 by year"
			di as text "Year" _col(10) "IRR" _col(20) "loIRR" _col(30) "upIRR" _col(40) "deathsS" /*
			*/ _col(50) "lodthS" _col(60) "updthS" _col(70) "deathsN" _col(80) "lodthN" _col(90) "updthN" /*
			*/ _col(100) "diff" _col(110) "lodiff" _col(120) "updiff" _col(130) "poolSE" _col(140) "deathsN"
		}
		qui poisson deaths i.north male _Imidage_9 _Imidage_10 if year==`yr' & inlist(agecat,8,9), exposure(population) irr
		matrix mat1[`=`yr'-1980',1]=`yr'
		matrix mat1[`=`yr'-1980',2]=exp(_b[1.north])
		matrix mat1[`=`yr'-1980',3]=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]
		matrix mat1[`=`yr'-1980',4]=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]	
		di as result "`yr'" _col(10) %6.3f `=exp(_b[1.north])' _col(20) %6.3f `=exp(_b[1.north])-invnormal(0.975)*exp(_b[1.north])*_se[1.north]' /*
		*/ _col(30) %6.3f `=exp(_b[1.north])+invnormal(0.975)*exp(_b[1.north])*_se[1.north]' _continue
		//for margins I need interaction terms
		qui poisson deaths i.north male i.north#i.agecat if year==`yr' & inlist(agecat,8,9), exposure(population) irr
		qui margins i.north#i.agecat
		matrix A=r(table)
		//get overall numbers for north and south and pool SEs
		local sumS=0
		forvalues i=1(1)2 {
			local sumS=`sumS'+A[1,`i']
		}
		local plSES=sqrt((A[2,1]^2+A[2,2]^2)/2)
		local sumN=0
		forvalues i=3(1)4 {
			local sumN=`sumN'+A[1,`i']
		}
		local plSEN=sqrt((A[2,3]^2+A[2,4]^2)/2)	
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
		//number of northern deathts
		qui sum deaths if year==`yr' & north==1 & inlist(agecat,8,9)
		local totnrth=r(sum)		
		di _col(40) %6.0f `sumS' _col(50) %6.0f mat2[`=`yr'-1980',3] _col(60) %6.0f mat2[`=`yr'-1980',4] /*
		*/ _col(70) %6.0f `sumN' _col(80) %6.0f mat2[`=`yr'-1980',6] _col(90) %6.0f mat2[`=`yr'-1980',7] /*
		*/ _col(100) %6.0f mat3[`=`yr'-1980',2] _col(110) %6.0f mat3[`=`yr'-1980',3] _col(120) %6.0f mat3[`=`yr'-1980',4] /*
		*/ _col(130) %4.2f mat3[`=`yr'-1980',5] _col(140) %6.0f `totnrth'
	}
	/*clear
	qui svmat mat1, names(col)
	qui drop year
	qui svmat mat2, names(col)
	qui drop year 
	qui svmat mat3, names(col)
	order year*/
	
	di _newline(2) as text "Gaps in premature SMRs"
	qui use "`dir5'SMRdata_und752.dta", clear
	keep year north allD SMR SMRlo SMRup
	reshape wide allD SMR SMRlo SMRup, i(year) j(north)
	gen diff= SMR1- SMRlo0
	list year diff, clean
	

	qui log close _all
	qui log2html "`dir4'xlog15-paper_stats2", replace ti("Paper statistics (more)")

end
	