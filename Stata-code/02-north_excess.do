local dir1 "C:/Users/`c(username)'/The University of Manchester Dropbox/Evangelos Kontopantelis/zP_drive/Evan/Informatics/North-South divide/2025_all-causes/"
local dir2 "`dir1'data/"
local dir3 "`dir1'shared/"
cd "`dir1'"
set more off

capture log close _all
qui log using "`dir3'log2-north_excess", name(log1) smcl replace
use "`dir2'All_cause_dataset.dta", clear
local midagelst="0.5 3 7.5 12.5 17.5 22.5 27.5 32.5 37.5 42.5 47.5 52.5 57.5 62.5 67.5 72.5 77.5 82.5 87.5"

//WORKSHEET "Age Band North Excess"
//loops for gender and midage
foreach x in 0 1 {
	if `x'==0 {
		display _newline(1) as text "Female (all years)"
	}
	else {
		display _newline(1) as text "Male (all years)"
	}
	di as text _col(5) "midage" _col(15) "IRR" _col(30) "se"
	forvalues y=1(1)`=wordcount("`midagelst'")' {
		local lmage = word("`midagelst'",`y')
		qui poisson deaths north if midage==`lmage' & male==`x', exposure(population)
		//display
		di as result _col(5) `lmage' _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'		
	}
}

//NEW WORKSHEET "Age Band North Excess 2016-20"
//loops for gender and midage
foreach x in 0 1 {
	if `x'==0 {
		display _newline(1) as text "Female (2016-2020)"
	}
	else {
		display _newline(1) as text "Male (2016-2020)"
	}
	di as text _col(5) "midage" _col(15) "IRR" _col(30) "se"
	forvalues y=1(1)`=wordcount("`midagelst'")' {
		local lmage = word("`midagelst'",`y')
		qui poisson deaths north if midage==`lmage' & male==`x' & year>=2016 & year <=2020, exposure(population)
		//display
		di as result _col(5) `lmage' _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'		
	}
}
//loops for gender and midage
foreach x in 0 1 {
	if `x'==0 {
		display _newline(1) as text "Female (2021-2024)"
	}
	else {
		display _newline(1) as text "Male (2021-2024)"
	}
	di as text _col(5) "midage" _col(15) "IRR" _col(30) "se"
	forvalues y=1(1)`=wordcount("`midagelst'")' {
		local lmage = word("`midagelst'",`y')
		qui poisson deaths north if midage==`lmage' & male==`x' & year>=2021 & year <=2024, exposure(population)
		//display
		di as result _col(5) `lmage' _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'		
	}
}

//WORKSHEET "Summary"
local sstr0 "all years"
local scod0 "year<=2015"
local sstr1 "2020-2024"
local scod1 "year<=2024 & year>=2020"
local sstr2 "2016-2020"
local scod2 "year<=2020 & year>=2016"
local sstr3 "1981-2015"
local scod3 "year<=2015 & year>=1981"
foreach tp in 0 1 2 3 {
	local str0 "Female(`sstr`tp'')"
	local cod0 "i.midage if male==0 & `scod`tp''"
	local str1 "Male(`sstr`tp'')"
	local cod1 "i.midage if male==1 & `scod`tp''"
	local str2 "Persons(`sstr`tp'')"
	local cod2 "male i.midage if `scod`tp''"
	//loops for gender and midage
	foreach x in 2 1 0 {
		display _newline(1) as text "`str`x''"
		di as text _col(5) "Age" _col(15) "IRR" _col(30) "se"
		//all ages
		qui xi: poisson deaths north `cod`x'', exposure(population)
		di as result _col(5) "all" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
		//under 75
		qui xi: poisson deaths north `cod`x'' & midage<77.5, exposure(population)
		di as result _col(5) "under75" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
		//0 to 1
		qui xi: poisson deaths north `cod`x'' & midage==0.5, exposure(population)
		di as result _col(5) "0to1" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
		//1 to 19
		qui xi: poisson deaths north `cod`x'' & midage>0.5 & midage<22.5, exposure(population)
		di as result _col(5) "1to19" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
		//20 to 34
		qui xi: poisson deaths north `cod`x'' & midage>17.5 & midage<37.5, exposure(population)
		di as result _col(5) "20to34" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
		//35 to 49
		qui xi: poisson deaths north `cod`x'' & midage>32.5 & midage<52.5, exposure(population)
		di as result _col(5) "35to49" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
		//50 to 64
		qui xi: poisson deaths north `cod`x'' & midage>47.5 & midage<67.5, exposure(population)
		di as result _col(5) "50to64" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
		//65 to 74
		qui xi: poisson deaths north `cod`x'' & midage>62.5 & midage<77.5, exposure(population)
		di as result _col(5) "65to74" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
		//over 75
		qui xi: poisson deaths north `cod`x'' & midage>72.5, exposure(population)
		di as result _col(5) "over75" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
	}
}
//and interaction models
gen xmalenorth=male*north
xi: poisson deaths north male i.midage xmalenorth if year<=2024 & year>=2021, exposure (population) irr
xi: poisson deaths north male i.midage xmalenorth if year<=2020 & year>=2016, exposure (population) irr
xi: poisson deaths north male i.midage xmalenorth if year<=2015 & year>=1981, exposure (population) irr

//WORKSHEET "Overall"
forvalues yr=1981(1)2024 {
	if `yr'==1981 {
		di as text _newline(2) "Overall by year"
		di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
	}
	qui poisson deaths north male _Imidage_2 _Imidage_3 _Imidage_4 _Imidage_5 _Imidage_6 _Imidage_7 _Imidage_8 /*
	*/ _Imidage_9 _Imidage_10 _Imidage_11 _Imidage_12 _Imidage_13 _Imidage_14 _Imidage_15 _Imidage_16 _Imidage_17 /*
	*/ _Imidage_18 _Imidage_19 if year==`yr', exposure(population) irr
	di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
}
gen xnorthyear=year*north
xi: poisson deaths north male i.midage year xnorthyear if year >= 1981 & year <2000, exposure (population) irr
xi: poisson deaths north male i.midage year xnorthyear if year >= 2000, exposure (population) irr
xi: poisson deaths north male i.midage year xnorthyear if year >= 2000 & year <= 2008, exposure (population) irr
xi: poisson deaths north male i.midage year xnorthyear if year >= 2008, exposure (population) irr
xi: poisson deaths north male i.midage year xnorthyear if year >= 2016, exposure (population) irr
xi: poisson deaths north male i.midage year xnorthyear if year >= 2021, exposure (population) irr

//WORKSHEET "Overall by sex"
local str0 "Females"
local str1 "Males"
forvalues sx=0(1)1 {
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Overall by year for `str`sx''"
			di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
		}
		qui poisson  deaths north _Imidage_2 _Imidage_3 _Imidage_4 _Imidage_5 _Imidage_6 _Imidage_7 _Imidage_8 /*
		*/_Imidage_9 _Imidage_10 _Imidage_11 _Imidage_12 _Imidage_13 _Imidage_14 _Imidage_15 _Imidage_16 _Imidage_17 /*
		*/_Imidage_18 _Imidage_19 if male==`sx' & year==`yr', exposure(population) irr
		di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
	}
}
tabstat deaths if north==1 & male==0, by(year) stat(sum)
tabstat deaths if north==1 & male==1, by(year) stat(sum)
xi: poisson deaths north i.midage year if year >= 1981 & male==0, exposure (population) irr
xi: poisson deaths north i.midage year if year >= 1981 & male==1, exposure (population) irr

//WORKSHEET "Under 75"
forvalues yr=1981(1)2024 {
	if `yr'==1981 {
		di as text _newline(2) "Overall by year for under 75"
		di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
	}
	qui poisson deaths north male _Imidage_2 _Imidage_3 _Imidage_4 _Imidage_5 _Imidage_6 _Imidage_7 _Imidage_8 /*
	*/ _Imidage_9 _Imidage_10 _Imidage_11 _Imidage_12 _Imidage_13 _Imidage_14 _Imidage_15 _Imidage_16 _Imidage_17 /*
	*/ _Imidage_18 _Imidage_19 if year==`yr' & midage<75, exposure(population) irr
	di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
}
xi: poisson deaths north male i.midage year xnorthyear if midage<75, exposure (population) irr

//WORKSHEET "Under 75 by sex"
local str0 "Females"
local str1 "Males"
forvalues sx=0(1)1 {
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Overall by year for `str`sx'' under 75"
			di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
		}
		qui poisson  deaths north _Imidage_2 _Imidage_3 _Imidage_4 _Imidage_5 _Imidage_6 _Imidage_7 _Imidage_8 /*
		*/_Imidage_9 _Imidage_10 _Imidage_11 _Imidage_12 _Imidage_13 _Imidage_14 _Imidage_15 _Imidage_16 _Imidage_17 /*
		*/_Imidage_18 _Imidage_19 if male==`sx' & year==`yr' & midage<75, exposure(population) irr
		di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
	}
}
xi: poisson deaths north male year i.midage xnorthyear if midage<75, exposure(population) irr
xi: poisson deaths north male i.midage if year==2024 & midage<75, exposure(population) irr

//WORKSHEET "75+ by sex"
local str0 "Females"
local str1 "Males"
forvalues sx=0(1)1 {
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Overall by year for `str`sx'' 75+"
			di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
		}
		qui poisson  deaths north _Imidage_18 _Imidage_19 if male==`sx' & year==`yr' & midage>=75, exposure(population) irr
		di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
	}
}
xi: poisson deaths north male xmalenorth i.midage if year>1985 & midage>=75 , exposure( population) irr

//WORKSHEET "20-34"
forvalues yr=1981(1)2024 {
	if `yr'==1981 {
		di as text _newline(2) "Overall by year 20-34"
		di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
	}
	qui poisson deaths north male _Imidage_6 _Imidage_7 _Imidage_8 if year==`yr' & midage>17.5 & midage<37.5, exposure(population) irr
	di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
}
poisson deaths north year xnorthyear male _Imidage_7 _Imidage_8 if year>=1981 & year <=1995 & midage>17.5 & midage<37.5, exposure(population) irr
poisson deaths north year xnorthyear male _Imidage_7 _Imidage_8 if year>1995 & midage>22.5 & midage<37.5, exposure(population) irr
poisson deaths north year xnorthyear male _Imidage_7 _Imidage_8 if year>1995 & year<=2008 & midage>17.5 & midage<37.5, exposure(population) irr
poisson deaths north year xnorthyear male _Imidage_7 _Imidage_8 if year>2008 & year<=2015 & midage>17.5 & midage<37.5, exposure(population) irr
poisson deaths north year xnorthyear male _Imidage_7 _Imidage_8 if year>=2016 & year<=2020 & midage>17.5 & midage<37.5, exposure(population) irr
poisson deaths north year xnorthyear male _Imidage_7 _Imidage_8 if year>=2021 & year<=2024 & midage>17.5 & midage<37.5, exposure(population) irr
poisson deaths north male  _Imidage_6 _Imidage_7 _Imidage_8 if year==2024 & midage>22.5 & midage<37.5, exposure(population) irr

//WORKSHEET "Infants by sex"
local str0 "Females"
local str1 "Males"
forvalues sx=0(1)1 {
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Overall by year for `str`sx'' infants"
			di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
		}
		qui poisson deaths north if male==`sx' & midage==0.5 & year==`yr', exposure(population) irr
		di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
	}
}
xi: poisson deaths north i.midage if year==1981 & midage==0.5, exposure(population) irr
xi: poisson deaths north i.midage if year==1988 & midage==0.5, exposure(population) irr
xi: poisson deaths north i.midage if year==2008 & midage==0.5, exposure(population) irr
xi: poisson deaths north i.midage if year==2015 & midage==0.5, exposure(population) irr
xi: poisson deaths north i.midage if year==2020 & midage==0.5, exposure(population) irr
xi: poisson deaths north i.midage if year==2024 & midage==0.5, exposure(population) irr

//WORKSHEET "Children 1-19 by sex"
local str0 "Females"
local str1 "Males"
forvalues sx=0(1)1 {
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Overall by year for `str`sx'' 1-19"
			di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
		}
		qui poisson deaths north _Imidage_3 _Imidage_4 _Imidage_5 if male==`sx' & midage>=1 & midage<20 & year==`yr', exposure(population) irr
		di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
	}
}
poisson deaths north male if midage>=1 & midage<20 & year <1995, exposure(population) irr
poisson deaths north male if midage>=1 & midage<20 & year>=1995 & year<=2008, exposure(population) irr
poisson deaths north male if midage>=1 & midage<20 & year>2008 & year<=2015, exposure(population) irr
poisson deaths north male if midage>=1 & midage<20 & year>=2016 & year<=2020, exposure(population) irr
poisson deaths north male if midage>=1 & midage<20 & year>=2021 & year<=2024, exposure(population) irr
poisson deaths north male year xnorthyear if  midage>=1 & midage<20, exposure(population) irr

//WORKSHEET "20-34 by sex"
local str0 "Females"
local str1 "Males"
forvalues sx=0(1)1 {
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Overall by year for `str`sx'' 20-34"
			di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
		}
		qui poisson deaths north _Imidage_7 _Imidage_8 if midage>17.5 & midage<37.5 & male==`sx' & year==`yr', exposure(population) irr
		di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
	}
}
poisson deaths north male year xnorthyear if midage>=17.5 & midage<37.5 & year>=1981 & year<=1993, exposure(population) irr
poisson deaths north male year xnorthyear if midage>=17.5 & midage<37.5 & year>=1994 & year<=2008, exposure(population) irr
poisson deaths north male year xnorthyear if midage>=17.5 & midage<37.5 & year>2008 & year<=2015, exposure(population) irr
poisson deaths north male year xnorthyear if midage>=17.5 & midage<37.5 & year>=2016 & year<=2020, exposure(population) irr
poisson deaths north male year xnorthyear if midage>=17.5 & midage<37.5 & year>=2021 & year<=2024, exposure(population) irr
poisson deaths north male _Imidage_7 _Imidage_8 if midage>17.5 & midage<37.5 & year==2024, exposure(population) irr

//WORKSHEET "35-49 by sex"
local str0 "Females"
local str1 "Males"
forvalues sx=0(1)1 {
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Overall by year for `str`sx'' 35-49"
			di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
		}
		qui poisson deaths north _Imidage_10 _Imidage_11 if male==`sx' & midage>32.5 & midage<52.5 & year==`yr', exposure(population) irr
		di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
	}
}
poisson deaths north male year xnorthyear if midage>=32.5 & midage<52.5 & year>=1981 & year <=1993, exposure(population) irr
poisson deaths north male year xnorthyear if midage>=32.5 & midage<52.5 & year>=1994 & year <=2008, exposure(population) irr
poisson deaths north male year xnorthyear if midage>=32.5 & midage<52.5 & year>=2009 & year <=2015, exposure(population) irr
poisson deaths north male year xnorthyear if midage>=32.5 & midage<52.5 & year>=2016 & year <=2020, exposure(population) irr
poisson deaths north male year xnorthyear if midage>=32.5 & midage<52.5 & year>=2021 & year <=2024, exposure(population) irr
poisson deaths north male _Imidage_8-_Imidage_10 if midage>=32.5 & midage<52.5 & year==2024, exposure(population) irr
poisson deaths north male xmalenorth if midage>=32.5 & midage<52.5 & year>=1965 & year <=1990, exposure(population) irr
poisson deaths north male xmalenorth _Imidage_8-_Imidage_10 if midage>=32.5 & midage<52.5 & year==2024, exposure(population) irr

//WORKSHEET "50-64 by sex"
local str0 "Females"
local str1 "Males"
forvalues sx=0(1)1 {
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Overall by year for `str`sx'' 50-64"
			di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
		}
		qui poisson deaths north _Imidage_13 _Imidage_14 if male==`sx' & midage>47.5 & midage<67.5 & year==`yr', exposure(population) irr
		di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
	}
}
poisson deaths north _Imidage_13 _Imidage_14 if year==1981 & midage>=47.5 & midage<67.5 & male==1, exposure(population) irr
poisson deaths north _Imidage_13 _Imidage_14 if year==2024 & midage>47.5 & midage<67.5 & male==1, exposure(population) irr
capture drop xmaleyear
gen xmaleyear=male*year
xi: poisson deaths north male year xmaleyear i.midage if midage>47.5 & midage<67.5, exposure(population) irr
xi: poisson deaths north male year xnorthyear i.midage if midage>47.5 & midage<67.5 & year>=1965 & year<1990, exposure(population) irr
poisson deaths  north year xnorthyear _Imidage_13 _Imidage_14 if year<=2008 & midage>=47.5 & midage<67.5 & male==1, exposure(population) irr
poisson deaths  north year xnorthyear _Imidage_13 _Imidage_14 if midage>=47.5 & midage<67.5 & male==1, exposure(population) irr

//WORKSHEET "65-74 by sex"
local str0 "Females"
local str1 "Males"
forvalues sx=0(1)1 {
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Overall by year for `str`sx'' 65-74"
			di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
		}
		qui poisson deaths north _Imidage_16 if male==`sx' & midage>62.5 & midage<77.5 & year==`yr', exposure(population) irr
		di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
	}
}
poisson deaths north _Imidage_16 if year==1981 & midage>=62.5 & midage<77.5 & male==1, exposure(population) irr
poisson deaths north _Imidage_16 if year==2024 & midage>62.5 & midage<77.5 & male==1, exposure(population) irr
xi: poisson deaths north  male  year xnorthyear i.midage if midage>62.5 & midage<77.5 & male==1, exposure(population) irr
poisson deaths north _Imidage_16 male if year==1981 & midage>=62.5 & midage<77.5, exposure(population) irr
poisson deaths north _Imidage_16 male if year==2024 & midage>62.5 & midage<77.5, exposure(population) irr

//WORKSHEET "45-59 by sex"
local str0 "Females"
local str1 "Males"
forvalues sx=0(1)1 {
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Overall by year for `str`sx'' 45-59"
			di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
		}
		qui poisson deaths north _Imidage_12 _Imidage_13 if male==`sx' & midage>=45 & midage<60 & year==`yr', exposure(population) irr
		di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
	}
}
xi: poisson deaths north year xmalenorth male i.midage if year>=1981 & year <=1990 & midage>=45 & midage<60, exposure(population) irr
poisson deaths north _Imidage_12 _Imidage_13 if year==2024 & midage>=45 & midage<60, exposure(population) irr

//WORKSHEET "35-74 by sex"
local str0 "Females"
local str1 "Males"
forvalues sx=0(1)1 {
	forvalues yr=1981(1)2024 {
		if `yr'==1981 {
			di as text _newline(2) "Overall by year for `str`sx'' 35-74"
			di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
		}
		qui poisson deaths north _Imidage_10 _Imidage_11 _Imidage_12 _Imidage_13 _Imidage_14 _Imidage_15 _Imidage_16 if male==`sx' & midage>=35 & midage<75 & year==`yr', exposure(population) irr
		di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
	}
}
poisson deaths north _Imidage_10 _Imidage_11 _Imidage_12 _Imidage_13 _Imidage_14 _Imidage_15 _Imidage_16 if year==1981 & midage>=35 & midage<75, exposure(population) irr
poisson deaths north _Imidage_10 _Imidage_11 _Imidage_12 _Imidage_13 _Imidage_14 _Imidage_15 _Imidage_16 if year==2024 & midage>=35 & midage<75, exposure(population) irr
xi: poisson deaths north male year xnorthyear i.midage if midage>=35 & midage<75, exposure(population) irr

//additional requested by Iain
//NEW WORKSHEET "Age Band North Excess 1981-1990"
//loops for gender and midage
foreach x in 0 1 {
	if `x'==0 {
		display _newline(1) as text "Female (1981-90)"
	}
	else {
		display _newline(1) as text "Male (1981-90)"
	}
	di as text _col(5) "midage" _col(15) "IRR" _col(30) "se"
	forvalues y=1(1)`=wordcount("`midagelst'")' {
		local lmage = word("`midagelst'",`y')
		qui poisson deaths north if midage==`lmage' & male==`x' & year>=1981 & year <=1990, exposure(population)
		//display
		di as result _col(5) `lmage' _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'		
	}
}
//NEW WORKSHEET "Age Band North Excess 1991-2000"
//loops for gender and midage
foreach x in 0 1 {
	if `x'==0 {
		display _newline(1) as text "Female (1991-00)"
	}
	else {
		display _newline(1) as text "Male (1991-00)"
	}
	di as text _col(5) "midage" _col(15) "IRR" _col(30) "se"
	forvalues y=1(1)`=wordcount("`midagelst'")' {
		local lmage = word("`midagelst'",`y')
		qui poisson deaths north if midage==`lmage' & male==`x' & year>=1991 & year <=2000, exposure(population)
		//display
		di as result _col(5) `lmage' _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'		
	}
}
//NEW WORKSHEET "Age Band North Excess 2001-2010"
//loops for gender and midage
foreach x in 0 1 {
	if `x'==0 {
		display _newline(1) as text "Female (2001-10)"
	}
	else {
		display _newline(1) as text "Male (2001-10)"
	}
	di as text _col(5) "midage" _col(15) "IRR" _col(30) "se"
	forvalues y=1(1)`=wordcount("`midagelst'")' {
		local lmage = word("`midagelst'",`y')
		qui poisson deaths north if midage==`lmage' & male==`x' & year>=2001 & year <=2010, exposure(population)
		//display
		di as result _col(5) `lmage' _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'		
	}
}
//NEW WORKSHEET "Age Band North Excess 2011-2020"
//loops for gender and midage
foreach x in 0 1 {
	if `x'==0 {
		display _newline(1) as text "Female (2011-20)"
	}
	else {
		display _newline(1) as text "Male (2011-20)"
	}
	di as text _col(5) "midage" _col(15) "IRR" _col(30) "se"
	forvalues y=1(1)`=wordcount("`midagelst'")' {
		local lmage = word("`midagelst'",`y')
		qui poisson deaths north if midage==`lmage' & male==`x' & year>=2011 & year <=2020, exposure(population)
		//display
		di as result _col(5) `lmage' _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'		
	}
}
//NEW WORKSHEET "Age Band North Excess 2021-2024"
//loops for gender and midage
foreach x in 0 1 {
	if `x'==0 {
		display _newline(1) as text "Female (2021-24)"
	}
	else {
		display _newline(1) as text "Male (2021-24)"
	}
	di as text _col(5) "midage" _col(15) "IRR" _col(30) "se"
	forvalues y=1(1)`=wordcount("`midagelst'")' {
		local lmage = word("`midagelst'",`y')
		qui poisson deaths north if midage==`lmage' & male==`x' & year>=2021 & year <=2024, exposure(population)
		//display
		di as result _col(5) `lmage' _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'		
	}
}
//NEW WORKSHEET "25-34"
forvalues yr=1981(1)2024 {
	if `yr'==1981 {
		di as text _newline(2) "Overall by year 25-34"
		di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
	}
	qui poisson deaths north male _Imidage_7 _Imidage_8 if year==`yr' & midage>22.5 & midage<37.5, exposure(population) irr
	di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
}
//NEW WORKSHEET "35-44"
forvalues yr=1981(1)2024 {
	if `yr'==1981 {
		di as text _newline(2) "Overall by year 35-44"
		di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
	}
	qui poisson deaths north male _Imidage_9 _Imidage_10 if year==`yr' & midage>32.5 & midage<47.5, exposure(population) irr
	di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
}
//NEW WORKSHEET "45-54"
forvalues yr=1981(1)2024 {
	if `yr'==1981 {
		di as text _newline(2) "Overall by year 45-54"
		di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
	}
	qui poisson deaths north male _Imidage_11 _Imidage_12 if year==`yr' & midage>42.5 & midage<57.5, exposure(population) irr
	di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
}
//NEW WORKSHEET "25-54"
forvalues yr=1981(1)2024 {
	if `yr'==1981 {
		di as text _newline(2) "Overall by year 25-54"
		di as text _col(5) "Year" _col(15) "IRR" _col(30) "se"
	}
	qui poisson deaths north male _Imidage_7 _Imidage_8 _Imidage_9 _Imidage_10 _Imidage_11 _Imidage_12 if year==`yr' & midage>22.5 & midage<57.5, exposure(population) irr
	di as result _col(5) "`yr'" _col(15) `=exp(_b[north])' _col(30) `=exp(_b[north])*_se[north]'
}


qui log close _all
qui log2html "`dir3'log2-north_excess", replace ti("North excess deaths analyses")	
