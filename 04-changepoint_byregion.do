capture program drop chngbyregion
program chngbyregion
	local dir1 "C:/Users/`c(username)'/The University of Manchester Dropbox/Evangelos Kontopantelis/zP_drive/Evan/Informatics/North-South divide/2025_all-causes/"
	local dir2 "`dir1'/data/"
	local dir3 "`dir1'/shared/"
	local dir4 "`dir3'/graphs/"
	local dir5 "`dir3'/export/"
	capture mkdir "`dir3'"
	capture mkdir "`dir4'"
	capture mkdir "`dir5'"
	cd "`dir1'"
	*use "`dir2'All_cause_dataset.dta", clear
	set more off
	
	capture log close _all
	local pfix="`1'"
	local efix="`2'"
	local titl="`3'"
	qui log using "`dir3'log`pfix'-changepoint_byregion`efix'", name(log1) smcl replace
	local mcmcs=100000
	local buins=10000

	//info
	*http://www.stata.com/new-in-stata/structural-breaks/ 
	*http://www.stata.com/stata-news/news30-1/bayesian-analysis/

	//gen some needed variables
	qui {
		collapse (sum) deaths population, by(year north)
		gen drate=deaths/population*10000
		gen sterror=sqrt(drate*(10000-drate)/population)
		qui gen period1=1 if year<1990
		qui gen period2=1 if year>=1990 & year<2000
		qui gen period3=1 if year>=2000 & year<2010
		qui gen period4=1 if year>=2010 & year<2020
		qui gen period5=1 if year>=2020
		forvalues i=1(1)5{
			qui replace period`i'=0 if period`i'==.
		}
		qui gen tperiod=.
		forvalues i=1(1)5 {
			replace tperiod=`i' if period`i'==1
		}
		tempvar tempv
		gen `tempv'=drate[_n-1]
		gen drtchng=drate-`tempv'
		qui gen chng1=year-1989
		qui gen chng2=year-1999
		qui gen chng3=year-2009
		qui gen chng4=year-2019	
		forvalues i=1(1)4 {
			replace chng`i'=0 if chng`i'<0
		}
		forvalues i=1(1)4 {
			gen northXperiod`i' = north*period`i'
		}
		gen northXyear=north*year
		gen northXchng2=north*chng2
		gen northXchng3=north*chng3
		gen northXchng4=north*chng4		
	}

	//smoothed plots
	lpoly drate year if north==0, gen(at sdrate0) se(ssterror0) degree(0) at(year) ci nograph
	qui drop at
	lpoly drate year if north==1, gen(at sdrate1) se(ssterror1) degree(0) at(year) ci nograph
	qui drop at
	forvalues i=0(1)1 {
		gen l95CI`i'=sdrate`i'-1.96*ssterror`i'
		gen u95CI`i'=sdrate`i'+1.96*ssterror`i'
	}
	//to shade between xlines
	qui sum u95CI1
	local max=r(max)
	gen c1=`max' if (year<=1981)
	gen c2=`max' if (year>=1990 & year<=1991)	
	gen c3=`max' if (year>=2008 & year<=2009)
	gen c4=`max' if (year>=2020 & year<=2022)	
	//death rate plots with CI	
	//overall
	graph twoway area c1 year, bcolor(sandb%30) || area c2 year, bcolor(sandb%30) /*
	*/ || area c3 year, bcolor(sandb%30) || area c4 year, bcolor(sandb%30) /*		
	*/ || rarea l95CI0 u95CI0 year if north==0, color(gs14) || line sdrate0 year if north==0, lcolor(blue) || scatter drate year if north==0, color(maroon) /*
	*/ || rarea l95CI1 u95CI1 year if north==1, color(gs14) || line sdrate1 year if north==1, lcolor(blue) || scatter drate year if north==1, color(maroon) /*
	*/ legend(order(6 "South(`titl')" 9 "North(`titl')" 5 "95% CI" 1 "Stress periods")) /*
	*/ ytitle("Crude mortality rate (per 10,000)") /*
	*/ xtitle("Year") /*	
	*/ ylabel(, format(%3.1f) labsize(2.5) angle(horizontal)) /*
	*/ xlabel(1981 1985 1990 1995 2000 2005 2010 2015 2020 2024, labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4) /*
	*/ plotregion(margin(tiny)) ylabel(,nogextend)
	*graph save "`dir4'/`pfix'-drates_smooth_byregion`efix'.gph", replace
	graph export "`dir4'/`pfix'-drates_smooth_byregion`efix'.png", width(1200) replace
	drop c1 c2 c3 c4 

	//raw plots with CI
	gen l95CI=drate-1.96*sterror
	gen u95CI=drate+1.96*sterror
	//to shade between xlines
	qui sum u95CI
	local max=r(max)
	gen c1=`max' if (year<=1981)
	gen c2=`max' if (year>=1990 & year<=1991)	
	gen c3=`max' if (year>=2008 & year<=2009)
	gen c4=`max' if (year>=2020 & year<=2022)		
	graph twoway area c1 year, bcolor(sandb%30) || area c2 year, bcolor(sandb%30) /*
	*/ || area c3 year, bcolor(sandb%30) || area c4 year, bcolor(sandb%30) /*			
	*/ || rarea l95CI u95CI year if north==0, color(gs14) || rarea l95CI u95CI year if north==1, color(gs14) /*
	*/ || line drate year if north==0, lcolor(blue) || line drate year if north==1, lcolor(maroon) /*	
	*/ legend(order(7 "South(`titl')" 8 "North(`titl')" 5 "95% CI" 1 "Stress periods")) /*
	*/ ytitle("Crude mortality rate (per 10,000)") /*
	*/ xtitle("Year") /*	
	*/ ylabel(, format(%3.1f) labsize(2.5) angle(horizontal)) /*
	*/ xlabel(1981 1985 1990 1995 2000 2005 2010 2015 2020 2024, labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4) /*
	*/ plotregion(margin(tiny)) ylabel(,nogextend)
	*graph save "`dir4'/`pfix'-drates_raw_byregion`efix'.gph", replace
	graph export "`dir4'/`pfix'-drates_raw_byregion`efix'.png", width(1200) replace
	window manage close graph
	preserve
	qui keep year north	deaths population drate	sterror	l95CI u95CI
	qui export excel "`dir5'graphexport.xlsx", firstrow(variables) sheet("`efix'") sheetreplace
	restore

	//FREQUENTIST
	preserve 
	forvalues i=0(1)1 {
		qui keep if north==`i'
		tsset year
		regress drate year, vce(robust)
		estat sbsingle
		estat sbknown, break(2020)
		restore, preserve
	}
	//MODEL 1 - comparing levels in 5 different periods: pre-1990, 1990-1999, 2000-2009, 2010-2019, 2020+
	//with interaction term for north
	regress drate i.north i.tperiod i.north#i.tperiod
	margins i.north#i.tperiod
	//MODEL 2 - year trend
	regress drate i.north year i.north#c.year
	margins i.north, at(year=(1981 1990 1995 2000 2005 2010 2015 2020 2024))
	//MODEL 3 - year trend and change in slope for 2020-2024 compared to 2010-2019
	di _newline(2) as text "MODEL 3 - year trend and change in slope for 2020-2024 compared to 2010-2019"
	regress drate i.north year chng4 i.north#c.year i.north#c.chng4 if year>=2010 & year<=2024
	margins i.north, at(year=(1981 1985 1990 1995 2000 2005))
	margins i.north, at(year=(2010)) at(year=(2011)) at(year=(2012)) /*
	*/ at(year=(2013)) at(year=(2014)) at(year=(2015)) at(year=(2016)) /*
	*/ at(year=(2017)) at(year=(2018)) at(year=(2019)) at(year=(2020) chng4=(1)) /*
	*/ at(year=(2021) chng4=(2)) at(year=(2022) chng4=(3)) /*
	*/ at(year=(2023) chng4=(4)) at(year=(2024) chng4=(5))
	
	sort north year
	list year north drate, sepby(north)
	
	qui log close _all
	qui log2html "`dir3'log`pfix'-changepoint_byregion`efix'", replace ti("Change-point analyses - North vs South `titl'")
end	
