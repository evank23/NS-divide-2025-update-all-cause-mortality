capture program drop chngengland
program chngengland
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
	qui log using "`dir3'log`pfix'-changepoint_england`efix'", name(log1) smcl replace
	local mcmcs=100000
	local buins=10000

	//info
	*http://www.stata.com/new-in-stata/structural-breaks/ 
	*http://www.stata.com/stata-news/news30-1/bayesian-analysis/

	//gen some needed variables
	qui {
		collapse (sum) deaths population, by(year)
		gen drate=deaths/population*100
		gen sterror=sqrt(drate*(100-drate)/population)
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
	}

	//smoothed plots
	lpoly drate year, gen(at sdrate) se(ssterror) degree(0) ci /*nograph*/ at(year)
	qui drop at
	*graph save "`dir4'/`pfix'-drates_smooth_england`efix'.gph", replace
	graph export "`dir4'/`pfix'-drates_smooth_england`efix'.png", width(1200) replace
	lpoly drtchng year, gen(at sdrate2) se(ssterror2) degree(0) ci /*nograph*/ at(year)
	*graph save "`dir4'/`pfix'-drtchng_smooth_england`efix'.gph", replace
	graph export "`dir4'/`pfix'-drtchng_smooth_england`efix'.png", width(1200) replace
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
	//death rate plots with CI	
	//overall
	graph twoway area c1 year, bcolor(sandb%30) || area c2 year, bcolor(sandb%30) /*
	*/ || area c3 year, bcolor(sandb%30) || area c4 year, bcolor(sandb%30) /*	
	*/ || rarea l95CI u95CI year, color(gs14) || line drate year, lcolor(maroon) /*
	*/ legend(order(6 "England(`titl')" 5 "95% CI" 1 "Stress periods")) /*
	*/ ytitle("death rate") /*
	*/ xtitle("Year") /*	
	*/ ylabel(, format(%3.1f) labsize(2.5) angle(horizontal)) /*
	*/ xlabel(1981 1985 1990 1995 2000 2005 2010 2015 2020 2024, labsize(2.2) angle(45)) /*
	*/ xsize(6) ysize(4) /*
	*/ plotregion(margin(tiny)) ylabel(,nogextend)
	*graph save "`dir4'/`pfix'-drates_raw_england`efix'.gph", replace
	graph export "`dir4'/`pfix'-drates_raw_england`efix'.png", width(1200) replace
	window manage close graph

	//FREQUENTIST
	regress drate year, vce(robust)
	tsset year
	estat sbsingle
	estat sbknown, break(2020)
	//MODEL 1 - comparing levels in 4 different periods: pre-1990, 1990-1999, 2000-2009, 2010-2019, 2020+
	di _newline(2) as text "MODEL 1 - comparing levels in 4 different periods: pre-1990, 1990-1999, 2000-2009, 2010-2019, 2020+"
	regress drate i.tperiod
	margins i.tperiod
	//MODEL 2 - year trend
	di _newline(2) as text "MODEL 2 - year trend"
	regress drate year
	//MDOEL 3 - year trend and change in slope for 2020-2024 compared to 2010-2019
	di _newline(2) as text "MODEL 3 - year trend and change in slope for 2020-2024 compared to 2010-2019"
	regress drate year chng4 if year>=2010 & year<=2024
	
	qui log close _all
	qui log2html "`dir3'log`pfix'-changepoint_england`efix'", replace ti("Change-point analyses - England `titl'")
end
		