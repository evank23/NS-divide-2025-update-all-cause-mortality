capture program drop PoissonEng
program PoissonEng
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
	qui log using "`dir3'log`pfix'-Poisson_england`efix'", name(log1) smcl replace

	//info
	*http://www.stata.com/new-in-stata/structural-breaks/ 
	*http://www.stata.com/stata-news/news30-1/bayesian-analysis/

	//gen some needed variables
	qui {
		//collapse (sum) deaths population, by(year agecat midage male)
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
		qui gen chng1=year-1989
		qui gen chng2=year-1999
		qui gen chng3=year-2009
		qui gen chng4=year-2019		
		forvalues i=1(1)4 {
			replace chng`i'=0 if chng`i'<0
		}
	}

	//MODEL 1 - comparing levels in 5 different periods: pre-1990, 1990-1999, 2000-2009, 2010-2019, 2020+
	di _newline(2) as text "MODEL 1 - comparing levels in 5 different periods: pre-1990, 1990-1999, 2000-2009, 2010-2019, 2020+"
	poisson deaths i.north male i.agecat ib1.tperiod, exposure(population) irr
	//MODEL 2 - year trend
	di _newline(2) as text "MODEL 2 - year trend"
	poisson deaths i.north male i.agecat year, exposure(population) irr
	//MDOEL 3 - year trend and slope change from 2020 onwards, compared to 2010-2019
	di _newline(2) as text "MODEL 3 - year trend and slope change from 2020 onwards, compared to 2010-2019"	
	poisson deaths i.north male i.agecat year chng4 if year>=2010 & year<=2024, exposure(population) irr
	//change in slope across the whole time series
	di _newline(5) as text "year*" _col(10) "pre10" _col(20) "lo95%CI" _col(30) "up95%CI" _col(40) "post5" _col(50) "lo95%CI" _col(60) "up95%CI"
	forvalues yr=1981(1)2010 {
		local t1 = `yr'+9
		local t2 = `yr'+14
		capture drop chng*
		qui gen chng3=year-`t1'
		qui replace chng3=0 if chng3<0
		qui poisson deaths i.north male i.agecat year chng3 if year>=`yr' & year<=`t2', exposure(population) irr			
		di as result "`=`t1'+1'" _col(10) %6.4f exp(_b[year]) _col(20) %6.4f exp(_b[year]-invnormal(0.975)*_se[year]) _col(30) %6.4f exp(_b[year]+invnormal(0.975)*_se[year]) /*
		*/ _col(40) %6.4f exp(_b[chng3]) _col(50) %6.4f exp(_b[chng3]-invnormal(0.975)*_se[chng3]) _col(60) %6.4f exp(_b[chng3]+invnormal(0.975)*_se[chng3])

	}
	di as text "*year breaking point, reporting slope 10 years previous and slope change 5 years after"
	
	qui log close _all
	qui log2html "`dir3'log`pfix'-Poisson_england`efix'", replace ti("Poisson change-point analyses - England `titl'")
end
		