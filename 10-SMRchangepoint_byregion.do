program SMRchngbyregion
	local dir1 "C:/Users/`c(username)'/The University of Manchester Dropbox/Evangelos Kontopantelis/zP_drive/Evan/Informatics/North-South divide/2025_all-causes/"
	local dir2 "`dir1'/data/"
	local dir3 "`dir1'/shared/"
	local dir4 "`dir3'/graphs/"
	local dir5 "`dir3'/export/"
	capture mkdir "`dir3'"
	capture mkdir "`dir4'"
	capture mkdir "`dir5'"
	cd "`dir1'"
	set more off
	
	capture log close _all
	local pfix="`1'"
	local efix="`2'"
	local titl="`3'"
	qui log using "`dir3'log`pfix'-SMRchangepoint_byregion`efix'", name(log1) smcl replace
	local mcmcs=100000
	local buins=10000

	//info
	*http://www.stata.com/new-in-stata/structural-breaks/ 
	*http://www.stata.com/stata-news/news30-1/bayesian-analysis/

	//gen some needed variables
	qui {
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
		forvalues i=1(1)4 {
			gen northXperiod`i' = north*period`i'
		}
		gen northXyear=north*year
		gen northXchng2=north*chng2
		gen northXchng3=north*chng3
		gen northXchng4=north*chng4		
	}

	//FREQUENTIST
	preserve 
	forvalues i=0(1)1 {
		qui keep if north==`i'
		tsset year
		regress SMR year, vce(robust)
		estat sbsingle
		estat sbknown, break(2020)
		restore, preserve
	}
	//MODEL 1 - comparing levels in 5 different periods: pre-1990, 1990-1999, 2000-2009, 2010-2019, 2020+
	//with interaction term for north
	regress SMR i.north i.tperiod i.north#i.tperiod
	margins i.north#i.tperiod
	//MODEL 2 - year trend
	regress SMR i.north year i.north#c.year
	margins i.north, at(year=(1981 1990 1995 2000 2005 2010 2015 2020 2024))
	//MODEL 3 - year trend and change in slope for 2020-2024 compared to 2010-2019
	di _newline(2) as text "MODEL 3 - year trend and change in slope for 2020-2024 compared to 2010-2019"
	regress SMR i.north year chng4 i.north#c.year i.north#c.chng4 if year>=2010 & year<=2024
	margins i.north, at(year=(1981 1985 1990 1995 2000 2005))
	margins i.north, at(year=(2010)) at(year=(2011)) at(year=(2012)) /*
	*/ at(year=(2013)) at(year=(2014)) at(year=(2015)) at(year=(2016)) /*
	*/ at(year=(2017)) at(year=(2018)) at(year=(2019)) at(year=(2020) chng4=(1)) /*
	*/ at(year=(2021) chng4=(2)) at(year=(2022) chng4=(3)) /*
	*/ at(year=(2023) chng4=(4)) at(year=(2024) chng4=(5))

	qui log close _all
	qui log2html "`dir3'log`pfix'-SMRchangepoint_byregion`efix'", replace ti("SMR change-point analyses - North vs South `titl'")	
end	
