clear all
local dir1 "C:/Users/`c(username)'/The University of Manchester Dropbox/Evangelos Kontopantelis/zP_drive/Evan/Informatics/North-South divide/2025_all-causes/"
local dir2 "`dir1'/data/"
local dir3 "`dir1'/shared/"
local dir4 "`dir3'/graphs/"
local dir5 "`dir3'/export/"
cd "`dir1'"
set more off
use "`dir2'All_cause_dataset.dta", clear
preserve 
qui do "03-changepoint_england.do"
qui do "04-changepoint_byregion.do"
qui do "05-IRRplots_byage.do"
qui do "07-IRRplots_overtime.do"
qui do "12-Poisson_england.do"
qui do "13-Poisson_byregion.do"
qui do "06-plots_SMR.do"

//POISSON
//All ages
restore, preserve
PoissonEng "13" "_allages" "all ages"
restore, preserve
Poissonbyreg "14" "_allages" "all ages"
//by gender
restore, preserve
qui keep if male==1
PoissonEng "13" "_allages_males" "males all ages"
restore, preserve
qui keep if male==1
Poissonbyreg "14" "_allages_males" "males all ages"
restore, preserve
qui keep if male==0
PoissonEng "13" "_allages_females" "females all ages"
restore, preserve
qui keep if male==0
Poissonbyreg "14" "_allages_females" "females all ages"

//25-49
restore, preserve
qui keep if midage>=25 & midage<=50
PoissonEng "15" "_25to49" "aged 25-49"
restore, preserve
qui keep if midage>=25 & midage<=50
Poissonbyreg "16" "_25to49" "aged 25-49"
//by gender
restore, preserve
qui keep if male==1
qui keep if midage>=25 & midage<=50
PoissonEng "15" "_25to49_males" "males aged 25-49"
restore, preserve
qui keep if male==1
qui keep if midage>=25 & midage<=50
Poissonbyreg "16" "_25to49_males" "males aged 25-49"
restore, preserve
qui keep if male==0
qui keep if midage>=25 & midage<=50
PoissonEng "15" "_25to49_females" "females aged 25-49"
restore, preserve
qui keep if male==0
qui keep if midage>=25 & midage<=50
Poissonbyreg "16" "_25to49_females" "females aged 25-49"

//Under 75
restore, preserve
qui drop if midage>75
PoissonEng "17" "_under75" "aged under 75"
restore, preserve
qui drop if midage>75
Poissonbyreg "18" "_under75" "aged under 75"
//by gender
restore, preserve
qui keep if male==1
qui drop if midage>75
PoissonEng "17" "_under75_males" "males aged under 75"
restore, preserve
qui keep if male==1
qui drop if midage>75
Poissonbyreg "18" "_under75_males" "males aged under 75"
restore, preserve
qui keep if male==0
qui drop if midage>75
PoissonEng "17" "_under75_females" "females aged under 75"
restore, preserve
qui keep if male==0
qui drop if midage>75
Poissonbyreg "18" "_under75_females" "females aged under 75"

//0-24
restore, preserve
qui keep if midage<=25
PoissonEng "19" "_under25" "aged 0-24"
restore, preserve
qui keep if midage<=25
Poissonbyreg "20" "_under25" "aged 0-24"
//by gender
restore, preserve
qui keep if male==1
qui keep if midage<=25
PoissonEng "19" "_under25_males" "males aged 0-24"
restore, preserve
qui keep if male==1
qui keep if midage<=25
Poissonbyreg "20" "_under25_males" "males aged 0-24"
restore, preserve
qui keep if male==0
qui keep if midage<=25
PoissonEng "19" "_under25_females" "females aged 0-24"
restore, preserve
qui keep if male==0
qui keep if midage<=25
Poissonbyreg "20" "_under25_females" "females aged 0-24"

//All ages
restore, preserve
chngengland "3" "_allages" "all ages"
restore, preserve
chngbyregion "4" "_allages" "all ages"
restore, preserve
//by gender
qui keep if male==1
chngengland "3" "_allages_males" "males all ages"
restore, preserve
qui keep if male==1
chngbyregion "4" "_allages_males" "males all ages"
restore, preserve
qui keep if male==0
chngengland "3" "_allages_females" "females all ages"
restore, preserve
qui keep if male==0
chngbyregion "4" "_allages_females" "females all ages"

//25-49
restore, preserve
qui keep if midage>=25 & midage<=50
chngengland "11" "_25to49" "aged 25-49"
restore, preserve
qui keep if midage>=25 & midage<=50
chngbyregion "12" "_25to49" "aged 25-49"
restore, preserve
//by gender
qui keep if male==1
qui keep if midage>=25 & midage<=50
chngengland "11" "_25to49_males" "males aged 25-49"
restore, preserve
qui keep if male==1
qui keep if midage>=25 & midage<=50
chngbyregion "12" "_25to49_males" "males aged 25-49"
restore, preserve
qui keep if male==0
qui keep if midage>=25 & midage<=50
chngengland "11" "_25to49_females" "females aged 25-49"
restore, preserve
qui keep if male==0
qui keep if midage>=25 & midage<=50
chngbyregion "12" "_25to49_females" "females aged 25-49"

//0-24
restore, preserve
qui keep if midage<25
chngengland "13" "_0to24" "aged 0-24"
restore, preserve
qui keep if midage<25
chngbyregion "14" "_0to24" "aged 0-24"
restore, preserve
//by gender
qui keep if male==1
qui keep if midage<25
chngengland "13" "_0to24_males" "males aged 0-24"
restore, preserve
qui keep if male==1
qui keep if midage<25
chngbyregion "14" "_0to24_males" "males aged 0-24"
restore, preserve
qui keep if male==0
qui keep if midage<25
chngengland "13" "_0to24_females" "females aged 0-24"
restore, preserve
qui keep if male==0
qui keep if midage<25
chngbyregion "14" "_0to24_females" "females aged 0-24"

//Under 75
restore, preserve
qui drop if midage>75
chngengland "5" "_under75" "aged under 75"
restore, preserve
qui drop if midage>75
chngbyregion "6" "_under75" "aged under 75"
restore, preserve
//by gender
qui keep if male==1
qui drop if midage>75
chngengland "5" "_under75_males" "males aged under 75"
restore, preserve
qui keep if male==1
qui drop if midage>75
chngbyregion "6" "_under75_males" "males aged under 75"
restore, preserve
qui keep if male==0
qui drop if midage>75
chngengland "5" "_under75_females" "females aged under 75"
restore, preserve
qui keep if male==0
qui drop if midage>75
chngbyregion "6" "_under75_females" "females aged under 75"
restore, preserve

//IRR graphs over time
plotsyear 1 16
plotsyear 1 19
plotsyear 7 11
plotsyear 1 6
plotsyear 12 19

//IRR graphs by year
plotsagegrp 1981 1989
plotsagegrp 1990 1999
plotsagegrp 2000 2009
plotsagegrp 2010 2019
plotsagegrp 2020 2024
plotsagegrp2 
plotsagegrp2 "male"
plotsagegrp2 "female"

//SMR plots
plotsmr
forvalues i=1(1)7 {
	plotsmr "`i'"
}


//SMT changepoint analyses
qui do "09-SMRchangepoint_england.do"
qui do "10-SMRchangepoint_byregion.do"
//SMR analyses necessarily for all ages and both sexes
use "`dir5'SMRdata_all1.dta", clear
SMRchngeng "7" "_allages" "all ages"
use "`dir5'SMRdata_all2.dta", clear
SMRchngbyregion "8" "_allages" "all ages"

//plot some descriptives
qui do "11-descr_stats.do"
plotdescr

//paper stats
qui do "14-paper_stats.do"
qui do "15-paper_stats_too.do"
stats1
stats2

//contour plots
qui do "16-contour_plots.do"

//sens nbreg
qui do "17-nbr_sens.do"

