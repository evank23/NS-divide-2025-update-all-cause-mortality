local dir1 "C:/Users/`c(username)'/The University of Manchester Dropbox/Evangelos Kontopantelis/zP_drive/Evan/Informatics/North-South divide/2025_all-causes/"
local dir2 "`dir1'data/"
cd "`dir1'"
set more off

//get numerators
import excel "`dir2'Copy of deathsnorthsouthdivideengland1981to2024.xlsx", sheet("All causes") cellrange(A4:CM43) firstrow clear
rename Agegroup agegrp
drop if agegrp==""
sencode agegrp, replace
replace agegrp=agegrp-1
label drop agegrp
rename Area area
sencode area, replace
replace area=1 in 1/19
replace area=2 in 20/38
drop AU
local yr=1980
foreach x of varlist C-AT {
	local yr=`yr'+1
	rename `x' m`yr'
}
local yr=1980
foreach x of varlist AV-CM {
	local yr=`yr'+1
	rename `x' f`yr'
}
reshape long m f, i(agegrp area) j(year)
rename m deaths2
rename f deaths1
reshape long deaths, i(agegrp area year) j(sex)
tempfile tempf
save `tempf', replace
//denominators
import delimited "`dir2'population_estimates_1981_2024.csv", clear
rename agegroup agegrp
sencode agegrp, replace
label drop agegrp
replace agegrp=0 if agegrp==19
sencode area, replace
sencode sex, replace
merge 1:1 area sex agegrp year using `tempf', nogen
//align to previous analyses
label define agegrp 0 "<01" 1 "01-04" 2 "05-09" 3 "10-14" 4 "15-19" 5 "20-24" 6 "25-29" 7 "30-34" 8 "35-39" 9 "40-44" 10 "45-49" 11 "50-54" 12 "55-59" 13 "60-64" 14 "65-69" 15 "70-74" 16 "75-79" 17 "80-84" 18 "85+"  
label val agegrp agegrp
rename agegrp agecat
rename sex male
replace male=male-1
label var male "Male"
label drop sex
rename area north
replace north=0 if north==2
label var north "North"
label drop area
label var agecat "Age category"
label var deaths "All cause deaths"
decode agecat, gen(ageband)
qui replace ageband="<01" if ageband=="<1"
qui gen midage=0.5
replace midage=3 if agecat==1
replace midage=agecat*5-2.5 if agecat>1

order ageband agecat midage year deaths population north male
//sort and save
sort north male year agecat
qui drop if agecat==.
save "`dir2'All_cause_dataset.dta", replace

