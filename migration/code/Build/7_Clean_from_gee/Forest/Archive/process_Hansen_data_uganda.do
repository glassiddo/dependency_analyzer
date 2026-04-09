clear
set more off

disp c(username)

if c(username) == "liamw"{
	global dir = "D:\Dropbox\Liam\"
}
	
global subdir = "$dir/Migration Africa/data/Hansen Forest Loss"
global dirinput = "$subdir/Input"
global dirtemp = "$subdir/Temp"
global diroutput = "$subdir/Output"
cd  "$subdir/Code"

import delimited "$dirinput/area_uganda.csv", stringcols(2) encoding(ISO-8859-1) clear

keep v* d* t* sum

*collapse (sum) sum , by(c_code01)

ren sum area

compress

saveold "$dirtemp/area_uganda.dta", replace


*treecover
import delimited "$dirinput/treecover_uganda.csv", stringcols(2) encoding(ISO-8859-1) clear

keep v* d* t* sum 

*gcollapse (sum) sum , by(c_code01)

ren sum treecover

compress

saveold "$diroutput/treecover_uganda.dta", replace


use "$diroutput/treecover_uganda.dta", clear

merge 1:1 v* d* *total* using "$dirtemp/area_uganda.dta", nogen

erase "$dirtemp/area_uganda.dta"
save "$diroutput/treecover_uganda.dta", replace





*yearly loss

import delimited "$dirinput/units_lossXcoverbyyear_uganda.csv", stringcols(2) encoding(ISO-8859-1) clear

split groups , p("},")

keep v* d* *total* groups*
drop groups
forval y=1/20 {
	replace groups`y'=subinstr(groups`y',"{group=","",.)
	replace groups`y'=subinstr(groups`y',", sum="," ",.)
	replace groups`y'=subinstr(groups`y',"}","",.)
	replace groups`y'=subinstr(groups`y',"]","",.)
	replace groups`y'=subinstr(groups`y',"[","",.)
}

forval y=1/20 {
	gen year`y'=word(groups`y',1)
	gen loss`y'=word(groups`y',2)
}

forval y=1/20 {
	drop groups`y'
	destring year`y', replace
	destring loss`y', replace
	
}
forval y=1/20 {
	local year=2000+`y'
	gen loss_`year'=0
	forval v=1/20 {
		disp "y=`y', v=`v'"
		replace loss_`year'=loss_`year'+loss`v' if year`v'==`y' & loss`v'!=.
	}
}

keep v* d* t* loss_*

*gcollapse (sum) loss_*, by(c_code01) fast

fastreshape long loss_, i(v2 v3 v4 v5 v6 d dname2019 total2020 ttotal2020) j(year)

compress

merge m:1 v2 v3 v4 v5 v6 d dname2019 total2020 ttotal2020 using "$diroutput/treecover_uganda.dta", nogen

saveold "$diroutput/lossbyyear_uganda.dta", replace

