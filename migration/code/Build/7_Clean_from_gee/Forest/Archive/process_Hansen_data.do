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

import delimited "$dirinput/area_.csv", stringcols(2) encoding(ISO-8859-1) clear

keep adm* sum

*collapse (sum) sum , by(c_code01)

ren sum area

compress

saveold "$dirtemp/area.dta", replace


*treecover
import delimited "$dirinput/treecover_.csv", stringcols(2) encoding(ISO-8859-1) clear

keep adm* sum 

*gcollapse (sum) sum , by(c_code01)

ren sum treecover

compress

saveold "$diroutput/treecover.dta", replace


use "$diroutput/treecover.dta", clear

merge 1:1 adm* using "$dirtemp/area.dta", nogen

erase "$dirtemp/area.dta"
save "$diroutput/treecover.dta", replace





*yearly loss

import delimited "$dirinput/units_lossXcoverbyyear.csv", stringcols(2) encoding(ISO-8859-1) clear

split groups , p("},")

keep adm* groups*
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

keep adm* loss_*

*gcollapse (sum) loss_*, by(c_code01) fast

fastreshape long loss_, i(adm0_code adm1_code adm2_code adm0_name adm1_name adm2_name) j(year)

compress

merge m:1 adm* using "$diroutput/treecover.dta", nogen

saveold "$diroutput/lossbyyear.dta", replace

