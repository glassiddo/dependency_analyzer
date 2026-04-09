clear
set more off

disp c(username)

if c(username) == "liamw"{
	global dir = "D:\Dropbox\Liam\"
}


if c(username) == "Liam"{
	global dir = "C:\Users\Liam\Dropbox\Liam\"
}

	
global subdir = "$dir/Migration Africa/data/Hansen Forest Loss"
global dirinput = "$subdir/Input"
global dirtemp = "$subdir/Temp"
global diroutput = "$subdir/Output"
cd  "$subdir/Code"

import delimited "$dirinput/area_SA.csv", stringcols(2) encoding(ISO-8859-1) clear

keep mn* dc* pr* sum

*collapse (sum) sum , by(c_code01)

ren sum area

compress

saveold "$dirtemp/area_SA.dta", replace


*treecover
import delimited "$dirinput/treecover_SA.csv", stringcols(2) encoding(ISO-8859-1) clear

keep mn* dc* pr* sum 

*gcollapse (sum) sum , by(c_code01)

ren sum treecover

compress

saveold "$diroutput/treecover_SA.dta", replace


use "$diroutput/treecover_SA.dta", clear

merge 1:1 mn* dc* pr*  using "$dirtemp/area_SA.dta", nogen

erase "$dirtemp/area_SA.dta"
save "$diroutput/treecover_SA.dta", replace





*yearly loss

import delimited "$dirinput/units_lossXcoverbyyear_SA.csv", stringcols(2) encoding(ISO-8859-1) clear

split groups , p("},")

keep mn* dc* pr*  groups*
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

keep mn* dc* pr* loss_*

*gcollapse (sum) loss_*, by(c_code01) fast

fastreshape long loss_, i(mn_code mn_code_st mn_mdb_c mn_name mn_name_c) j(year)

compress

merge m:1 mn_code mn_code_st mn_mdb_c mn_name mn_name_c using "$diroutput/treecover_SA.dta", nogen

saveold "$diroutput/lossbyyear_SA.dta", replace

