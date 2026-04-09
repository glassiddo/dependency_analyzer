
foreach w in 1 2 {
import delimited "$projdir\data\Raw\Africa\Crop Area\IPUMSADMUNITCROPLAND.csv", clear

tostring geo_id, gen(ipums_id)
replace ipums_id="0"+ipums_id if cntry_name=="Botswana"
*** This to get start and end years 
if `w'==1 {
	merge 1:1 ipums_id using "${final}/forestloss.dta", nogen keep(match) keepusing(fl_*_yr Country)
}
else {
	merge 1:1 ipums_id using "${final}/forestloss_period2.dta", nogen keep(match) keepusing(fl_*_yr Country)
}
global gap 1

gen sharecrop_bl=sharcrop2003
gen start_year=2003
gen sharecrop_el=sharcrop2003
gen end_year=2003
foreach year in 2007 2011 2015 2019 {
	replace sharecrop_bl=sharcrop`year' if fl_start_yr>=`year'-${gap}
	replace start_year=`year' if fl_start_yr>=`year'-${gap}
 	replace sharecrop_el=sharcrop`year' if fl_end_yr>=`year'-${gap}
	replace end_year=`year' if fl_end_yr>=`year'-${gap}
}

gen delta_sharecrop=(sharecrop_el-sharecrop_bl)/(end_year-start_year)
*winsor2 delta_sharecrop, replace
gen gr_sharecrop=delta_sharecrop/sharecrop_bl
*replace delta_sharecrop=0 if cntry_name=="Cameroon" & delta_sharecrop==.
rename sharecrop_bl bl_sharecrop 
keep ipums_id delta_sharecrop gr_sharecrop bl_sharecrop
compress
save "$projdir\data\Final\sharecrop_w`w'.dta", replace

}
clear
append using "$projdir\data\Final\sharecrop_w1.dta" "$projdir\data\Final\sharecrop_w2.dta", gen(wave)

save "$projdir\data\Final\sharecrop_new.dta", replace