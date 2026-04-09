
/*
global alist ""
forval y=1995/2015 {
	foreach l in ESA ESA2 {
		import delimited "$projdir\data\Raw\Africa\ESA/`l'_LC_forest_`y'.csv", clear
	
		gen ESA_forest_unweighted=sum/count
		gen year=`y'
			
		*recast str1000 admin_name 
		compress
		drop if geolevel==. | geolevel==888888 | geolevel==888888888
		keep geolevel ESA* year
		save "$projdir\data\Raw\Africa\ESA\temp/`l'_LC_forest_`y'.dta", replace

		import delimited "$projdir\data\Raw\Africa\ESA/`l'_LC_w40plus_forest_`y'.csv", clear
	
		gen ESA_forest_w40plus=sum/count
		gen year=`y'
			
		*recast str1000 admin_name 
		compress
		drop if geolevel==. | geolevel==888888 | geolevel==888888888
		keep geolevel ESA* year
		save "$projdir\data\Raw\Africa\ESA\temp/`l'_LC_w40plus_forest_`y'.dta", replace

		
		import delimited "$projdir\data\Raw\Africa\ESA/`l'_LC_weighted_crop_`y'.csv", clear
	
		gen ESA_crop=sum/count
		gen year=`y'
			
		*recast str1000 admin_name 
		compress
		drop if geolevel==. | geolevel==888888 | geolevel==888888888
		keep geolevel ESA* year
		save "$projdir\data\Raw\Africa\ESA\temp/`l'_LC_weighted_crop_`y'.dta", replace
 
		import delimited "$projdir\data\Raw\Africa\ESA/`l'_LC_weighted_forest_`y'.csv", clear
		
		gen ESA_forest_weighted=sum/count
		gen year=`y'
		drop if geolevel==. | geolevel==888888 | geolevel==888888888
		*recast str1000 admin_name
		
	
		merge 1:1 year geolevel using "$projdir\data\Raw\Africa\ESA\temp/`l'_LC_forest_`y'.dta", nogen 	
		merge 1:1 year geolevel using "$projdir\data\Raw\Africa\ESA\temp/`l'_LC_w40plus_forest_`y'.dta", nogen 	
		merge 1:1 year geolevel using "$projdir\data\Raw\Africa\ESA\temp/`l'_LC_weighted_crop_`y'.dta", nogen 	
		
		*erase  "$projdir\data\Raw\Africa\ESA\temp/`l'_LC_forest_`y'.dta"
		keep geolevel ESA* year
		compress
		
		tostring geolevel, gen(ipums_id)
		drop geolevel
		
		save "$projdir\data\Raw\Africa\ESA\temp/`l'_LC_`y'.dta", replace
		global alist "$alist `l'_LC_`y'.dta"
	}
}


clear 

cd "$projdir\data\Raw\Africa\ESA\Temp\"
append using $alist

save "$projdir\data\Build\Africa\ESA\ESA_long", replace
*/

local w=1
use "$projdir\data\Build\Africa\ESA\ESA_long", clear

foreach var in ESA_forest_weighted ESA_forest_w40plus ESA_crop {
	replace `var'=`var'/100
}
*** This to get start and end years 

if `w'==1 {
	merge m:1 ipums_id using "${final}/forestloss.dta", nogen keep(match) keepusing(fl_*_yr Country)
}
else {
	merge m:1 ipums_id using "${final}/forestloss_period2.dta", nogen keep(match) keepusing(fl_*_yr Country)
}
replace fl_start_yr=2004 if fl_start_yr<2004

keep if year==fl_start_yr | year==fl_end_yr
sort ipums_id year

foreach var in ESA_forest_unweighted ESA_forest_weighted ESA_forest_w40plus ESA_crop {
	gen bl_`var'=`var'[_n-1] if year==fl_end_yr
	gen delta_`var'=(`var'-`var'[_n-1])/(fl_end_yr-fl_start_yr) if year==fl_end_yr
	gen delta_log_`var'=log((`var'/`var'[_n-1])^(1/(fl_end_yr-fl_start_yr))) if year==fl_end_yr
}

compress
keep if year==fl_end_yr

drop *yr
drop ES*
gen wave=1
save "${final}\ESA_wide", replace
