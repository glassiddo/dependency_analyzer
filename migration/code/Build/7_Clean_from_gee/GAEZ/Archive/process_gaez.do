
import delimited "${dropbox}\Migration Africa\DATA\RAW\AFRICA\GAEZ DATA\GAEZADM.csv", bindquote(strict) clear

** Aiming for country in wide format with variables for each crop / produciton , etc.
*** Don't know why this doesn't work
*** Need Ipums_id
ren cntry_name country
global stubnames ""
foreach var of varlist *har *prd *yld {
	local crop=substr("`var'",1,3)
	local year=substr("`var'",4,4)
	local end=substr("`var'",8,3)
	ren `var' `crop'_`end'`year'
	if `year'==2000 {
		global stubnames "$stubnames `crop'_`end'"
	}
}

reshape long $stubnames, i(country geo_id) j(year)
ren geo_id ipums_id
tostring ipums_id, replace
replace ipums_id="0" +ipums_id if country=="Botswana"

compress

save "${dropbox}\Migration Africa\data\Build\Africa\GAEZ data\gaez_processed.dta", replace









**** Below is old code

/*
foreach a in 1 2 {
	import delimited "${dropbox}\Migration Africa\data\Raw\Africa\GAEZ data\GAEZADM`a'.csv", bindquote(strict) clear
	stop
	foreach y in 2000 2010 {
		gegen GAEZ_cropped_area_`y'=rowtotal(*`y'har)
		gegen GAEZ_production_`y'=rowtotal(*`y'prd)
		gen GAEZ_avg_yield_`y'=GAEZ_production_`y'/GAEZ_cropped_area_`y'
	}
	
	foreach var in cropped_area avg_yield {
		gen GAEZ_delta_ln_`var'=ln(GAEZ_`var'_2010)-ln(GAEZ_`var'_2000)
	}
	
	
	
	foreach var of varlist *har {
		local crop=substr("`var'",1,3)
		local year=substr("`var'",4,4)
		disp "crop `crop', year `year'"
		gen `crop'`year'_YLD=`crop'`year'prd/`crop'`year'har
	}
	
	
	keep cntry_name bpl_code GAEZ_delta*
	
	compress
	
	save "${dropbox}\Migration Africa\data\build\Africa\GAEZ data\GAEZ_lvl`a'.dta", replace
}

*/
/*
foreach a in 1 2 {
	import delimited "${dropbox}\Migration Africa\data\Raw\Africa\GAEZ data\GAEZADM`a'.csv", bindquote(strict) clear

	reshape 
	stop
	save "${dropbox}\Migration Africa\data\build\Africa\GAEZ data\GAEZ_lvl`a'.dta", replace
}
*/