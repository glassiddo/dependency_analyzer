
global cash_crops "ban cot frt olp olv nes sub suc sfl tob veg"

*use "${dropbox}\Migration Africa\data\Build\Africa\GAEZ data\sample_ts.dta", clear
use "${dropbox}\Migration Africa\data\Build\Africa\GAEZ data\gaez_processed.dta", clear


drop *_yld
reshape wide *_prd *_har, i(ipums_id) j(year)

foreach y in 2000 2010 {
	gegen GAEZ_cropped_area_`y'=rowtotal(*har`y')
	replace GAEZ_cropped_area_`y'=1000*GAEZ_cropped_area_`y'
	gegen GAEZ_production_`y'=rowtotal(*prd`y')
	gen GAEZ_avg_yield_`y'=GAEZ_production_`y'/GAEZ_cropped_area_`y'
	
	gen GAEZ_cropped_area_cash_`y'=0
	gen GAEZ_production_cash_`y'=0
	foreach crop in $cash_crops {
		disp "`crop'"
		replace GAEZ_cropped_area_cash_`y'=GAEZ_cropped_area_cash_`y'+`crop'_har`y'
		replace GAEZ_production_cash_`y'=GAEZ_production_cash_`y'+`crop'_prd`y'
	}
	replace GAEZ_cropped_area_cash_`y'=1000*GAEZ_cropped_area_cash_`y'
}
	
	
foreach var in cropped_area avg_yield production  cropped_area_cash  production_cash{
	gen GAEZ_delta_ln_`var'=ln(GAEZ_`var'_2010)-ln(GAEZ_`var'_2000)
}

global croplist ""
foreach var of varlist *har* {
	local crop=substr("`var'",1,3)
	
	local year=substr("`var'",-4,4)
	if "`year'"=="2000" {
		global croplist "$croplist `crop'"
	}
	disp "crop `crop', year `year'"
	gen `crop'_YLD_`year'=`crop'_prd`year'/`crop'_har`year'
	
}


/*	
foreach crop in $croplist {
	gen GAEZ_delta_ln_yld_`crop'=ln(`crop'_YLD_2010)-ln(`crop'_YLD_2010)
}
*/
keep ipums_id GAEZ* country 
compress
save "${build}/Cross Country/GAEZ.dta", replace
save "${final}/GAEZ.dta", replace