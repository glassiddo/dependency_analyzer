***********************************************************************************
* THIS PROGRAM CREATES A DATASET REPORTING BUILT-UP AREA AND BUILT-UP VOLUME DATA *
*          IN 1975 THROUGH 2020 ACROSS ADMINISTRATIVE UNITS USING GHSL            *
***********************************************************************************

*  PREAMBLE  *
cd "${projdir}"

* IMPORTING GHSL BUILT-UP AREA DATA INTO STATA FILE *

import delimited "..\MIGRATION AFRICA\DATA\RAW\AFRICA\GHSL\sampleUnitsGHSLBUArea.csv", clear bindquote(strict)

levelsof country, local(lcountry)

preserve
	import delimited "..\MIGRATION AFRICA\DATA\RAW\WORLD\GEE\worldUnitsGHSLBUArea.csv", clear bindquote(strict)
	
	gen row = 1
	foreach c of local lcountry {
		replace row = 0 if iso3 == "`c'"
	}
	
	tab row
	keep if row == 1

	collapse (first) geotype (sum) areaha *1975 *1980 *1985 *1990 *1995 *2000 *2005 *2010 *2015 *2020
	gen country = "Rest of the World"
	gen admin_name = "ROW"
	gen ipums_id = 1
	tempfile ROWGHLSAREA
	save `"`ROWGHLSAREA'"', replace
restore

append using `"`ROWGHLSAREA'"'

tempfile BUILTUPAREA
save `"`BUILTUPAREA'"', replace

* IMPORTING GHSL BUILT UP VOLUME DATA INTO STATA FILE *

import delimited "..\MIGRATION AFRICA\DATA\RAW\AFRICA\GHSL\sampleUnitsGHSLBUVolume.csv", clear bindquote(strict)

preserve
	import delimited "..\MIGRATION AFRICA\DATA\RAW\WORLD\GEE\worldUnitsGHSLBUVolume.csv", clear bindquote(strict)

	gen row = 1
	foreach c of local lcountry {
		replace row = 0 if iso3 == "`c'"
	}
	
	tab row
	keep if row == 1

	collapse (first) geotype (sum) areaha *1975 *1980 *1985 *1990 *1995 *2000 *2005 *2010 *2015 *2020
	
	gen country = "Rest of the World"
	gen admin_name = "ROW"
	gen ipums_id = 1
	
	tempfile ROWGHLSVOLUME
	save `"`ROWGHLSVOLUME'"', replace
restore

append using `"`ROWGHLSVOLUME'"'

* COMBINING GHSL BUILT UP AREA AND VOLUME DATA INTO A SINGLE FILE 

merge 1:1 ipums_id using `"`BUILTUPAREA'"', keep(master using matched) keepusing(bua*) nogen

reshape long buatotal buanres buvtotal buvnres , i(ipums_id) j(year)

keep if inlist(year, 1980, 1990, 2000, 2010)

la var country "Country"
la var admin_name "Admin unit"
la var ipums_id "IPUMS ID"
la var geotype "Admin unit shape"
la var areaha "Area (ha)"
la var year "Year"

la var buatotal "Built-up area (Total, ha)"
la var buanres  "Built-up area (Non-Residendial, ha)"

la var buvtotal "Built-up volume (Total, m3)"
la var buvnres  "Built-up volume (Non-Residendial, m3)"

order *, alphabetic 
order country ipums_id admin_name geotype areaha year

export delimited using "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\GHSL\builtup.csv", replace

compress
save "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\GHSL\builtup.dta", replace	
