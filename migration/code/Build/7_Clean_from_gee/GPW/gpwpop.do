*****************************************************************************
* THIS PROGRAM CREATES A DATASET REPORTING POPULATION DATA IN 2000 AND 2010 *
*       ACROSS ADMINISTRATIVE UNITS USING GRIDDED POPULATION DATA V4        *
*****************************************************************************

*  PREAMBLE  *

cd "${projdir}"

* IMPORTING GPW DATA INTO STATA FILE *

import delimited "..\MIGRATION AFRICA\DATA\RAW\AFRICA\GPW\sampleUnitsGpwAggregate.csv", clear bindquote(strict)

levelsof country, local(lcountry)

preserve
	import delimited "..\MIGRATION AFRICA\DATA\RAW\WORLD\GEE\worldUnitsGpwAggregate.csv", clear bindquote(strict)
	
	gen row = 1
	foreach c of local lcountry {
		replace row = 0 if iso3 == "`c'"
	}
	
	tab row
	keep if row == 1
	collapse (first) geotype (sum) areaha *2000* *2010*
	
	gen country = "Rest of the World"
	gen admin_name = "ROW"
	gen ipums_id = 1

	tempfile ROWGPWPOP
	save `"`ROWGPWPOP'"', replace
restore

append using `"`ROWGPWPOP'"'

rename (*2000* *2005* *2010* *2015* *2020*) (**2000 **2005 **2010 **2015 **2020)

reshape long gpwcount gpwarea gpwvalue, i(ipums_id) j(year)

keep if inlist(year, 2000, 2010)

gen population = round(gpwvalue, 1)

drop gpw*

la var country "Country"
la var admin_name "Admin unit"
la var ipums_id "IPUMS ID"
la var geotype "Admin unit shape"
la var areaha "Area (ha)"
la var year "Year"
la var population "Population"

order *, alphabetic 
order country ipums_id admin_name geotype areaha year

export delimited using "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\GPW\population.csv", replace

compress
save "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\GPW\population.dta", replace	
