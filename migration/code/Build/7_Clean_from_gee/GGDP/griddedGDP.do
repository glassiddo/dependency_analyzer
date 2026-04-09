* This program produces estimates of local annual GDP between 1992 and 2019
cd "${projdir}"

* Local annual GDP

import delimited "..\MIGRATION AFRICA\DATA\RAW\AFRICA\GGDP\sampleUnitsGriddedGDP.csv", clear bindquote(strict)

levelsof country, local(lcountry)

preserve
	import delimited "..\MIGRATION AFRICA\DATA\RAW\World\GEE\worldUnitsGriddedGDP.csv", clear bindquote(strict)
	gen row = 1
	foreach c of local lcountry {
		replace row = 0 if iso3 == "`c'"
	}
	
	tab row
	keep if row == 1

	collapse (sum) areaha gdp*
	gen country = "Rest of the World"
	gen admin_name = "ROW"
	gen ipums_id = 1
	tempfile ROWGriddedGDP
	save `"`ROWGriddedGDP'"', replace
restore

qui append using `"`ROWGriddedGDP'"'

reshape long gdp, i(ipums_id) j(year)

la var country "Country"
la var admin_name "Admin unit"
la var ipums_id "IPUMS ID"

la var areaha "Area (ha)"
la var gdp "Local GDP"

drop geo 

order country admin_name ipums_id areaha gdp

tostring ipums_id , replace
replace ipums_id="0"+ipums_id if length(ipums_id)==5
compress
save "${final}/gridded_gdp_with_row.dta", replace
