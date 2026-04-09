************************************************************************
* THIS PROGRAM CREATES A DATASET REPORTING THE VALUE OF CROPS PRODUCED *
*          IN 2000 AND 2010 ACROSS ADMINISTRATIVE UNITS                *
************************************************************************
* IMPORTING GAEZ DATA FROM ADMIN UNITS OF INTEREST INTO STATA FILE *

import delimited "${projdir}\DATA\RAW\AFRICA\GAEZ DATA\sampleUnitsGAEZValue.csv", clear bindquote(strict)
levelsof country, local(lcountry)

preserve
	import delimited "${projdir}\DATA\RAW\WORLD\GEE\worldUnitsGaezAggregate.csv", clear bindquote(strict)

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
	
	tempfile ROW
	save `"`ROW'"', replace
restore

append using `"`ROW'"'

rename (*2000* *2010*) (**2000 **2010)

drop cer* oil* rts* allcount*

reshape long allarea allvalue, i(ipums_id) j(year)

ren all* ag*

la var country "Country"
la var admin_name "Admin unit"
la var ipums_id "IPUMS ID"
la var geotype "Admin unit shape"
la var areaha "Area (ha)"
la var year "Year"
la var agarea "Land area cultivated (ha)"
la var agvalue "Aggregate crop production value (2000 International $)" 

order *, alphabetic 
order country ipums_id admin_name geotype areaha year
sort country ipums_id year

export delimited using "${projdir}\DATA\BUILD\AFRICA\GAEZ DATA\agoutputvalue.csv", replace

compress
save "${projdir}\DATA\BUILD\AFRICA\GAEZ DATA\agoutputvalue.dta", replace	
