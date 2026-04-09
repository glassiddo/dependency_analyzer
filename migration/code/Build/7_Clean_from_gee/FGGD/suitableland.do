*******************************************************************************
* THIS PROGRAM CREATES A DATASET REPORTING AREA OF NON FORESTED LAND SUITABLE *
*   FOR PASTURE AND RAINFED CROPS OVER TIME AND ACROSS ADMINISTRATIVE UNITS   *
*******************************************************************************

*  PREAMBLE  *

if "`c(username)'" == "houngbedji"{
	global projdir = "C:\USERS\HOUNGBEDJI\DROPBOX (COMPTE PERSONNEL)\MIGRATION AFRICA"
}

cd "${projdir}"

global rfggd "..\MIGRATION AFRICA\DATA\RAW\AFRICA\FGGD"
global bfggd "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\FGGD"

cap log close SUITABLELAND

log using "${bfggd}\suitableland.txt", name(SUITABLELAND) replace text

* IMPORTING DATA INTO STATA FILE *

import delimited "${rfggd}\sampleUnitsSuitableLand.csv", clear bindquote(strict)

drop *count

rename (comblow* combint* combhig* ) (lowsuit* intsuit* higsuit*)

la var country "Country"
la var admin_name "Admin unit"
la var ipums_id "IPUMS ID"
la var geotype "Admin unit shape"
la var areaha "Area (ha)"

la var lowsuitarea   "Land area suitable for agriculture - low input level (ha)"
la var lowsuitnfarea "Not forested land area suitable for agriculture - low input level (ha)"

la var intsuitarea   "Land area suitable for agriculture - intermediate input level (ha)"
la var intsuitnfarea "Not forested land area suitable for agriculture - intermediate input level (ha)"

la var higsuitarea   "Land area suitable for agriculture - high input level (ha)"
la var higsuitnfarea "Not forested land area suitable for agriculture - high input level (ha)"

order country ipums_id admin_name geotype areaha

export delimited using "${bfggd}\suitableland.csv", replace

compress
save "${bfggd}\suitableland.dta", replace	

log close SUITABLELAND