*************************************************************************
* THIS PROGRAM REPORTS ESTIMATES OF PERMANENT CROP AND ARABLE LAND AREA *
*    FOR EACH ADMIN UNIT ACROSS THE COUNTRIES INCLUDED IN THE STUDY     *
*************************************************************************

*  PREAMBLE  *

if "`c(username)'" == "houngbedji"{
	global projdir = "C:\USERS\HOUNGBEDJI\DROPBOX (COMPTE PERSONNEL)\MIGRATION AFRICA"
}

cd "${projdir}"

global rfao "..\MIGRATION AFRICA\DATA\RAW\AFRICA\FAO"
global bfao "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\FAO"

* IMPORTING DATA INTO STATA FILE *

import delimited "${rfao}\sampleUnitsPermanentCropArableLand.csv", clear bindquote(strict)

la var country "Country"
la var admin_name "Admin unit"
la var ipums_id "IPUMS ID"
la var geotype "Admin unit shape"
la var areaha "Area (ha)"

la var pcalarea  "Permanent crop and arable land (ha)"
la var pcalnfarea  "Permanent crop and arable land - not forested (ha)"

order country ipums_id admin_name geotype areaha pcalarea pcalnfarea
keep  country ipums_id admin_name geotype areaha pcalarea pcalnfarea

export delimited using "${bfao}\arableland.csv", replace

compress
save "${bfao}\arableland.dta", replace	
