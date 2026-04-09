*************************************************************************
* THIS PROGRAM CREATES A DATASET REPORTING LAND SUITABILITY INDEXES FOR *
*       FOR PASTURE AND RAINFED CROPS ACROSS ADMINISTRATIVE UNITS       *
*************************************************************************

*  PREAMBLE  *

if "`c(username)'" == "houngbedji"{
	global projdir = "C:\USERS\HOUNGBEDJI\DROPBOX (COMPTE PERSONNEL)\MIGRATION AFRICA"
}

cd "${projdir}"

global rfggd "..\MIGRATION AFRICA\DATA\RAW\AFRICA\FGGD"
global bfggd "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\FGGD"

cap log close LANDSI

log using "${bfggd}\landsi.txt", name(LANDSI) replace text

* IMPORTING DATA INTO STATA FILE *

import delimited "${rfggd}\sampleUnitsLandSuitabilityIndex.csv", clear bindquote(strict)

la var country "Country"
la var admin_name "Admin unit"
la var ipums_id "IPUMS ID"
la var geotype "Admin unit shape"
la var areaha "Area (ha)"

gen csia = csivalue/csicount
gen csib = csiarea/csinwarea

gen psia = psivalue/psicount
gen psib = psiarea/psinwarea

gen asia = asivalue/asicount
gen asib = asiarea/asinwarea

la var asia "Land suitability index for agriculture - simple average"
la var asib "Land suitability index for agriculture - area weighted average"

la var psia "Land suitability index for pasture - simple average"
la var psib "Land suitability index for pasture - area weighted average"

la var csia "Land suitability index for rainfed crop - simple average"
la var csib "Land suitability index for rainfed crop - area weighted average"

order country ipums_id admin_name geotype areaha asia asib csia csib psia psib
keep  country ipums_id admin_name geotype areaha asia asib csia csib psia psib

export delimited using "${bfggd}\landsi.csv", replace

compress
save "${bfggd}\landsi.dta", replace	

log close LANDSI