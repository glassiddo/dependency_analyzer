********************************************************************
* THIS PROGRAM COMBINES KEY DATASETS NEEDED TO CALIBRATE THE MODEL *
********************************************************************

*  PREAMBLE  *

if "`c(username)'" == "houngbedji"{
	global projdir = "C:\USERS\HOUNGBEDJI\DROPBOX (COMPTE PERSONNEL)\MIGRATION AFRICA"
}

cd "${projdir}"

global cfao    "..\MIGRATION AFRICA\CODE\BUILD\FAO"
global cfggd   "..\MIGRATION AFRICA\CODE\BUILD\FGGD"
global cforest "..\MIGRATION AFRICA\CODE\BUILD\FOREST"
global cgaez   "..\MIGRATION AFRICA\CODE\BUILD\GAEZ"
global cgdp    "..\MIGRATION AFRICA\CODE\BUILD\GDP"

global dfao    "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\FAO"
global dfggd   "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\FGGD"
global dforest "..\MIGRATION AFRICA\DATA\BUILD\FOREST COVER"
global dgaez   "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\GAEZ DATA"
global dntli   "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\NIGHT LIGHT"

global final "..\MIGRATION AFRICA\DATA\FINAL"

* Value added outside agriculture *

preserve
	qui do "${cgdp}\nonaggdp.do"
restore

use "${dntli}\nonaggdp.dta", clear

* Land suitability index for pasture and rainfed crops *

preserve
	qui do "${cfggd}\landsi.do"
restore
merge m:1 ipums_id using "${dfggd}\landsi.dta", keepusing(csi* psi* asi*) keep(master matched) nogen

* Land area suitable for pasture and rainfed crops *

preserve
	qui do "${cfggd}\suitableland.do"
restore
merge m:1 ipums_id using "${dfggd}\suitableland.dta", keepusing(*suit*) keep(master matched) nogen

* Forest and tree cover in 2000

preserve
	qui do "${cforest}\forestcover.do"
restore
merge m:1 ipums_id using "${dforest}\gfcforestcover.dta", keepusing(fc2000ha tc2000ha) keep(master matched) nogen

* Permanent crop and arable land *

preserve
	qui do "${cfao}\arableland.do"
restore
merge m:1 ipums_id using "${dfao}\arableland.dta", keepusing(pcalarea pcalnfarea) keep(master matched) nogen

* Aggregate crop production value *

preserve
	qui do "${cgaez}\agvalue.do"
restore
merge 1:1 ipums_id year using "${bgaez}\agoutputvalue.dta", keepusing(agarea agvalue) keep(master matched) nogen

* Yield

ren ipums_id adm_code
preserve
	qui do "${cgaez}\agyield.do"
restore
merge 1:1 adm_code year using "${bgaez}\yield.dta", keepusing(*yld) keep(master matched) nogen
ren adm_code ipums_id

compress
save "${final}\parameters.dta", replace	
