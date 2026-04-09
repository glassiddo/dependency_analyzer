*****************************************************************************************************************************
* THIS PROGRAM CREATES A DATASET REPORTING AREA OF FOREST AND TREE COVER ACROSS ADMINISTRATIVE UNITS IN 2000 USING GFC DATA *
*****************************************************************************************************************************

*  PREAMBLE  *

cd "${projdir}"

* Area of forest cover in 2000

import delimited "..\MIGRATION AFRICA\DATA\RAW\AFRICA\Forest\sampleUnitsTreeCover.csv", clear bindquote(strict)

vallist country, sort quote local(lcountry)

preserve
	import delimited "..\MIGRATION AFRICA\DATA\RAW\WORLD\GEE\worldUnitsTreeCover.csv", clear bindquote(strict)
	gen row = 1
	foreach cty in `lcountry' {
		di as text "`cty'"
		qui count if strpos(lower(adm0_name), lower("`cty'"))
		if `r(N)' > 0 {
			replace row = 0 if strpos(lower(adm0_name), lower("`cty'")) > 0
		}
		else {
			di as text "Spelling of `cty' does not match country names listed in FAO GAUL Database"
		}
	}
	replace row = 1 if inlist(adm0_name, "Equatorial Guinea", "Guinea-Bissau", "Papua New Guinea")
	tab row 
	keep if row == 1
	collapse (sum) *area*
	gen country = "Rest of the World"
	gen admin_name = "ROW"
	gen ipums_id = 1
	tempfile ROWTreeCover
	save `"`ROWTreeCover'"', replace
restore

qui append using `"`ROWTreeCover'"'

la var country "Country"
la var admin_name "Admin unit"
la var ipums_id "IPUMS ID"

la var areaha "Area (ha)"

drop tc*

order country ipums_id admin_name areaha

export delimited using "..\MIGRATION AFRICA\DATA\BUILD\FOREST COVER\gfcForestCover.csv", replace quote

compress
save "..\MIGRATION AFRICA\DATA\BUILD\FOREST COVER\gfcForestCover.dta", replace	

* Area of yearly forest cover loss between 2000 and 2020

import delimited "..\MIGRATION AFRICA\DATA\RAW\AFRICA\Forest\sampleUnitsTreeCoverLoss.csv", clear bindquote(strict)

vallist country, sort quote local(lcountry)

preserve
	import delimited "..\MIGRATION AFRICA\DATA\RAW\WORLD\GEE\worldUnitsTreeCoverLoss.csv", clear bindquote(strict)
	gen row = 1
	foreach cty in `lcountry' {
		di as text "`cty'"
		qui count if strpos(lower(adm0_name), lower("`cty'"))
		if `r(N)' > 0 {
			replace row = 0 if strpos(lower(adm0_name), lower("`cty'")) > 0
		}
		else {
			di as text "Spelling of `cty' does not match country names listed in FAO GAUL Database"
		}
	}
	replace row = 1 if inlist(adm0_name, "Equatorial Guinea", "Guinea-Bissau", "Papua New Guinea")
	tab row 
	keep if row == 1
	collapse (sum) areaha *loss*
	gen country = "Rest of the World"
	gen admin_name = "ROW"
	gen ipums_id = 1
	tempfile ROWTreeCoverLoss
	save `"`ROWTreeCoverLoss'"', replace
restore

qui append using `"`ROWTreeCoverLoss'"'

la var country "Country"
la var admin_name "Admin unit"
la var ipums_id "IPUMS ID"

la var areaha "Area (ha)"

drop tc*
drop geo systemindex

order country admin_name ipums_id areaha

export delimited using "..\MIGRATION AFRICA\DATA\BUILD\FOREST COVER\gfcForestCoverLoss.csv", replace  quote  

foreach var in fcloss00 fcloss10 fcloss20 fcloss30 fcloss40 fcloss50 fcloss60 fcloss70 fcloss80 fcloss90 {
	gen `var'2000 = 0
}

compress
save "..\MIGRATION AFRICA\DATA\BUILD\FOREST COVER\gfcForestCoverLoss.dta", replace	

* Data on forest cover and forest cover loss in the ROW

use "..\MIGRATION AFRICA\DATA\BUILD\FOREST COVER\gfcForestCover.dta", clear

merge 1:1 ipums_id using "..\MIGRATION AFRICA\DATA\BUILD\FOREST COVER\gfcForestCoverLoss.dta", keepusing(*loss*) keep(master matched) nogen 

reshape long fcloss00 fcloss10 fcloss20 fcloss30 fcloss40 fcloss50 fcloss60 fcloss70 fcloss80 fcloss90, i(ipums_id) j(year)

ren fcarea* treecover_*
forvalues t =0/9{
	bys ipums_id (year): gen loss`t'0 = sum(fcloss`t'0)
	gen treecover_new_`t'0 =  treecover_`t'0 - loss`t'0
	cap drop loss`t'0 fcloss`t'0
}

tostring ipums_id , replace
replace ipums_id="0"+ipums_id if length(ipums_id)==5

save "${final}/forestloss_cuts_long_with_row.dta", replace
