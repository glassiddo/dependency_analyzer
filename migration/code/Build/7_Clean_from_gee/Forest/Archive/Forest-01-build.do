/*****************************************************************
PROJECT: 		SSA Environment
				
TITLE:			Build Forest Data.do
			
AUTHOR: 		Liam, Sam Marshall

DATE CREATED:	1/19/2022

LAST EDITED:	1/19/2022

DESCRIPTION: 	Create forest coverage and forest loss datasets


ORGANIZATION:	
				
******************************************************************/

/****************************************************************
	SECTION 2: Tree Cover
****************************************************************/

import delimited "${forestraw}/area_.csv", stringcols(2) encoding(ISO-8859-1) clear

keep adm* sum

ren sum area

tempfile area 
save `area'

import delimited "${forestraw}/treecover_.csv", stringcols(2) encoding(ISO-8859-1) clear

keep adm* sum 

ren sum treecover

merge 1:1 adm* using `area', nogen

rename (adm0_code adm0_name) (country_code country_name)
rename (adm1_code adm1_name) (region_code region_name)
rename (adm2_code adm2_name) (district_code district_name)

destring country_code, replace

save "${forestbuild}/treecover.dta", replace


/****************************************************************
	SECTION 2: Yearly Loss
****************************************************************/


import delimited "${forestraw}/units_lossXcoverbyyear.csv", stringcols(2) encoding(ISO-8859-1) clear

split groups , p("},")

keep adm* groups*
drop groups
forval y=1/20 {
	replace groups`y'=subinstr(groups`y',"{group=","",.)
	replace groups`y'=subinstr(groups`y',", sum="," ",.)
	replace groups`y'=subinstr(groups`y',"}","",.)
	replace groups`y'=subinstr(groups`y',"]","",.)
	replace groups`y'=subinstr(groups`y',"[","",.)
}

forval y=1/20 {
	gen year`y'=word(groups`y',1)
	gen loss`y'=word(groups`y',2)
}

forval y=1/20 {
	drop groups`y'
	destring year`y', replace
	destring loss`y', replace
	
}
forval y=1/20 {
	local year=2000+`y'
	gen loss_`year'=0
	forval v=1/20 {
		disp "y=`y', v=`v'"
		replace loss_`year'=loss_`year'+loss`v' if year`v'==`y' & loss`v'!=.
	}
}

keep adm* loss_*


reshape long loss_, i(adm0_code adm1_code adm2_code adm0_name adm1_name adm2_name) j(year)

rename (adm0_code adm0_name) (country_code country_name)
rename (adm1_code adm1_name) (region_code region_name)
rename (adm2_code adm2_name) (district_code district_name)

destring country_code, replace


merge m:1 country* region* district* using "${forestbuild}/treecover.dta", nogen

save "${forestbuild}/lossbyyear.dta", replace


/****************************************************************
	SECTION 3: Clean 30 percent cut off data
****************************************************************/

use "${projdir}/data/Hansen Forest Loss/Output/lossbyyear30_global.dta", clear

rename (adm0_code adm0_name) (country_code country_name)
rename (adm1_code adm1_name) (region_code region_name)
rename (adm2_code adm2_name) (district_code district_name)

rename tc2000 treecover
rename tcloss loss_

save "${forestbuild}/lossbyyear30.dta", replace





