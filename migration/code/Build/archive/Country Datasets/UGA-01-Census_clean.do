/*****************************************************************
PROJECT: 		SSA Migration History			
TITLE:			Clean Uganda Census.do		
AUTHOR: 		Sam Marshall
DATE CREATED:	3/30/2022
DESCRIPTION: 	Clean census for calculating populations
ORGANIZATION:			
******************************************************************/

* Set global directory paths
do "/Users/SMARSH/Dropbox (Personal)/Migration Africa/Code/Enviro-globals.do"

/****************************************************************
	SECTION 1: 
****************************************************************/

use "${ugacensus}/Census02_12.dta", clear

drop country sample

keep if age >= 15 & age <= 65

decode geo1_ug2014, gen(d_name)
decode geo1_ug2002, gen(d_name_2002)
decode geo1_ug, gen(d_name_cons)

decode mig1_p_ug, gen(prev_district)

replace d_name = d_name_2002 if d_name == ""

rename urban urbrur
gen urban = urbrur - 1

gen female = sex == 2

rename migyrs1 duration
replace duration = . if duration == 98 | duration == 99

drop geolev1-geo2_ug2014 sex geomig1_p mig1_p_ug d_name_2002 urbrur

compress 

save "${ugabuild}/Census/Census.dta", replace
