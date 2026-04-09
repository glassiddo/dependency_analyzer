/*****************************************************************
PROJECT: 		SSA Migration History			
TITLE:			Clean Rwanda Census.do		
AUTHOR: 		Sam Marshall
DATE CREATED:	5/26/2022
DESCRIPTION: 	Clean census for calculating migration flows
ORGANIZATION:			
******************************************************************/

* Set global directory paths
do "/Users/SMARSH/Dropbox (Personal)/Migration Africa/Code/Enviro-globals.do"

/****************************************************************
	SECTION 1: 
****************************************************************/
** Not RUN **
use "${rwaraw}/Census/Census02_12.dta", clear

keep if age >= 15 & age <= 65

gen female = sex == 2

* drop cases where duration is unknown or not in the universe
drop if rw2002a_resdur == 998 | rw2002a_resdur == 999
drop if rw2012a_resdur == 99

replace rw2002a_resdur = 98 if rw2002a_resdur == 990

gen duration = rw2002a_resdur if year == 2002
replace duration = rw2012a_resdur if year == 2012
replace duration = . if duration == 98

decode bplcountry, gen(birth_country)
replace birth_country = strproper(birth_country)

drop country sample sex rw2002a_resdur rw2012a_resdur geolev1 geolev2 migyrs1 ///
	bplcountry

decode geo2_rw2002, gen(district_name2002)
decode geo2_rw2012, gen(district_name2012)

replace district_name2002 = strproper(district_name2002)
replace district_name2012 = strproper(district_name2012)

decode rw2012a_resprev, gen(prev_district_name2012)
replace prev_district_name2012 = strproper(prev_district_name2012)

decode migrw2, gen(prev_district_name)
decode migrw4, gen(prev_district_name2)
*replace 
	

compress 

save "${rwabuild}/Census/Census.dta", replace
