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

use "${rawdata}/Benin/migration_2013.dta", clear

use "${rawdata}/Botswana/Census/Consistent01_11.dta", clear

use "${rawdata}/Burkina Faso/Census/consistent96_06.dta", clear

use "${rawdata}/Ghana/Census/consistent00_10.dta", clear

use "${rawdata}/Guinea/Census/consistent96_14.dta", clear

use "${rawdata}/Kenya/Census/consistent99_09.dta", clear

use "${rawdata}/Lesotho/Census/consistent96_06.dta", clear

use "${rawdata}/Mali/Census/consistent98_09.dta", clear

use "${rawdata}/Mauritius/Census/consistent00_11.dta", clear

use "${rawdata}/Mozambique/Census/consistent97_07.dta", clear

use "${rawdata}/Senegal/Census/consistent02_13.dta", clear

use "${rawdata}/Sierra Leone/Census/consistent04_15.dta", clear

use "${rawdata}/South Africa/IPUMS/Census/consistent01_16.dta", clear

use "${rawdata}/South Africa/census 2001/SA Census 2001 Person_v1.1_20111024.dta", clear

use "${rawdata}/South Africa/census 2011/sa-census-2011-person-prov-1to5-v1.2-20150825.dta", clear

use "${rawdata}/Tanzania/Census/consistent02_12.dta", clear

use "${rawdata}/Uganda/Census/consistent02_12.dta", clear

use "${rawdata}/Zambia/Census/consistent00_10.dta", clear

/****************************************************************
	SECTION 1: Tanzania (level 1, 1 year)
****************************************************************/

* can do regional level migration for 23 units (accounting for redistricting)
use "${tzacensus}/Census2002_2012.dta", clear

*keep if age >= 15 & age <= 65

rename (geo1_tz) (region)
rename (mig1_1_tz) (prev_region)
rename (bpltz) (birth_region)

decode region, gen(region_name)
decode prev_region, gen(prev_region_name)
decode birth_region, gen(birth_region_name)

rename perwt wgt
keep year age wgt *region* 

*save "${build}/Tanzania/Census/Census_1yr.dta", replace

/****************************************************************
	SECTION 2: Uganda (level 2, duration)
****************************************************************/

* geo variables not labeled, this one is a bit of a pain, come back to
use "${ugacensus}/Census2002_2012.dta", clear

*keep if age >= 15 & age <= 65

rename regnug region
rename geo1_ug district 
rename mig1_p_ug prev_district 
rename migyrs1 duration
rename perwt wgt 

decode district, gen(district_name)
decode prev_district, gen(prev_district_name)
decode region, gen(region_name)

keep year age wgt *district* region* duration

* okay to leave 98/99 as is
gen migration_year = year - duration

*save "${build}/Uganda/Census/Census_duration.dta", replace

/****************************************************************
	SECTION 3: Senegal (level 2 department, 1, 5, 10 years ago)
****************************************************************/

use "${rawdata}/Senegal/Census/Census2002_2013.dta", clear

*keep if age >= 15 & age <= 65

rename (geo1_sn geo2_sn) (region district)
rename (mig1_5_sn mig2_5_sn) (prev_region prev_district)

decode region, gen(region_name)
decode district, gen(district_name)
decode prev_region, gen(prev_region_name)
decode prev_district, gen(prev_district_name)

rename perwt wgt
keep year age wgt district* region* prev*

*save "${build}/Senegal/Census/Census_5yr.dta", replace

/****************************************************************
	SECTION 4: Mozambique (level 2 district, 1, 5 years ago)
****************************************************************/

use "${rawdata}/Mozambique/Census/Consistent.dta", clear

*keep if age >= 15 & age <= 65

rename (geo1_mz geo2_mz) (region district)
rename (mig1_5_mz mig2_5_mz) (prev_region prev_district)
rename (bplmz1 bplmz2) (birth_region birth_district)

decode region, gen(region_name)
decode district, gen(district_name)
decode prev_region, gen(prev_region_name)
decode prev_district, gen(prev_district_name)
decode birth_region, gen(birth_region_name)
decode birth_district, gen(birth_district_name)

rename perwt wgt
keep year age wgt *district* *region* 

*save "${build}/Mozambique/Census/Census_5yr.dta", replace

/****************************************************************
	SECTION 4: Guinea (level 2 district, 1, 5 years ago)
****************************************************************/

use "${rawdata}/Guinea/Census/Consistent.dta", clear

rename regngn region
rename geo1_gn district 
rename mig1_p_gn prev_district 
rename migyrs1 duration
rename perwt wgt 

decode district, gen(district_name)
decode prev_district, gen(prev_district_name)
decode region, gen(region_name)

keep year age wgt *district* region* duration

* okay to leave 98/99 as is
gen migration_year = year - duration

*save "${build}/Guinea/Census/Census_duration.dta", replace

/****************************************************************
	SECTION 4: Rwanda (level 2 department, 1, 5, 10 years ago)
****************************************************************/
/*
use "${rawdata}/Rwanda/Census/Census02_12.dta", clear

keep if age >= 15 & age <= 65

rename (geo1_sn geo2_sn) (region district)
rename (mig1_5_sn mig2_5_sn) (prev_region prev_district)

decode region, gen(region_name)
decode district, gen(district_name)
decode prev_region, gen(prev_region_name)
decode prev_district, gen(prev_district_name)

rename perwt wgt
keep year age wgt district* region* prev*

save "${build}/Senegal/Census/Census_5yr.dta", replace
*/
