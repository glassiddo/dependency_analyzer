/*****************************************************************
PROJECT: 		SSA Migration History
				
TITLE:			Build South Africa Migration.do
			
AUTHOR: 		Sam Marshall

DATE CREATED:	11/1/2021

LAST EDITED:	12/2/2021

DESCRIPTION: 	Create dataset with current and previous municipality and date of migration
				
Naming convention: region region_name district district_name prev_region 
	prev_region_name prev_district prev_district_name


ORGANIZATION:	
				
******************************************************************/

clear
set more off

* Set global directory paths
do "/Users/SMARSH/Dropbox (Personal)/Migration Africa/code/Enviro-globals.do"

/****************************************************************
	SECTION 1: 2016
	Note: previous muni codes are consistent with 2011 muni codes 
****************************************************************/

use "${saraw}/community survey/cs-2016-person.dta", clear
	
* keep only people living in usual residence
keep if UsualRes == 1

* keep only people born before 2011
keep if Since2011 <= 2

* drop people who don't know the date of move 
drop if YMToDU == 9999

rename Age age
rename YMToDU migration_year 
rename PMunic prev_muni
rename MN_CODE_2011 muni 
rename pers_pstrwgt p_wgt
rename UqNo hhid
rename Personno pid

gen urban = EA_GTYPE_C == 1

keep hhid pid age migration_year prev_muni PRReasons muni p_wgt urban

decode muni, gen(muni_name_long)

gen split_spot = strpos(muni_name_long, ":")

gen muni_name = substr(muni_name_long, split_spot + 2, .)

replace muni_name = "Emalahleni-EC" if muni_name_long == "EC136: Emalahleni"
replace muni_name = "Emalahleni-MP" if muni_name_long == "MP312: Emalahleni"

replace muni_name = "Naledi-FS" if muni_name_long == "FS164: Naledi"
replace muni_name = "Naledi-NW" if muni_name_long == "NW392: Naledi"

replace prev_muni = muni if migration_year == 8888
decode prev_muni, gen(prev_muni_name)

drop split_spot 


replace migration_year = . if migration_year == 8888

* adjust names
replace muni_name = "Khai-Ma" if muni == 368
replace prev_muni_name = "Khai-Ma" if prev_muni == 368
replace muni_name = "Kheis" if muni_name == "!Kheis"
replace prev_muni_name = "Kheis" if prev_muni_name == "!Kheis"
replace muni_name = "Khara Hais" if muni_name == "//Khara Hais"
replace prev_muni_name = "Khara Hais" if prev_muni_name == "//Khara Hais"

compress 

save "${sabuild}/Census/Census_2016.dta", replace

/****************************************************************
	SECTION 1: 2011
****************************************************************/

foreach fl in "1to5" "6to9" {
	use "${saraw}/census 2011/sa-census-2011-person-prov-`fl'-v1.2-20150825.dta", clear
		
	gen muni = P_MUNIC

	* drop people not living at their usual residence
	drop if P10_USUALRES == 2	
	
	gen migration_year = P11A_RESYEARMOVED

	gen prev_muni = P11C_PREVRESMUNIC
	replace prev_muni = muni if migration_year == .

	rename PERSON_10PER_WGT p_wgt 
	rename F02_AGE age 

	label values muni P_MUNIC
	label values prev_muni P_MUNIC

	* make code for previous residence outside SA
	replace prev_muni = 1000 if P11B_PREVRESPROV == 10
	
	rename ( P_DISTRICT P_PROVINCE) (district province)
	decode district, gen(district_name)
	decode province, gen(province_name)

	keep SN age P11_SINCE2001 p_wgt muni migration_year prev_muni dist* prov*
	
	decode muni, gen(muni_name)
	decode prev_muni, gen(prev_muni_name)
	
	if "`fl'" == "6to9" {
		replace muni_name = substr(muni_name, 6, .)
		replace prev_muni_name = substr(prev_muni_name, 6, .)
	}
	
	replace prev_muni_name = "999" if prev_muni == 999
	replace prev_muni_name = "Outside South Africa" if prev_muni == 1000

	* adjust characters
	replace muni_name = "Khai-Ma" if muni == 368
	replace prev_muni_name = "Khai-Ma" if prev_muni == 368
	replace muni_name = "Kheis" if muni_name == "!Kheis"
	replace prev_muni_name = "Kheis" if prev_muni_name == "!Kheis"
	replace muni_name = "Khara Hais" if muni_name == "//Khara Hais"
	replace prev_muni_name = "Khara Hais" if prev_muni_name == "//Khara Hais"
	replace muni_name = "Ga-Segonyane" if muni_name == "Ga-Segonyana"
	replace prev_muni_name = "Ga-Segonyane" if prev_muni_name == "Ga-Segonyana"

	tempfile SA_2011_`fl'
	save `SA_2011_`fl''
}

/*
* replace municipality with usual residence if code in range of municipalities
replace muni = P10B_USUALRESMUNIC if P10_USUALRES == 2

* drop people whose usual residence is outside SA or DK or unspecified
drop if P10A_USUALRESPROV == 10 | P10A_USUALRESPROV == 11 | P10A_USUALRESPROV == 99
*/

use `SA_2011_1to5'

append using `SA_2011_6to9'

save "${sabuild}/Census/Census_2011.dta", replace

/****************************************************************
	SECTION 3: 2001
****************************************************************/

use "${saraw}/census 2001/SA Census 2001 Person_v1.1_20111024.dta", clear

keep sn munic_co md_code dc_munic pr_code p02_age p11* p12* der60_mg-der67_mg weight

gen migration_year = p12b_96y + 1995
replace migration_year = . if migration_year == 2004

rename sn SN
rename munic_co muni 
rename p02_age age 
rename weight p_wgt

* drop people who are not at their usual residence
drop if p11_4ngt == 2
/*
* replace muni with main residence if different
tostring p11a_pur, gen(us_code)
gen us_muni = substr(us_code, 1, 3)
destring us_muni, replace 

replace muni = us_muni if us_muni >= 100
*/

* create previous municipality
tostring p12a_ppr, gen(prev_code)

gen prev_muni = substr(prev_code, 1, 3)
destring prev_muni, replace

* fill with current location if non-migrant
replace prev_muni = muni if migration_year == .

* drop undetermined place of previous residence 
drop if prev_muni == 0

* drop foreign 1996 residence 
* tab der62_mg if prev_muni == 3
replace prev_muni = 1000 if prev_muni == 3

* create string names for current and previous municipality
label values prev_muni munic_co
decode muni, gen(muni_name)
decode prev_muni, gen(prev_muni_name)
replace prev_muni_name = "Outside South Africa" if prev_muni == 1000

* adjust Emalahleni EC and MP 
replace muni_name = "Emalahleni-EC" if muni == 223
replace muni_name = "Emalahleni-MP" if muni == 809
replace prev_muni_name = "Emalahleni-EC" if prev_muni == 223
replace prev_muni_name = "Emalahleni-MP" if prev_muni == 809

replace muni_name = "Naledi-FS" if muni == 404
replace muni_name = "Naledi-NW" if muni == 612
replace prev_muni_name = "Naledi-FS" if prev_muni == 404
replace prev_muni_name = "Naledi-NW" if prev_muni == 612

rename (md_code dc_munic pr_code) (magesterial_district district province)
decode magesterial_district, gen(magesterial_district_name)
decode district, gen(district_name)
decode province, gen(province_name)

keep SN muni age p12_96 p_wgt prev_muni migration_year muni_name prev_muni_name ///
	magesterial_district* district* province*
compress

save "${sabuild}/Census/Census_2001.dta", replace

/****************************************************************
	SECTION 4: 1996
	Munis here are magesterial districts
****************************************************************/

use "${saraw}/census 1996/census-1996-person-v1.3.dta", clear

*keep province district age personno hhnumber migworke tempresi usualdis movedy2 moveddis pespweig hhid

* drop people who aren't at their usual residence
keep if tempresi == 1

* drop people who don't know when they moved 
drop if movedy2 == 9999

rename district muni 
rename moveddis prev_muni 

replace prev_muni = muni if movedy2 == 9997

rename pespweig p_wgt 
rename movedy2 migration_year 

rename (dccode province) (district province)
decode district, gen(district_name)
decode province, gen(province_name)

keep personno hhnumber hhid muni age p_wgt prev_muni migration_year dist* prov*

* cap migration at 10 years
gen ever_migrate = migration_year <= 1996
replace migration_year = . if migration_year == 9997

replace prev_muni = muni if migration_year == .

decode muni, gen(muni_name)
decode prev_muni, gen(prev_muni_name)

save "${sabuild}/Census/Census_1996.dta", replace
