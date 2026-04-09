/*****************************************************************
PROJECT: 		SSA Migration History
				
TITLE:			Build Uganda District xwalk.do
			
AUTHOR: 		Sam Marshall

DATE CREATED:	11/1/2021

LAST EDITED:	12/2/2021

DESCRIPTION: 	Create dataset with current, previous, and birth location, as 
				well as reason for move and time since move/year of move
				
Naming convention: region region_name district district_name prev_region 
	prev_region_name prev_district prev_district_name


ORGANIZATION:	
				
******************************************************************/

* Set global directory paths
do "/Users/SMARSH/Dropbox (Personal)/Migration Africa/Code/Enviro-globals.do"

/****************************************************************
	SECTION 1: 2005
****************************************************************/

* 2005 codes
use "${ugalsms}/NPS_2005-2009/2005_GSEC1.dta", clear

destring Districtc05, gen(district)
rename Districtn05 district_name_2005

keep district* 
duplicates drop 

save "${ugabuild}/Support/uga_dist_xwalk.dta", replace

* 2009 codes
use "${ugalsms}/NPS_2005-2009/2005_GSEC1.dta", clear

destring Districtc09, gen(district)
rename Districtn09 district_name_2009

keep district* 
duplicates drop 

merge 1:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta"
drop _merge 

replace district_name_2005 = "Luwero" if district == 104
replace district_name_2009 = "Luwero" if district == 104
replace district_name_2009 = "Bududa" if district == 218

save "${ugabuild}/Support/uga_dist_xwalk.dta", replace


* previous districts
use "${ugalsms}/NPS_2005-2009/2005_GSEC3.dta", clear

rename H3q11 district
decode district, gen(district_name_2005b)

keep district*
duplicates drop
drop if district == .

replace district_name_2005 = "Bundibugyo" if district == 401
replace district_name_2005 = "Ssembabule" if district == 111

merge 1:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta"

drop _merge
replace district_name_2005 = district_name_2005b if district_name_2005 == ""
drop district_name_2005b

save "${ugabuild}/Support/uga_dist_xwalk.dta", replace

/****************************************************************
	SECTION 2: 2009 - No labels for current location 
****************************************************************/

use "${ugalsms}/NPS_2005-2009/2009_GSEC3.dta", clear

rename h3q13 district 
decode district, gen(district_name_2009b)

keep district*
duplicates drop 

drop if district_name_2009b == ""
replace district_name_2009b = strproper(district_name_2009b)

merge 1:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta"

drop _merge 
replace district_name_2009 = district_name_2009b if district_name_2009 == ""
drop district_name_2009b

save "${ugabuild}/Support/uga_dist_xwalk.dta", replace


/****************************************************************
	SECTION 3: 2010 - Don't have codes for current district
****************************************************************/

use "${ugalsms}/NPS_2010/GSEC3.dta", clear
keep h3q13
duplicates drop
rename h3q13 district
decode district, gen(district_name_2010)
drop if district_name_2010 == ""

replace district_name_2010 = strproper(district_name_2010)
replace district_name_2010 = "Bukwo" if district_name_2010 == "Bukwa"

merge 1:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta"
drop _merge 

save "${ugabuild}/Support/uga_dist_xwalk.dta", replace


/****************************************************************
	SECTION 5: 2013 - no codes for current region--get from prev 
****************************************************************/

* Missing codes for current region so create
use "${ugalsms}/NPS_2013/GSEC3.dta", clear
keep h3q13D
duplicates drop
rename h3q13 district 
decode district, gen(district_name_2013)
drop if district_name_2013 == ""

rename district h1aq1a

merge 1:m h1aq1a using "${ugalsms}/NPS_2013/GSEC1.dta"
keep h1aq1a district_name_2013 
duplicates drop 

rename h1aq1a district 

merge 1:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta"
drop _merge 

save "${ugabuild}/Support/uga_dist_xwalk.dta", replace

/****************************************************************
	SECTION 6: 2015
****************************************************************/

use "${ugalsms}/NPS_2015/gsec1.dta", clear

rename region region_15
rename district_name district_name_2015 
keep district* region
duplicates drop 

merge 1:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta"
drop _merge 

* create district label that is most consistent to merge with 2011 strings
gen district_name = district_name_2015
* differences with 2013 - all name mismatch due to misspelling in 2013
replace district_name = district_name_2013 if district_name == ""
replace district_name = district_name_2010 if district_name == ""
replace district_name = district_name_2009 if district_name == ""
save "${ugabuild}/Support/uga_dist_xwalk.dta", replace


/****************************************************************
	SECTION 7: Match with 2011 strings
****************************************************************/
* need to create var for 2010 and 2011 and make str proper
* then need to delete duplicates and fix spellings
* finally merge with xwalk 

*use "${ugabuild}/Support/uga_dist_xwalk.dta", clear

use "${ugalsms}/NPS_2011/GSEC1.dta", clear

gen district_name_2011 = strproper(h1aq1)
replace district_name_2011 = "Kalangala" if district_name_2011 == "Kalanga"
replace district_name_2011 = "Luwero" if district_name_2011 == "Luweero"
*rename h1aq1 district_name_2011
keep district* 
duplicates drop

gen district_name = strproper(district_name_2011)
replace district_name = trim(district_name)
drop if district_name == ""


merge 1:m district_name using "${ugabuild}/Support/uga_dist_xwalk.dta"
drop _merge 
save "${ugabuild}/Support/uga_dist_xwalk.dta", replace

/****************************************************************
	SECTION 8: 2018 
	New Districts were created prior to this round as follows: (From wikipedia)
	
	Kyotera District became functional on 1 July 2017. Before that, it was part 
	of the Rakai District. 
	
	Up until 30 June 2018, Kikuube District was part of Hoima District.
	
	Rubanda District was established by an Act of Parliament on 3 September 2015 
	and became operational on 1 July 2016. Before its creation, the district was 
	Rubanda County in neighboring Kabale District.
	
	Kakumiro district was created by the government of Uganda, effective 1 July 
	2016, when Kibaale District was split into three creating the current 
	districts of Kagadi, Kakumiro and Kibaale
	
	Pakwach District was created by the government of Uganda in 2015 and became 
	operational on 1 July 2017. Prior to then it was part of Nebbi District
	
	Omoro District was created by the Parliament of Uganda on 3 September 2015, 
	and became operational on 1 July 2016.[6] Prior to its creation, Omoro was 
	"Omoro County" in neighboring Gulu District.
	
	Butebo district to become operational on 1 July 2017.[6] Prior to then the 
	district was "Butebo County" in Pallisa District
	
Change codes, but leaves names the same for this wave
district_name_2018	district_code_2018 old_district_name old_district_code
KYOTERA	125 -> Rakai 110
OMORO	331 -> Gulu  304
PAKWACH	332 -> Nebbi 310
KAGADI	427 -> Kibaale 407
KAKUMIRO	428 -> Kibale 407
RUBANDA	429 -> Kabale 404
KIKUUBE	432 -> Hoima 403
BUTEBO	233 -> Pallisa 210

****************************************************************/

use "${ugalsms}/NPS_2018/HH/GSEC1.dta", clear
rename region region_18
keep distirct_name district_code region 
duplicates drop
gen district_name_2018 = strproper(distirct_name)
gen district = district_code  // will use new codes in merge 
drop distirct_name

* corrections to district names
replace district_name_2018 = "Pallisa" if district_name_2018 == "Palisa"
replace district_name_2018 = "Rakai" if district_code == 110
replace district_name_2018 = "Gulu" if district_code == 304
replace district_name_2018 = "Nebbi" if district_code == 310
replace district_name_2018 = "Hoima" if district_code == 403
replace district_name_2018 = "Kabale" if district_code == 404
replace district_name_2018 = "Mubende" if district_code == 107
replace district_name_2018 = "Iganga" if district_code == 203
replace district_name_2018 = "Manafwa" if district_code == 223
replace district_name_2018 = "Apac" if district_code == 302
replace district_name_2018 = "Nakapiripirit" if district_code == 311
replace district_name_2018 = "Kabarole" if district_code == 405
replace district_name_2018 = "Kibale" if district_code == 407
replace district_name_2018 = "Ntungamo" if district_code == 411

* Mayuge has two codes in this wave, should have only one
replace district = 214 if district == 200
replace district_code = 214 if district_code == 200 
duplicates drop 

* adjust district codes for new districts to match with previous waves
replace district_code = 110 if district_code == 125 
replace district_code = 304 if district_code == 331 
replace district_code = 310 if district_code == 332 
replace district_code = 407 if district_code == 427 
replace district_code = 407 if district_code == 428 
replace district_code = 404 if district_code == 429 
replace district_code = 403 if district_code == 432 
replace district_code = 210 if district_code == 233 

* so merge on the new codes instead of the old 
rename district_code district_code_2018
label var district_code_2018 "Code of 2018 District prior to split"

merge 1:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta", nogen 

replace district_name = district_name_2018 if district_name == ""

rename region_18 region 
replace region = region_15 if region == .
drop region_15

save "${ugabuild}/Support/uga_dist_xwalk.dta", replace


