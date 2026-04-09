/*****************************************************************
PROJECT: 		SSA Migration History
				
TITLE:			Build Uganda.do
			
AUTHOR: 		Sam Marshall

DATE CREATED:	11/1/2021

LAST EDITED:	11/1/2021

DESCRIPTION: 	Create dataset with current, previous, and birth location, as 
				well as reason for move and time since move/year of move
				
Naming convention: vars I need to create
	district (code)
	district_name
	year (of survey)
	irw_year (year of interview)
	urban 
	prev_district (code)
	prev_district_name
	prev_urban 
	move_reason
	(only 4 regions, so don't do those)

ORGANIZATION:	
				
******************************************************************/


* Set global directory paths
do "/Users/SMARSH/Dropbox (Personal)/Migration Africa/code/Enviro-globals.do"

capture program drop backfill_prev_dist
program define backfill_prev_dist

	capture rename district district_code

	* get sex and age information
	gen female = h2q3 == 0 | h2q3 == 2
	rename h2q8 age
	
	rename (h3q13 h3q14 h3q15 h3q16 h3q18) ///
		(birth_district fivey_district duration prev_district move_reason)
	drop if duration == .
	replace duration = 100 if duration >= 100 

	keep year HHID PID *urban* *wgt* *district* irw_year duration move_reason female age 

	* back fill five year district and prev district if lived in current loc for more than five years
	replace fivey_district = district_code if fivey_district == . & duration > 5
	* still missing ones have duration less than 5, so put in birth district 
	replace fivey_district = birth_district if fivey_district == .

	* prev dist gets curr if lived there forever
	replace prev_district = district_code if duration == 100
	* five years ago district if still missing and duration > = 5
	replace prev_district = fivey_district if mi(prev_district) & duration >= 5
	* still missing also get five years ago district
	replace prev_district = fivey_district if mi(prev_district)

	* one guy with no previous location info, so drop
	drop if prev_district == .

	* merge in xwalk for prev district 
	rename prev_district district 
	merge m:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta", nogen keep(match)
	drop district_name_* district_code_2018

	rename (region district district_name) prev_=
	rename district_code district 
	merge m:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta", nogen keep(match)
	drop district_name_* district_code_2018
	
	decode region, gen(region_name)
	decode prev_region, gen(prev_region_name)

end


/****************************************************************
	SECTION 1: 2005
	Notes: Migration data only for those who have lived somewhere
			else for at least 6 months since 2001
****************************************************************/

use "${ugalsms}/NPS_2005-2009/2005_GSEC1.dta", clear
gen urban = Substrat == 1
destring Districtc05, gen(district_code)

*rename Districtn05 district_name_wave
rename Year year
rename Yrsurve irw_year 
rename Hmult p_wgt
keep year Hhid district* urban irw_year p_wgt

rename Hhid HHID
merge 1:m HHID using "${ugalsms}/NPS_2005-2009/2005_GSEC2.dta", nogen 

gen female = h2q4 == 2
rename h2q9 age

rename HHID Hhid
rename PID Pid 
merge 1:m Hhid Pid using "${ugalsms}/NPS_2005-2009/2005_GSEC3.dta", keep(match)

* create duration since last move
rename H3q10 migration_year

drop if migration_year < 1000
*replace migration_year = . if migration_year < 1000
gen duration = irw_year - migration_year + 1

* make migration_year 1999 for anyone who hasn't moved
*replace migration_year = 1999 if migration_year == . | migration_year < 1999

* create previous district
gen prev_urban = H3q12 == 2
rename H3q11 prev_district
* decode prev_district, gen(prev_district_name_wave)

rename H3q13 move_reason

keep year Hhid Pid female age district* urban irw_year migration_year ///
	duration prev* move_reason p_wgt

replace prev_district = district_code if migration_year == .
replace prev_district = district_code if prev_district == .

rename prev_district district 
merge m:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta", nogen keep(match)
drop district_name_*

rename (region district district_name) prev_=
rename district_code district 
merge m:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta", nogen keep(match)
drop district_name_*

rename Hhid HHID
rename Pid PID

rename move_reason mr_code
decode mr_code, gen(move_reason)

decode region, gen(region_name)
decode prev_region, gen(prev_region_name)

* make consistent with other years
replace duration = 100 if migration_year == .
drop migration_year

save "${ugabuild}/LSMS/Uganda_2005.dta", replace

/****************************************************************
	SECTION 2: 2009
	Notes: 
****************************************************************/

use "${ugalsms}/NPS_2005-2009/2009_GSEC1.dta", clear

rename h1aq1_05 district
rename h1bq2c irw_year  
rename wgt09 p_wgt 
keep year HHID urban p_wgt district irw_year 

drop if district == .

merge 1:m HHID using "${ugalsms}/NPS_2005-2009/2009_GSEC2.dta", nogen keep(match)
gen female = h2q3 == 2
rename h2q8 age

merge 1:1 HHID PID using "${ugalsms}/NPS_2005-2009/2009_GSEC3.dta", keep(match) nogen

* district name is missing labels
rename h3q13 birth_district
rename h3q14 fivey_district 
rename h3q15 duration
rename h3q16 prev_district 
rename h3q18 move_reason
recode h3q17 (2 = 1) (3 = 0), gen(prev_urban)

keep year HHID PID *urban* *wgt* *district* irw_year duration move_reason female age 

drop if duration == .

	* back fill five year district and prev district if lived in current loc for more than five years
	replace fivey_district = district if fivey_district == . & duration > 5
	* still missing ones have duration less than 5, so put in birth district 
	replace fivey_district = birth_district if fivey_district == . | fivey_district == 0

	* prev dist gets curr if lived there forever
	replace prev_district = district if duration == 100
	* five years ago district if still missing and duration > = 5
	replace prev_district = fivey_district if mi(prev_district) & duration >= 5
	* still missing also get five years ago district
	replace prev_district = fivey_district if mi(prev_district)
	
* these are miscodes
replace prev_district = fivey_district if prev_district <= 100 | ///
	prev_district == 133 | prev_district == 604 | prev_district == 999

drop if prev_district == 999

rename district district_code 
rename prev_district district 
merge m:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta", nogen keep(match)
drop district_name_*

rename (region district district_name) prev_=
rename district_code district 

merge m:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta", nogen keep(match)
drop district_name_*

rename move_reason mr_code
decode mr_code, gen(move_reason)

decode region, gen(region_name)
decode prev_region, gen(prev_region_name)

save "${ugabuild}/LSMS/Uganda_2009.dta", replace


/****************************************************************
	SECTION 3: 2010
	Notes: Don't have codes for current district
****************************************************************/

use "${ugalsms}/NPS_2010/GSEC1.dta", clear
rename year irw_year 
rename wgt10 p_wgt 
gen year = 2010
gen district_name = strproper(h1aq1)
drop if district_name == ""
replace district_name = "Luwero" if district_name == "Luweero"
replace district_name = "Mityana" if district_name == "Miyana"

keep year HHID urban p_wgt district_name irw_year 

merge m:1 district_name using "${ugabuild}/Support/uga_dist_xwalk.dta", nogen keep(match)
drop district_name* district_code_2018
rename district district_code

merge 1:m HHID using "${ugalsms}/NPS_2010/GSEC2.dta", keep(match) nogen

merge 1:1 HHID PID using "${ugalsms}/NPS_2010/GSEC3.dta", keep(match) nogen

gen prev_urban = h3q17 == 1 | h3q17 == 2

* create previous district 
backfill_prev_dist

rename move_reason mr_code
decode mr_code, gen(move_reason)

save "${ugabuild}/LSMS/Uganda_2010.dta", replace

/****************************************************************
	SECTION 4: 2011
	Notes: No codes for current district and no value labels for previous...
****************************************************************/

use "${ugalsms}/NPS_2011/GSEC1.dta", clear

rename mult p_wgt 
rename year irw_year
gen year = 2011 

* create district names and get codes for current 
gen district_name = strproper(h1aq1)
replace district_name = "Kalangala" if district_name == "Kalanga"
replace district_name = "Luwero" if district_name == "Luweero"
replace district_name = trim(district_name)
drop if district_name == ""

keep year HHID irw_year urban p_wgt district_name

merge m:1 district_name using "${ugabuild}/Support/uga_dist_xwalk.dta", nogen keep(match)
drop district_name* district_code_2018


merge 1:m HHID using "${ugalsms}/NPS_2011/GSEC2.dta", keep(match) nogen
drop h2q1
merge 1:1 HHID PID using "${ugalsms}/NPS_2011/GSEC3.dta", keep(match) nogen

gen prev_urban = h3q17 == 1 | h3q17 == 2
* create previous district 
backfill_prev_dist

rename move_reason mr_code
decode mr_code, gen(move_reason)

save "${ugabuild}/LSMS/Uganda_2011.dta", replace

/****************************************************************
	SECTION 5: 2013
	Notes: No previous urban/rural status, so district only here...
****************************************************************/

use "${ugalsms}/NPS_2013/GSEC1.dta", clear

* want to use cross section weight here... I think
rename wgt_X p_wgt 
rename wgt panel_wgt
rename year irw_year
gen year = 2013 
rename h1aq1a district
keep year irw_year HHID urban *wgt district 

merge 1:m HHID using "${ugalsms}/NPS_2013/GSEC2.dta", nogen 

merge 1:1 HHID PID using "${ugalsms}/NPS_2013/GSEC3.dta", keep(2 3) nogen

rename h3q*D h3q*

* create previous district 
backfill_prev_dist

rename move_reason mr_code
decode mr_code, gen(move_reason)

drop wgt_X 

save "${ugabuild}/LSMS/Uganda_2013.dta", replace


/****************************************************************
	SECTION 6: 2015
	Seems only those aged 17 or younger were given the questionaire about previous living
	although all were supposed to get it
	
vars: HHID district urban p_wgt panel_wgt irw_year year district_name PID age 
	birth_district fivey_district duration prev_district move_reason female 
	prev_district_name
****************************************************************/

use "${ugalsms}/NPS_2015/gsec1.dta", clear

rename h_xwgt_W5 p_wgt 
rename hwgt_W5 panel_wgt
rename year irw_year
gen year = 2015 
keep year irw_year HHID urban p_wgt panel_wgt district 

rename HHID hhid

merge 1:m hhid using "${ugalsms}/NPS_2015/gsec2.dta", nogen

merge 1:m hhid pid using "${ugalsms}/NPS_2015/gsec3.dta", keep(match) nogen

rename h3q*D h3q*
rename hhid HHID
rename pid PID

* create previous district 
backfill_prev_dist

save "${ugabuild}/LSMS/Uganda_2015.dta", replace

* prev location only for people less than 17, get previous location for adults from previous wave
use "${ugabuild}/LSMS/Uganda_2013.dta", clear

duplicates tag PID, gen(dup)
drop if dup == 1 & (panel_wgt == 0 | panel_wgt == .)
duplicates drop PID, force
drop prev_* dup urban p_wgt panel_wgt irw_year year age duration move_reason ///
	female fivey_district

rename (region region_name district district_name) prev_=

tempfile uga13
save `uga13'

use "${ugalsms}/NPS_2015/gsec1.dta", clear

rename h_xwgt_W5 p_wgt 
rename hwgt_W5 panel_wgt
rename year irw_year
gen year = 2015 
keep year irw_year HHID urban p_wgt panel_wgt district region 

rename HHID hhid

merge 1:m hhid using "${ugalsms}/NPS_2015/gsec2.dta", nogen

merge 1:m hhid pid using "${ugalsms}/NPS_2015/gsec3.dta", keep(match) nogen

rename h3q*D h3q*
rename hhid HHID
rename pid PID

duplicates tag PID, gen(dup)
drop if dup == 1 & (panel_wgt == 0 | panel_wgt == .)
drop if dup == 1 & (h2q7 == 3 | h2q7 == 4)
duplicates drop PID, force

gen female = h2q3 == 0 | h2q3 == 2
rename h2q8 age

keep HHID district urban *wgt* year irw_year PID age female 

merge m:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta", nogen keep(match)
drop district_name_*

merge 1:1 PID using `uga13', keep(match) nogen

drop if age <= 17

decode region, gen(region_name)

* make duratoin 1 for adult moves
gen duration = district != prev_district
replace duration = 100 if duration == 0

append using "${ugabuild}/LSMS/Uganda_2015.dta"

save "${ugabuild}/LSMS/Uganda_2015.dta", replace

/****************************************************************
	SECTION 7: 2018
	Notes: No information on previous location, use the panel part
	pid_unps t0_hhid
	
	vars: p_wgt panel_wgt 
****************************************************************/

* get location from 2015 round
use "${ugalsms}/NPS_2015/gsec1.dta", clear
rename HHID hhid

keep hhid district
merge m:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta", keep(match) nogen  
drop district_name_*
rename (region district district_name) prev_=

merge 1:m hhid using "${ugalsms}/NPS_2015/gsec2.dta", nogen
rename (hhid pid) (t0_hhid pid_unps)
keep t0_hhid pid_unps prev_* 

tempfile prevloc
save `prevloc'

* get current location
use "${ugalsms}/NPS_2018/HH/GSEC1.dta", clear
rename district_code district 
drop distirct_name
merge m:1 district using "${ugabuild}/Support/uga_dist_xwalk.dta", keep(match) nogen  
drop district_name_2011 district_name_2015 district_name_2013 ///
	district_name_2010 district_name_2009 district_name_2005  
rename district_code_2018 district_2018

merge 1:m hhid using "${ugalsms}/NPS_2018/HH/GSEC2.dta", nogen

gen female = h2q3 == 2
rename h2q8 age 
rename hwgt_W7 panel_wgt
rename wgt p_wgt 
rename year irw_year 
gen year = 2018

merge m:1 t0_hhid pid_unps using `prevloc'

keep if _merge == 3

rename hhid HHID 
keep HHID PID panel_wgt p_wgt age female *district* urban year irw_year *region

save "${ugabuild}/LSMS/Uganda_2018.dta", replace


