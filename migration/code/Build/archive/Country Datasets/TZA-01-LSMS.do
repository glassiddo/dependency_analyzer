/*****************************************************************
PROJECT: 		SSA Environment
				
TITLE:			tzabuild_Tanzania.do
			
AUTHOR: 		Sam Marshall

DATE CREATED:	11/7/2021

LAST EDITED:	11/9/2021

DESCRIPTION: 	Create migration and location dataset


ORGANIZATION:	
				
******************************************************************/

clear
set more off

* Set global directory paths
do "/Users/SMARSH/Dropbox (Personal)/Migration Africa/code/Enviro-globals.do"


/***** Largest Cities *****
Dar Es Salaam = region == 7
Mwanza = region == 19 & (district == 3 | district == 8) 
Arusha = region == 2 & district == 3
Dodoma = region == 1 & district == 5
Mbeya = region == 12 & district == 8
Morogoro = region == 5 & district == 5
Tanga = region == 4 & district == 4
Kahama = region == 17 & district == 4
Tabora = region == 14 & district == 6
Zanzibar = region == 53
Kigoma = region == 16 & district == 4
Sumbawanga = region == 15 & district == 4
Kasulu = region == 16 & district == 2
Songea = region == 10 & district == 4
Moshi = region == 3 & district == 6
*/

/****************************************************************
	SECTION 0A: Func to create locations for 15 largest cities, other urban and rural
****************************************************************/
* arguments 1: wave number, 2: region variable 3: district variable 
capture program drop urbanloc
program define urbanloc
	gen urban_loc = "Dar Es Salaam" if `2' == 7
	
	* differences in region/district codes due to redistricting priot to wave 4
	if `1' <= 3 {
		replace urban_loc = "Mwanza" if `2' == 19 & (`3' == 3 | `3' == 8) 
		replace urban_loc = "Kahama" if `2' == 17 & `3' == 4
		replace urban_loc = "Kasulu" if `2' == 16 & `3' == 2
	}
	else {
		replace urban_loc = "Mwanza" if `2' == 19 & (`3' == 3 | `3' == 6) 
		replace urban_loc = "Kahama" if `2' == 17 & `3' == 5
		replace urban_loc = "Kasulu" if `2' == 16 & `3' == 8
	}
	
	replace urban_loc = "Arusha" if `2' == 2 & `3' == 3
	replace urban_loc = "Dodoma" if `2' == 1 & `3' == 5
	replace urban_loc = "Mbeya" if `2' == 12 & `3' == 8
	replace urban_loc = "Morogoro" if `2' == 5 & `3' == 5
	replace urban_loc = "Tanga" if `2' == 4 & `3' == 4
	
	replace urban_loc = "Tabora" if `2' == 14 & `3' == 6
	replace urban_loc = "Zanzibar" if `2' == 53
	replace urban_loc = "Kigoma" if `2' == 16 & `3' == 4
	replace urban_loc = "Sumbawanga" if `2' == 15 & `3' == 4
	
	replace urban_loc = "Songea" if `2' == 10 & `3' == 4
	replace urban_loc = "Moshi" if `2' == 3 & `3' == 6
	replace urban_loc = "Other Urban" if urban_loc == "" & urban == 1
	replace urban_loc = "Rural" if urban_loc == "" & urban == 0
end

/****************************************************************
	SECTION 0B: Consistent districts
****************************************************************/

import excel "${tzalsms}/District_xwalk_clean.xlsx", ///
	sheet("Consistent") cellrange(A2:S209) firstrow clear
*drop Notes
* rename district_code* district*
save "${tzabuild}/Support/TZA_district_xwalk.dta", replace	

/****************************************************************
	SECTION 0C: Func to merge in consistent district and regions and do renaming of vars
****************************************************************/
capture program drop dist_consist
program define dist_consist 
	args w 
	
	* first do previous district
	rename prev_district district_code_w`w'
	merge m:m district_code_w`w' using "${tzabuild}/Support/TZA_district_xwalk.dta", ///
		keep(match master) nogen
		
	* drop the values for the other waves
	forvalues i = 1/5 {
		if `i' != `w' {
			drop *w`i'
		}
	}
	rename *_cons prev_* 
	rename district_code_w`w' prev_district_w`w' 
	drop region_name_w`w' district_name_w`w'
	* Not run if no labels supplied
	capture decode prev_district_w`w', gen(prev_district_name_w`w')
	capture replace prev_district_name = prev_district_name_w`w' if prev_district_name == ""
	capture replace prev_region_name = prev_district_name_w`w' if prev_region_name == ""
	capture drop prev_district_name_w`w'
	
	* replace missing for no previous district info
	replace prev_district = . if prev_district_w`w' == .
	replace prev_district_name = "" if prev_district_w`w' == .
	replace prev_region = . if prev_district_w`w' == .
	replace prev_region_name = "" if prev_district_w`w' == .
	
	* Now do current district 
	rename curr_district_code_w`w' district_code_w`w'

	merge m:m district_code_w`w' using "${tzabuild}/Support/TZA_district_xwalk.dta", ///
		keep(match) nogen
	* drop the values for the other waves
	forvalues i = 1/5 {
		if `i' != `w' {
			drop *w`i'
		}
	}
	rename *_cons *
	rename district_code_w`w' district_w`w'
	
	label var region "Region Code"
	label var region_name "Region Name"
	label var district "District Code"
	label var district_name "District Name"
	label var prev_region "Previous Region Code"
	label var prev_region_name "Previous Region Name"
	label var prev_district "Previous District Code"
	label var prev_district_name "Previous District Name"
	label var prev_district_w`w' "Previous District Code, Wave `w' Boundaries"
	label var district_w`w' "District Code, Wave `w' Boundaries"
	label var district_name_w`w' "District Name, Wave `w' Boundaries"
	label var region_name_w`w' "Region Name, Wave `w' Boundaries"
	
	*back fill previous district and region if lived there since birth 
	foreach lvl in "district" "region" {
		replace prev_`lvl' = `lvl' if duration == 99 & mi(prev_`lvl')
		capture replace prev_`lvl'_w`w' = `lvl'_w`w' if duration == 99 & mi(prev_`lvl'_w`w')
		replace prev_`lvl'_name = `lvl'_name if duration == 99 & mi(prev_`lvl'_name)
	}
	
	replace prev_district_w`w' = district_w`w' if duration == 99 & mi(prev_district_w`w')
	replace birth_district = district_w`w' if duration == 99 & mi(birth_district)
	* Not perfect for region, but there aren't too many changing regions, and can deal with easily
	* replace birth_region = region if duration == 99 & mi(birth_region) 
	capture drop birth_region 
	
	order region region_name district district_name prev_region prev_region_name ///
	prev_district prev_district_name

end



/****************************************************************
	SECTION 0D: Add wave 4 IDs to crosswalk
****************************************************************/

use "${tzalsms}/Panel/upd4_hh_b.dta", clear
keep UPI round r_hhid r_id
reshape wide r_hhid r_id, i(UPI) j(round)
rename (r_hhid1 r_id1 r_hhid2 r_id2 r_hhid3 r_id3 r_hhid4 r_id4) ///
	(y1_hhid indidy1 y2_hhid indidy2 y3_hhid indidy3 y4_hhid indidy4)
	
merge m:1 y1_hhid indidy1 y2_hhid indidy2 y3_hhid indidy3 using ///
	"${tzalsms}/NPS_2012/NPSY3.PANEL.KEY.dta", gen(panel_incl)
	
label define svy_incl 1 "Wave 4 Panel Only" 2 "Wave 3 Panel Only" ///
	3 "Wave 3 and 4 Panel" 4 "Wave 4 and 5 Panel" 5 "Wave 3 and 5 Panel" ///
	6 "Wave 3-5 Panel" 7 "Wave 5 Only"
label values panel_incl svy_incl
	
* There are some issues with id matching across waves in the two panel parts 
* So I will go with the original matching for those instances
duplicates tag y3_hhid indidy3, gen(dup3)

drop if dup3 == 1 & UPI == .

duplicates tag y2_hhid indidy2, gen(dup2)

drop if dup2 == 1 & UPI == .

* no additional duplicates for wave 1

drop dup* 

save "${tzabuild}/Support/TZA_id_xwalk.dta", replace	

use "${tzalsms}/NPS_2019/HH_SEC_B.dta", clear

rename (sdd_hhid sdd_indid) (y5_hhid indidy5)
rename hh_b06 indidy4
replace indidy4 = . if indidy4 == 99

keep y5_hhid indidy5 y4_hhid indidy4
preserve
drop if indidy4 == .
merge 1:m y4_hhid indidy4 using "${tzabuild}/Support/TZA_id_xwalk.dta"
replace panel_incl = 4 if panel_incl == 1 & _merge == 3
replace panel_incl = 5 if panel_incl == 2 & _merge == 3
replace panel_incl = 6 if panel_incl == 3 & _merge == 3

save "${tzabuild}/Support/TZA_id_xwalk.dta", replace	

restore
keep if indidy4 == .
append using "${tzabuild}/Support/TZA_id_xwalk.dta"
replace panel_incl = 7 if panel_incl == .

gen xwalk_id = _n
drop _merge
save "${tzabuild}/Support/TZA_id_xwalk.dta", replace	



/****************************************************************
	SECTION 1: Wave 1-2008
****************************************************************/

use "${tzalsms}/NPS_2008/SEC_A_T.dta", clear
merge 1:1 hhid using "${tzalsms}/NPS_2008/HH.Geovariables_Y1.dta", nogen
merge 1:m hhid using "${tzalsms}/NPS_2008/SEC_B_C_D_E1_F_G1_U.dta", nogen

rename (hhid sbmemno) (y1_hhid indidy1)
rename hh_weight_trimmed panel_wgt
rename (sa2q18m sa2q18y) (irw_month irw_year)

gen urban = rural == "Urban"
rename locality rum
gen year = 2008
gen female = sbq2 == 2
rename sbq4 age
rename (sbq24 sbq25district sbq26 sbq27district) ///
	(duration prev_district move_reason birth_district)

keep y1_hhid indidy1 panel_wgt irw_* year female age urban rum clusterid ///
	district ward ea ea_id lat_modified lon_modified duration ///
	prev_district move_reason birth_district region
	
* create consistent district codes
gen curr_district_code_w1 = 10 * region + district
drop district region

dist_consist 1

* add in id xwalk 	
merge 1:m y1_hhid indidy1 using "${tzabuild}/Support/TZA_id_xwalk.dta", keep(match) nogen

* create the smaller set of urban and rural locations
* prev_district_code is concatenation of prev region and district.
* urbanloc 1 region district

rename move_reason mr_code
decode mr_code, gen(move_reason)
	
save "${tzabuild}/LSMS/Tanzania_2008.dta", replace	

/****************************************************************
	SECTION 2: Wave 2-2010
****************************************************************/

use "${tzalsms}/NPS_2010/HH_SEC_A.dta", clear
merge m:1 clusterid using "${tzalsms}/NPS_2010/TZY2.EA.Offsets.dta", nogen
merge 1:m y2_hhid using "${tzalsms}/NPS_2010/HH_SEC_B.dta", nogen

rename y2_weight panel_wgt
rename (hh_a18_month hh_a18_year) (irw_month irw_year)

gen urban = y2_rural == 0
gen year = 2010
gen female = hh_b02 == 2
rename hh_b04 age
rename (hh_b25 hh_b26_2 hh_b26_3 hh_b27 hh_b28_2 hh_b28_3) ///
	(duration prev_region prev_district move_reason birth_region birth_district)
	
keep y2_hhid indidy2 panel_wgt irw_* year female age urban rum clusterid ///
	strataid region district ward ea lat_modified lon_modified duration ///
	prev_region prev_district move_reason birth_region birth_district
	
* create consistent district codes
gen curr_district_code_w2 = 10 * region + district
drop district region

rename prev_district prev_district_code
gen prev_district = 10 * prev_region + prev_district_code if prev_district_code < 10
replace prev_district = prev_district_code if prev_district_code > 100
drop prev_district_code prev_region

* one instance of prev district = 58 which does not exist, the person moved 11
* years ago, so they will only ever have their current district, so replace with that
replace prev_district = curr_district_code_w2 if prev_district == 58

dist_consist 2

* add in id xwalk 	
merge 1:m y2_hhid indidy2 using "${tzabuild}/Support/TZA_id_xwalk.dta", keep(match) nogen

/* **** Not run at the moment *****
* create the smaller set of urban and rural locations
urbanloc 2 region district
rename urban_loc urban_loc_curr

urbanloc 2 prev_region prev_district
rename urban_loc urban_loc_prev
replace urban_loc_prev = urban_loc_curr if duration_code > 2
*/

rename move_reason mr_code
decode mr_code, gen(move_reason)
	
save "${tzabuild}/LSMS/Tanzania_2010.dta", replace	

* hh_a11
* ORIGINAL HOUSEHOLD IN SAME LOCATION..1 ►14
* ORIGINAL HOUSEHOLD IN NEW LOCATION...2 ►14
* SPLIT-OFF HOUSEHOLD..................3

/****************************************************************
	SECTION 3: Wave 3-2012
****************************************************************/

use "${tzalsms}/NPS_2012/HH_SEC_A.dta", clear
merge 1:1 y3_hhid using "${tzalsms}/NPS_2012/HouseholdGeovars_Y3.dta", nogen
merge 1:m y3_hhid using "${tzalsms}/NPS_2012/HH_SEC_B.dta", nogen

rename y3_weight panel_wgt
rename (hh_a18_2 hh_a18_3) (irw_month irw_year)
rename (lat_dd_mod lon_dd_mod) (lat_modified lon_modified)
rename (hh_a01_1 hh_a01_2 hh_a02_1 hh_a02_2 hh_a03_1 hh_a04_1) ///
	(region region_name district district_name ward ea)

gen urban = y3_rural == 0
gen year = 2012
gen female = hh_b02 == 2
rename hh_b04 age
rename (hh_b26 hh_b27_1 hh_b27_2 hh_b27_3 hh_b28 hh_b29_1 hh_b29_2 hh_b29_3) ///
	(duration prev_district_name prev_region prev_district move_reason ///
	birth_district_name birth_region birth_district)
	
keep y3_hhid indidy3 panel_wgt irw_* year urban female age clusterid ///
	strataid region region_name district district_name ward ea lat_modified ///
	lon_modified duration prev_district_name prev_region prev_district ///
	move_reason birth_district_name birth_region birth_district
	
* adjust birth location 
replace birth_region = . if birth_region == 96
replace birth_district = 10* birth_region + birth_district
	
* create consistent district codes
gen curr_district_code_w3 = 10 * region + district
replace curr_district_code_w3 = 100 * region + district if district == 10
drop district region region_name district_name

* create prev district code
replace prev_region = . if prev_region == 96
rename prev_district prev_district_code
gen prev_district = 10 * prev_region + prev_district_code if prev_district_code < 10
replace prev_district = prev_district_code if prev_district_code > 100
drop prev_district_code prev_region

* prev district is missing for people who moved in from out of country, so replace with 999
replace prev_district = 999 if prev_district_name != "" & prev_district == .
rename prev_district_name stub_prev_district_name_w3

* one instance of prev district = 78 which does not exist, the person moved 40
* years ago, so they will only ever have their current district, so replace with that
replace prev_district = curr_district_code_w3 if prev_district == 78


dist_consist 3


replace prev_district_name = stub_prev_district_name_w3 if prev_district_name == ""
drop stub_prev_district_name_w3

* add in id xwalk 
merge 1:m y3_hhid indidy3 using "${tzabuild}/Support/TZA_id_xwalk.dta", keep(match) nogen

rename move_reason mr_code
decode mr_code, gen(move_reason)
	
save "${tzabuild}/LSMS/Tanzania_2012.dta", replace	

/****************************************************************
	SECTION 4: Wave 4-2014
****************************************************************/

***** 4.1 make the file for the new observations *****
use "${tzalsms}/NPS_2014/hh_sec_a.dta", clear
merge m:1 clusterid using "${tzalsms}/NPS_2014/npsy4.ea.offset.dta", nogen
merge 1:m y4_hhid using "${tzalsms}/NPS_2014/hh_sec_b.dta", nogen

rename y4_weights panel_wgt
rename (hh_a18_2 hh_a18_3) (irw_month irw_year)
rename (hh_a01_1 hh_a01_2 hh_a02_1 hh_a02_2 hh_a03_1 hh_a03_3a hh_a03_3b hh_a04_1) ///
	(region region_name district district_name ward village village_name ea)

gen urban = clustertype == 2
rename wardtype rum
gen year = 2014
gen female = hh_b02 == 2
rename hh_b04 age
rename (hh_b26 hh_b27_1 hh_b27_2 hh_b27_3 hh_b28 hh_b29_1 hh_b29_2 hh_b29_3) ///
	(duration prev_district_name prev_region prev_district move_reason ///
	birth_district_name birth_region birth_district)
	
keep y4_hhid indidy4 panel_wgt irw_* year urban rum female age domain ///
	clusterid strataid region region_name district district_name ward village ///
	village_name ea lat_modified lon_modified duration prev_district_name ///
	prev_region prev_district move_reason birth_district_name birth_region ///
	birth_district
	
* adjust birth location 
replace birth_district = 10* birth_region + birth_district
	
* create consistent district codes
gen curr_district_code_w4 = 10 * region + district
replace curr_district_code_w4 = 100 * region + district if district == 10
drop district region region_name district_name

* create prev district code
rename prev_district prev_district_code
gen prev_district = 10 * prev_region + prev_district_code if prev_district_code < 10
replace prev_district = prev_district_code if prev_district_code > 100
drop prev_district_code prev_region

* prev district is missing for people who moved in from out of country, so replace with 999
replace prev_district = 999 if prev_district_name != "" & prev_district == .
rename prev_district_name stub_prev_district_name_w4
	
dist_consist 4

replace prev_district_name = stub_prev_district_name_w4 if prev_district_name == ""
drop stub_prev_district_name_w4

* add in id xwalk 
merge 1:m y4_hhid indidy4 using "${tzabuild}/Support/TZA_id_xwalk.dta", keep(match) nogen

* both samples are scaled to be nationally representative, so scale by relative sample size
*16285 and 4742, so do 77% and 23%
replace panel_wgt = .77 * panel_wgt
	
save "${tzabuild}/LSMS/Tanzania_2014.dta", replace	


***** 4.2 Panel part ******
* No lat and lon data 
use "${tzalsms}/NPS_2014_Ext/hh_sec_a.dta", clear
merge 1:m y4_hhid using "${tzalsms}/NPS_2014_Ext/hh_sec_b.dta", nogen

rename y4_weights panel_wgt
rename (hh_a18_2 hh_a18_3) (irw_month irw_year)
rename (hh_a01_1 hh_a01_2 hh_a02_1 hh_a02_2 hh_a03_1 hh_a03_3a hh_a04_1) ///
	(region region_name district district_name ward village ea)
	

gen urban = y4_rural == 0
gen year = 2014
gen female = hh_b02 == 2
rename hh_b04 age
rename (hh_b26 hh_b27_1 hh_b27_2 hh_b27_3 hh_b28 hh_b29_1 hh_b29_2 hh_b29_3) ///
	(duration prev_district_name prev_region prev_district move_reason ///
	birth_district_name birth_region birth_district)
	
keep y4_hhid indidy4 panel_wgt irw_* year urban female age domain ///
	clusterid strataid region* district* ward village ///
	ea duration prev* move_reason birth*
	
* adjust birth location 
replace birth_district = 10* birth_region + birth_district
	
* create consistent district codes
gen curr_district_code_w4 = 10 * region + district
replace curr_district_code_w4 = 100 * region + district if district == 10
drop district region region_name district_name

* create prev district code
rename prev_district prev_district_code
gen prev_district = 10 * prev_region + prev_district_code if prev_district_code < 10
replace prev_district = prev_district_code if prev_district_code > 100
drop prev_district_code prev_region

* prev district is missing for people who moved in from out of country, so replace with 999
replace prev_district = 999 if prev_district_name != "" & prev_district == .
rename prev_district_name stub_prev_district_name_w4
	
dist_consist 4
		
replace prev_district_name = stub_prev_district_name_w4 if prev_district_name == ""
drop stub_prev_district_name_w4

* add in id xwalk 
merge 1:m y4_hhid indidy4 using "${tzabuild}/Support/TZA_id_xwalk.dta", keep(match) nogen

* both samples are scaled to be nationally representative, so scale by relative sample size
*16285 and 4742, so do 77% and 23%
replace panel_wgt = .23 * panel_wgt

append using "${tzabuild}/LSMS/Tanzania_2014.dta"

rename move_reason mr_code
decode mr_code, gen(move_reason)
	
save "${tzabuild}/LSMS/Tanzania_2014.dta", replace	

/****************************************************************
	SECTION 5: Wave 5-2019
****************************************************************/

use "${tzalsms}/NPS_2019/HH_SEC_A.dta", clear
*merge 1:1 sdd_hhid using "${tzalsms}/NPS_2019/HH_SEC_WQT.dta", nogen
merge 1:m sdd_hhid using "${tzalsms}/NPS_2019/HH_SEC_B.dta", nogen

rename (sdd_hhid sdd_indid) (y5_hhid indidy5)
rename hh_b06 indidy4
replace indidy4 = . if indidy4 == 99
rename sdd_weights panel_wgt

gen irw_year = substr(hh_a18, 1, 4)
destring irw_year, replace

gen irw_month = substr(hh_a18, 6, 2)
destring irw_month, replace

* t0_region t0_district t0_ward_code t0_ea_codee
rename (hh_a01_1 hh_a02_1 hh_a03_1 hh_a03_3a hh_a04_1) ///
	(region district ward village ea)

gen urban = sdd_rural == 2
gen year = 2019
gen female = hh_b02 == 2
rename hh_b04 age
rename (hh_b26 hh_b27_2 hh_b27_3 hh_b28 hh_b29_2 hh_b29_3) ///
	(duration prev_region prev_district move_reason birth_region birth_district)
	
keep y5_hhid indidy5 y4_hhid indidy4 panel_wgt irw_* year urban female age ///
	domain t0* hh_a10 hh_tracking_status ///
	clusterid strataid region  district ward village ///
	ea duration  prev* move_reason birth*
	
drop t0_ward t0_village t0_ea t0_mtaa t0_location t0_head t0_headphone

* fill in location info from t0 info if household hasn't moved
replace region = t0_region if region == . & hh_a10 == 1 & hh_tracking_status == 1
replace district = t0_district if district == . & hh_a10 == 1 & hh_tracking_status == 1
replace ward = t0_ward_code if ward == . & hh_a10 == 1 & hh_tracking_status == 1
replace ea = t0_ea_codee if ea == . & hh_a10 == 1 & hh_tracking_status == 1

* strataid matches t0 region for those who haven't moved 
replace region = t0_region if region == .
replace district = t0_district if district == . 
drop t0* hh_a10 hh_tracking_status region prev_region 

drop if district == .
	
* create consistent district codes
rename district curr_district_code_w5

dist_consist 5

* add in id xwalk 
merge 1:m y5_hhid indidy5 using "${tzabuild}/Support/TZA_id_xwalk.dta", keep(match) nogen

rename move_reason mr_code
decode mr_code, gen(move_reason)
	
save "${tzabuild}/LSMS/Tanzania_2019.dta", replace	


