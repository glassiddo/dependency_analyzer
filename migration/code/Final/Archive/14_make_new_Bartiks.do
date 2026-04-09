****** This file (written by Sam initially, updated by Liam) generates various Bartik / Shift-share instruments
****** Since we have worked with a lot of different shifts and shares, it only generates a subset of possible ones 
****** I.e. it generates all combinations of the following shifts with the following shares:
global shares "d m_d m_od m_od_L dXp_L dtXp_L DXp_L DtXp_L DurXp_L DurtXp_L"
global shifts "delta_nonag_ann_sum_pa delta_urban_pop_ann gr_nonag_ann gr_urban_pop_ann delta_nonag_ann delta_log_nonag_ann p_gr_real AR1_residual"
foreach l in lv la lv_ra la_ra tlv tla  {
	global shifts "$shifts gr_n`l'  "
}
/*
foreach m in Prop2 Type1 Type2 Type3 Type4 Type5 {
	global shifts "$shifts gr_built_`m'"
} 
*/
***** These are a bit cryptic, but are defined later on when the shifts and shares are each generated


*** Part 1 - merge some data necessary to generate the shares and save it in an 'origin' files
use "${final}/employment.dta", clear
merge 1:1 ipums_id using "${final}/population.dta", nogen keep(match) 
merge 1:1 ipums_id using "${final}/urban.dta", nogen keep(master match) keepusing(total_area)
rename * *_o
rename country_name_o country_name 
tempfile origin
save `origin'


*** Part 2 - merge data necessary to generate the shifts and save this in a file called shifts (which is also used later when we do the inversion)
use "${final}/employment.dta", clear
merge 1:1 ipums_id using "${final}/population.dta", nogen keep( match) 
merge 1:1 ipums_id using "${final}/nightlight.dta", nogen keep(master match)
merge 1:1 ipums_id using "${final}/mine_instrument.dta",  nogen  keep(master match)
merge 1:1 ipums_id using "${final}/MODIS_LC_wide.dta",  nogen  keep(master match) keepusing(*built*)

recode AR1_residual .=0

****  generate a bunch of shifts.... I have labeled the ones that are currently used to produce tables
gen gr_nonag_ann=delta_nonag_ann/bl_nonag
label var gr_nonag_ann "non-ag population growth rate"
*winsor2 gr_nonag_ann, replace

* This one I don't think makes much sense but was what we were doing before
gegen sum_pa=sum(bl_pa), by(country_name)
gen delta_nonag_ann_sum_pa=delta_nonag_ann/sum_pa



gen gr_urban_pop_ann=delta_urban_pop_ann/bl_urban_pop

foreach m in Prop2 Type1 Type2 Type3 Type4 Type5 {
	gen gr_built_`m'=delta_built_LC_`m'/built_LC_`m'
}

label var bl_nla "lit area"
foreach l in lv la lv_ra la_ra tlv tla  {
	gen gr_n`l'=delta_n`l'/bl_n`l'
	local temp: variable label bl_n`l'
	label var gr_n`l' "`temp' growth rate"
	sum gr_n`l'
	winsor2  gr_n`l', replace
	sum gr_n`l'
}


**** Since some countries don't have urban or non-ag population, we use the relevant one insted for these countries
replace bl_nonag=bl_urban_pop if Country=="CMR" | Country=="SAF"
replace bl_urban_pop=bl_nonag if inlist(Country,"BWA","BFA","ZMB") 
foreach var in delta_ delta_log_ gr_ {
	replace `var'nonag_ann=`var'urban_pop_ann if Country=="CMR" | Country=="SAF"
	replace `var'urban_pop_ann=`var'nonag_ann if inlist(Country,"BWA","BFA","ZMB") 
}

keep country_name ipums_id $shifts bl_pop


*** Rename everythign with a _d on end just so it doesn't get confusing if we merge this with the origin file  (not sure if we currently do this)
rename * *_d
rename country_name_d country_name 
save "${final}/shifts.dta", replace


*** Part 3 - start off with a matrix of origin by destination data, then merge the data neccessary to create the shares - i.e. the origin and the shifts - then generate shift shares
use "${final}/migration.dta", clear
merge 1:1 ipums_id_d ipums_id_o using "${final}/dist_mat.dta", nogen keep(master match) 
merge m:1 ipums_id_o using `origin', nogen keep(3)
merge m:1 ipums_id_d using "${final}/shifts.dta", nogen keep(3)

label var m_od "origin to destination migration"


***** Rename a bunch of variables - all share variables end with    `shares'
ren inv_osm_dist_shares D_shares
ren inv_osm_dur_shares Dur_shares
ren d_shares m_d_shares
ren o_shares m_o_shares
ren inv_dist_shares d_shares
ren m_od m_od_shares


*  Rescale the duration variable to be on similar scale to distance 
replace Dur_shares=Dur_shares/60

** For each of the different distance shares, generate `market access' shares
foreach share in D d Dur {
	** First by simply multiplying by destination population
	gen `share'Xp_shares=`share'_shares*bl_pop_d
	** Then by first taking a transformation of the distance variable (since this is often done)
	gen `share'tXp_shares=(`share'_shares)^(.5)*bl_pop_d
}
label var dXp_shares "destination population over distance (as crow flies)"
label var DXp_shares "destination population over distance (by road)"
label var DurXp_shares "destination population over distance (time to travel)"


***** Generate various shares based on old ones that are divided through by land area (since we are sort of looking to instrument changes in population / farming density)
foreach share in m_od dXp dtXp DXp DtXp DurXp DurtXp{
	gen `share'_L_shares=`share'_shares/total_area_o
	local temp: variable label `share'_shares
	label var `share'_L_shares "`temp' over origin land area"
}

*** Saves the share variables for later since we need them when doing the inversion
savesome  ipums_id_o ipums_id_d *shares using "${final}/shares.dta", replace


*** Combine the shifts and shares togetehr to make the component parts of the shift shares
foreach shift in $shifts {
	disp "shift = `shift'"
	foreach share in $shares {
		disp "share = `share'"
		gen B`share'_`shift' = `share'_shares *(`shift'_d)
	}
}

*** Collapse down the matrix at the origin level to get the shift share instruments (the B variables) and the sum of the shares (the *shares varaiables) which we need to control for
gcollapse   (sum) B* *shares, by(ipums_id_o country_name)
	
rename ipums_id_o ipums_id 

foreach share in $shares {
		ren `share'_shares `share'_shares_sum
}
save "${final}/new_bartiks" , replace

