****** This file (written by Sam initially, updated by Liam) generates various Bartik / Shift-share instruments
****** Since we have worked with a lot of different shifts and shares, it only generates a subset of possible ones 
****** I.e. it generates all combinations of the following shifts with the following shares:
*global shares "d m_d m_od m_od_L dXp_L dtXp_L" /*DXp_L DtXp_L DurXp_L DurtXp_L*/
*global shares "m_od_L dXp_L dtXp_L DXp_L DtXp_L DurXp_L  DurtXp_L logdXp_L logDXp_L logDurXp_L" 
*global shares "m_od_L dXp_L dtXp_L dtXnap_L DtXnap_L DurtXnap_L" 
*global shares "m_od_L tcXp_L tcXnap_L dtXnap_L dtXp_L" 
*global shares "m_od_L  DXp_L DurXp_L     D_S_Xp_L D_C_Xp_L Dur_C_Xp_L Dur_S_Xp_L m_od_p  DXp DurXp  DXp_p DurXp_p" 
global shares "m_od_L  DXp_L DurXp_L  DXp DurXp  DXp_p DurXp_p" 
*global ishares "d m_d m_od m_od_L dXp_L d_C_Xp_L d_S_Xp_L "
global ishares "d m_d m_od m_od_L dXp_L  "

*global shifts "delta_nonag_ann_sum_pa delta_urban_pop_ann gr_nonag_ann gr_urban_pop_ann delta_nonag_ann delta_log_nonag_ann delta_log_nlv_ra delta_log_nlv "
global mine_shifts "gr_p_mine_asm"
global shifts " gr_nonag_ann gr_urban_pop_ann gr_p_mine_asm" 
foreach l in tlv tlla tlv_ra tlla_ra  {
	global shifts "$shifts gr_n`l' "
}
// foreach l in prop2 type1 type2 type3 type4 type5 /*modis_lc*/ {
// 	global shifts "$shifts gr_built_`l'"
// } 
// foreach l in vtotal vnres atotal anres /*ghsl*/  {
// 	global shifts "$shifts gr_bu`l' "
// }
foreach l in nag_cd_hata nag_cd_hatb serv_cd_hata serv_cd_hatb ind_cd_hata ind_cd_hatb /*ghsl*/  {
	global shifts "$shifts gr_gdp_`l' "
}

global nl_cutoff 0 // NOTE - given that we changed the nl calculation, now the previous threshold (0.3) doesnt really filter as the values are higher, need to come up with a new threshold once the nightlight situation is clearer


***** These are a bit cryptic, but are defined later on when the shifts and shares are each generated

*** Part 1 - merge some data necessary to generate the shares and save it in an 'origin' files
use "${final}/employment.dta", clear
merge 1:1 ipums_id using "${final}/population.dta", nogen keep(match) 
merge 1:1 ipums_id using "${final}/urban.dta", nogen keep(master match) keepusing(total_area)
rename * *_o
rename country_o country
tempfile origin
save `origin'


*** Part 2 - merge data necessary to generate the shifts and save this in a file called shifts (which is also used later when we do the inversion)
*** This does it for wave 1, when we have multiple shifts to choose from
use "${final}/employment.dta", clear
merge 1:1 ipums_id using "${final}/population.dta", nogen keep( match) 
merge 1:1 ipums_id using "${final}/nightlight.dta", nogen 
gen wave=1
merge 1:1 ipums_id wave using "${final}/mine_instrument.dta",  nogen keep(master match) 
//merge 1:1 ipums_id wave using "${final}/builtup_rf.dta", nogen keep(master match)
//merge 1:1 ipums_id wave using "${final}/MODIS_LC_wide.dta",  nogen  keep(master match) keepusing(*barea*)

* set NAs from the mine instruments as zeros
// recode AR1_residual .=0
// recode gr_p_mine .=0
recode gr_p_mine_asm .=0
// label var p_gr_nom "weighted average of nominal price change of mines"
// label var p_gr_real "weighted average of real price change of mines"
// label var p_gr_real_asm "weighted average of real price change of mines inc. ASM"
// label var p_gr_real_asm "weighted average of real price change of mines inc."

****  generate a bunch of shifts.... I have labeled the ones that are currently used to produce tables
gen gr_nonag_ann=delta_nonag_ann/bl_nonag
label var gr_nonag_ann "non-ag population growth rate"
*winsor2 gr_nonag_ann, replace

* This one I don't think makes much sense but was what we were doing before
// gen sum_pa=sum(bl_pa), by(country_name)
// gen delta_nonag_ann_sum_pa=delta_nonag_ann/sum_pa

gen gr_urban_pop_ann=delta_urban_pop_ann/bl_urban_pop

// foreach m in prop2 type1 type2 type3 type4 type5 {
// 	gen gr_barea_`m'=delta_barea_lc_`m'/barea_lc_`m'
// }

label var bl_ntlla "lit area"
label var bl_ntlv_ra "night lights"
foreach l in tlv tlla tlv_ra tlla_ra {
	gen gr_n`l'=delta_n`l'/bl_n`l'
	local temp: variable label bl_n`l'
	label var gr_n`l' "`temp' growth rate"
	sum gr_n`l'
	
	*winsor2  gr_n`l', replace
	sum gr_n`l'
	
	replace gr_n`l'=0 if bl_n`l'<=${nl_cutoff}
}

foreach l in nag_cd_hata nag_cd_hatb serv_cd_hata serv_cd_hatb ind_cd_hata ind_cd_hatb {
	gen gr_gdp_`l'=delta_gdp_`l'/bl_gdp_`l'
	local temp: variable label bl_gdp_`l'
	label var gr_gdp_`l' "`temp' growth rate"
	sum gr_gdp_`l'
}

// foreach l in vtotal atotal vnres anres {
// 	gen gr_bu`l'=delta_bu`l'/bl_bu`l'
// 	local temp: variable label bl_bu`l'
// 	label var gr_bu`l' "`temp' growth rate"
// 	sum gr_bu`l'
// }

**** Since some countries don't have urban or non-ag population, we use the relevant one insted for these countries
replace bl_nonag=bl_urban_pop if country=="CMR" | country=="ZAF"
replace bl_urban_pop=bl_nonag if inlist(country,"BWA","BFA","ZMB") 
foreach var in delta_ delta_log_ gr_ {
	replace `var'nonag_ann=`var'urban_pop_ann if country=="CMR" | country=="ZAF"
	replace `var'urban_pop_ann=`var'nonag_ann if inlist(country,"BWA","BFA","ZMB") 
}
keep country_name ipums_id $shifts 

*** Rename everythign with a _d on end just so it doesn't get confusing if we merge this with the origin file  (not sure if we currently do this)
rename * *_d
rename country_name_d country_name 
gen wave=1
save "${final}/shifts_1.dta", replace

*** This does it for wave 2, when have much fewer
use  "${final}/nightlight_period2.dta", clear
gen wave=2
gduplicates tag ipums_id, gen(t)
drop if t>0 & delta_log_ntlv==.
drop t
merge 1:1 ipums_id wave using "${final}/mine_instrument.dta",  nogen  keep(master match)
//merge 1:1 ipums_id wave using "${final}/MODIS_LC_wide.dta",  nogen  keep(master match) keepusing(*barea*)
//merge 1:1 ipums_id wave using "${final}/builtup_rf.dta", nogen keep(master match)
// foreach m in prop2 type1 type2 type3 type4 type5 {
// 	gen gr_barea_`m'=delta_barea_lc_`m'/barea_lc_`m'
// }

label var bl_ntlla "lit area"
label var bl_ntlv_ra "night lights"
label var bl_gdp_nag_cd_hata "gdp non ag short model"
label var bl_gdp_nag_cd_hatb "gdp non ag long model"
label var bl_gdp_ind_cd_hata "gdp industry short model"
label var bl_gdp_ind_cd_hatb "gdp industry long model"
label var bl_gdp_serv_cd_hata "gdp service short model"
label var bl_gdp_serv_cd_hatb "gdp service short model"
// label var bl_buvtotal "total builtup volume"
// label var bl_buatotal "total builtup area"
// label var bl_buvnres "total builtup volume non residential"
// label var bl_buanres "total builtup area non residential"

foreach l in tlv tlla tlv_ra tlla_ra  {
	gen gr_n`l'=delta_n`l'/bl_n`l'
	local temp: variable label bl_n`l'
	label var gr_n`l' "`temp' growth rate"
	sum gr_n`l'
	*winsor2  gr_n`l', replace
	replace gr_n`l'=0 if bl_n`l'<=${nl_cutoff}
	sum gr_n`l'
}

foreach l in nag_cd_hata nag_cd_hatb serv_cd_hata serv_cd_hatb ind_cd_hata ind_cd_hatb {
	gen gr_gdp_`l'=delta_gdp_`l'/bl_gdp_`l'
	local temp: variable label bl_gdp_`l'
	label var gr_gdp_`l' "`temp' growth rate"
	sum gr_gdp_`l'
}

// foreach l in vtotal atotal vnres anres {
// 	gen gr_bu`l'=delta_bu`l'/bl_bu`l'
// 	local temp: variable label bl_bu`l'
// 	label var gr_bu`l' "`temp' growth rate"
// 	sum gr_bu`l'
// }

keep ipums_id delta* gr*


*** Rename everythign with a _d on end just so it doesn't get confusing if we merge this with the origin file  (not sure if we currently do this)
rename * *_d

save "${final}/shifts_2.dta", replace

*** Part 3 - start off with a matrix of origin by destination data, then merge the data neccessary to create the shares - i.e. the origin and the shifts - then generate shift shares
foreach w in 2 1 {
	disp "now wave `w'"
*need to distinguish internal case from the one including international 
use "${final}/full_migration_census`w'.dta", clear 


*** note for Ghana don't have any migration data in second census, so want to use 1st census
if `w'==2 {
	append using "${final}/full_migration_census1.dta", gen(temp)
	drop if temp==1 & !inlist(country_name, "Ghana", "Ivory Coast")
	drop temp
}

/*
use "${final}/full_migration_census1.dta", clear 

merge 1:1 ipums_id_d ipums_id_o using "${final}/full_migration_census1.dta", nogen
*/
gen intm_od = m_od
replace m_od = . if country_name != prev_country_name

merge 1:1 ipums_id_d ipums_id_o using "${final}/full_distance_census`w'.dta", nogen keep(master match) 

merge 1:1 ipums_id_d ipums_id_o using "${final}/dist_mat.dta", nogen keep(master match) keepusing(*osm*)

gen intinv_dist_shares = inv_dist_shares
*gen intinv_osm_dist_shares = inv_osm_dist_shares
*gen intinv_osm_dur_shares = inv_osm_dur_shares
replace inv_dist_shares = . if country_name != prev_country_name
*replace inv_osm_dist_shares = . if country_name != prev_country_name
*replace inv_osm_dur_shares = . if country_name != prev_country_name

merge m:1 ipums_id_o using `origin', nogen keep(3)
merge m:1 ipums_id_d using "${final}/shifts_`w'.dta", nogen keep(3)
ren ipums_id_d ipums_id
*** Note that should technically use endline pop for some wave 2 places
merge m:1 ipums_id using "${final}/population.dta", nogen keep(master match) keepusing(*l_pop *l_urban_pop)
merge m:1 ipums_id using "${final}/employment.dta", nogen keep(master match) keepusing(*l_nonag)
merge m:1 ipums_id using "${final}/mines_loc.dta",  nogen keep(master match)

foreach p in pop urban_pop nonag {
	if `w'==2 {
		replace bl_`p'=el_`p'
	}
}

replace bl_urban_pop=bl_nonag if inlist(country,"BWA","BFA","ZMB") 
replace bl_nonag=bl_urban_pop if country=="CMR" | country=="ZAF"

ren ipums_id ipums_id_d 
ren has_mine_asm has_mine_asm_d

ren bl_pop bl_pop_d
ren bl_urban_pop bl_urban_pop_d
ren bl_nonag bl_nonag_d


label var m_od "origin to destination migration, 0 if international migration"
label var intm_od "origin to destination migration, including international migration"


*** drop country_name and rename prev_country_name as country_name, bc that's the country of origin. if we don't do that, for each origin, we would have a row for each destination country with the bartiks, which is not what we want
drop country_name
ren prev_country_name country_name

***** Rename a bunch of variables - all share variables end with    `shares'
ren d_shares m_d_shares
ren o_shares m_o_shares
ren intd_shares im_d_shares
ren into_shares im_o_shares
ren inv_dist_shares d_shares
ren intinv_dist_shares id_shares
ren intm_od im_od_shares
ren m_od m_od_shares
ren inv_osm_dist_shares D_shares
ren inv_osm_dur_shares Dur_shares
*ren intinv_osm_dist_shares iD_shares
*ren intinv_osm_dur_shares iDur_shares


*gen tc_shares=1/(1+ 0.17*log(1/d_shares)) 
*gen tc_shares=1/(log(1+1/d_shares)) 
*
*  Rescale the duration variable to be on similar scale to distance 
*replace Dur_shares=Dur_shares/60

** For each of the different distance shares, generate `market access' shares
*foreach share in d /* D Dur */ tc{
quietly{
	foreach share in d D Dur {
	** First by simply multiplying by destination population
	gen `share'Xp_shares=`share'_shares*bl_pop_d /100
	gen `share'Xp_shares_b_mines=`share'_shares*bl_pop_d /100 * has_mine_asm_d
	gen `share'Xp_shares_log_mines=`share'_shares*bl_pop_d /100 * log_mines_asm
	*gen `share'Xnap_shares=`share'_shares*bl_urban_pop_d
	*gen `share'Xnap_shares=`share'_shares*bl_nonag_d /100
	cap gen i`share'Xp_shares= i`share'_shares*bl_pop_d /100
	cap gen i`share'Xp_shares_b_mines= i`share'_shares*bl_pop_d /100 * has_mine_asm_d
	cap gen i`share'Xp_shares_log_mines= i`share'_shares*bl_pop_d /100 * log_mines_asm
	** Then by first taking a transformation of the distance variable (since this is often done)
*	gen `share'tXp_shares=(`share'_shares)^(.3)*bl_pop_d /100
	*gen `share'tXnap_shares=(`share'_shares)^(.3)*bl_nonag_d /100
*	cap gen i`share'tXp_shares=(i`share'_shares)^(.3)*bl_pop_d /100
*	gen `share'_C_Xp_shares=`share'_shares^(-0.17)*bl_pop_d /100
*	cap gen i`share'_C_Xp_shares=i`share'_shares^(-0.17)*bl_pop_d /100
*	gen `share'_S_Xp_shares=1/(1+exp(-.104+0.437*log(`share'_shares+1)))*bl_pop_d /100
*	cap gen i`share'_S_Xp_shares=1/(1+exp(-.104+0.437*log(i`share'_shares+1)))*bl_pop_d /100
	}
}


/*label var dXp_shares "destination population over distance (as crow flies)"    
label var idXp_shares "destination population over distance (as crow flies), international"
label var DXp_shares "destination population over distance (by road)"
label var iDXp_shares "destination population over distance (by road), international"
label var DurXp_shares "destination population over distance (time to travel)"
label var iDurXp_shares "destination population over distance (time to travel), international" */

***** Generate various shares based on old ones that are divided through by land area (since we are sort of looking to instrument changes in population / farming density)
foreach share in im_od m_od  idXp DXp DurXp /*id_C_Xp  id_S_Xp  D_C_Xp D_S_Xp  Dur_C_Xp Dur_S_Xp */ {
	local lbl: variable label `share'_shares
	gen `share'_L_shares=`share'_shares/total_area_o
	gen `share'_p_shares=`share'_shares/bl_pop_o
	
	label var `share'_L_shares "`lbl' over origin land area"
	*winsor2 `share'_L_shares, replace
	
	* same, but only for units with mines
	gen `share'_L_shares_b_mines = `share'_L_shares * has_mine_asm_d
    gen `share'_p_shares_b_mines = `share'_p_shares * has_mine_asm_d
	
	* and multiplying by the log of mines
	gen `share'_L_shares_log_mines = `share'_L_shares * log_mines_asm
    gen `share'_p_shares_log_mines = `share'_p_shares * log_mines_asm
	
	label var `share'_L_shares_b_mines "`lbl' over origin land area, for units with SNL or ASM"
	label var `share'_L_shares_log_mines "`lbl' over origin land area, multiplied by log of mines"
}

*** Saves the share variables for later since we need them when doing the inversion
savesome  ipums_id_o ipums_id_d *shares *mines using "${final}/shares_`w'.dta", replace

*** Combine the shifts and shares togetehr to make the component parts of the shift shares
*** both for the internal case and the one including international pairs

foreach shift in $shifts {
	disp "shift = `shift'"
	
	cap gen `shift'_d=.
	foreach share in $shares {
		disp "share = `share'"
		
		if "`shift'" == "gr_p_mine_asm" {
            gen B`share'_b_`shift' = `share'_shares_b_mines * (`shift'_d)
            gen B`share'_log_`shift' = `share'_shares_log_mines * (`shift'_d)
        }
        else {
            gen B`share'_`shift' = `share'_shares * (`shift'_d)
        }
	}
}

foreach shift in $shifts {
	disp "shift = `shift'"
	local is_mine : list shift in mine_shifts
	
	cap gen `shift'_d=.
	foreach share in $ishares {
		disp "share = `share'"
		
// 		if "`is_mine'" != "" {
//             gen iB`share'_b_`shift' = i`share'_shares_b_mines * (`shift'_d)
// 			gen iB`share'_log_`shift' = i`share'_shares_log_mines * (`shift'_d)
//         }
        gen iB`share'_`shift' = i`share'_shares * (`shift'_d)
		}
}

*** Collapse down the matrix at the origin level to get the shift share instruments (the B variables) and the sum of the shares (the *shares varaiables) which we need to control for

gcollapse (sum) B* iB* *shares *mines, by(ipums_id_o country_name) missing // the "missing" prevents turning NAs into zeros

rename ipums_id_o ipums_id 

foreach share in $shares {
		capture ren `share'_shares `share'_shares_sum
		capture ren `share'_shares_b_mines `share'_shares_b_mines_sum
		capture ren `share'_shares_log_mines `share'_shares_log_mines_sum
}
foreach share in $ishares {
		capture ren i`share'_shares i`share'_shares_sum
		capture ren i`share'_shares_b_mines i`share'_shares_b_mines_sum
		capture ren i`share'_shares_log_mines i`share'_shares_log_mines_sum
} 

compress
save "${final}/new_bartiks_`w'" , replace
}
clear
use "${final}/new_bartiks_1", clear
gen wave=1
append using  "${final}/new_bartiks_2"
recode wave .=2
save "${final}/new_bartiks" , replace


clear 
use "${final}/shifts_1" 

append using "${final}/shifts_2"
recode wave .=2
compress
save "${final}/shifts.dta", replace