****** This file (written by Sam initially, updated by Liam) generates various Bartik / Shift-share instruments
****** Since we have worked with a lot of different shifts and shares, it only generates a subset of possible ones 
****** I.e. it generates all combinations of the following shifts with the following shares:
global shares "m_od_L  DXp_L DurXp_L m_odXlm_L m_odXdm_L DXpXlm_L DXpXdm_L m_oXlm_L DXlm_L m_oXdm_L DXdm_L" 
global ishares "m_od_L dXp_L"

global shifts " gr_nonag_ann gr_urban_pop_ann gr_p_mine_asm" 
foreach l in tlv tlla tlv_ra tlla_ra  {
	global shifts "$shifts gr_n`l' "
}
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
forval wave=1/2 {
	if `wave'==1 {
		use "${final}/employment.dta", clear
		merge 1:1 ipums_id using "${final}/population.dta", nogen keep( match) 
		merge 1:1 ipums_id using "${final}/nightlight.dta", nogen 
		
		gen gr_nonag_ann=delta_nonag_ann/bl_nonag
		label var gr_nonag_ann "non-ag population growth rate"
		*winsor2 gr_nonag_ann, replace
		
		gen gr_urban_pop_ann=delta_urban_pop_ann/bl_urban_pop
		
		**** Since some countries don't have urban or non-ag population, we use the relevant one insted for these countries
		replace bl_nonag=bl_urban_pop if country=="CMR" | country=="ZAF"
		replace bl_urban_pop=bl_nonag if inlist(country,"BWA","BFA","ZMB") 
		foreach var in gr_ {
			replace `var'nonag_ann=`var'urban_pop_ann if country=="CMR" | country=="ZAF"
			replace `var'urban_pop_ann=`var'nonag_ann if inlist(country,"BWA","BFA","ZMB") 
		}
	}
	else {
		use  "${final}/nightlight_period2.dta", clear
		gduplicates tag ipums_id, gen(t)
		drop if t>0 & delta_log_ntlv==.
		drop t
	}
	gen wave=`wave'
	
	merge 1:1 ipums_id wave using "${final}/mine_instrument.dta",  nogen keep(master match) 
	//merge 1:1 ipums_id wave using "${final}/builtup_rf.dta", nogen keep(master match)
	//merge 1:1 ipums_id wave using "${final}/MODIS_LC_wide.dta",  nogen  keep(master match) keepusing(*barea*)
	
	label var bl_ntlla "lit area"
	label var bl_ntlv_ra "night lights"
	foreach l in tlv tlla tlv_ra tlla_ra {
		gen gr_n`l'=delta_n`l'/bl_n`l'
		local temp: variable label bl_n`l'
		label var gr_n`l' "`temp' growth rate"
		*winsor2  gr_n`l', replace
		replace gr_n`l'=0 if bl_n`l'<=${nl_cutoff}
	}
		
	foreach l in nag_cd_hata nag_cd_hatb serv_cd_hata serv_cd_hatb ind_cd_hata ind_cd_hatb {
		gen gr_gdp_`l'=delta_gdp_`l'/bl_gdp_`l'
		local temp: variable label bl_gdp_`l'
		label var gr_gdp_`l' "`temp' growth rate"
		sum gr_gdp_`l'
	}

	label var bl_ntlla "lit area"
	label var bl_ntlv_ra "night lights"
	label var bl_gdp_nag_cd_hata "gdp non ag short model"
	label var bl_gdp_nag_cd_hatb "gdp non ag long model"
	label var bl_gdp_ind_cd_hata "gdp industry short model"
	label var bl_gdp_ind_cd_hatb "gdp industry long model"
	label var bl_gdp_serv_cd_hata "gdp service short model"
	label var bl_gdp_serv_cd_hatb "gdp service short model"

	keep ipums_id gr* 

	*** Rename everythign with a _d on end just so it doesn't get confusing if we merge this with the origin file  (not sure if we currently do this)
	rename * *_d
	gen wave=`wave'
	save "${final}/shifts_`wave'.dta", replace
}

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

	gen intm_od = m_od
	replace m_od = . if country_name != prev_country_name

	merge 1:1 ipums_id_d ipums_id_o using "${final}/full_distance_census`w'.dta", nogen keep(master match) 

	merge 1:1 ipums_id_d ipums_id_o using "${final}/dist_mat.dta", nogen keep(master match) keepusing(*osm*)
	
	gen intinv_dist_shares = inv_dist_shares
	replace inv_dist_shares = . if country_name != prev_country_name

	merge m:1 ipums_id_o using `origin', nogen keep(3)
	merge m:1 ipums_id_d using "${final}/shifts_`w'.dta", nogen keep(3)
	ren ipums_id_d ipums_id
	*** Note that should technically use endline pop for some wave 2 places
	merge m:1 ipums_id using "${final}/population.dta", nogen keep(master match) keepusing(*l_pop *l_urban_pop)
	merge m:1 ipums_id using "${final}/employment.dta", nogen keep(master match) keepusing(*l_nonag)
	merge m:1 ipums_id using "${final}/mines_loc.dta",  nogen keep(master match)

 	if `w'==2 {
		foreach p in pop urban_pop nonag {
	
 			replace bl_`p'=el_`p' if bl_`p'==.
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
	
	foreach share in m_od m_o {
		gen `share'Xdm_shares=`share'_shares*has_mine_asm_d
		gen `share'Xlm_shares=`share'_shares*log_mines_asm
	}
	
	
	quietly{
		foreach share in d D Dur {
		** First by simply multiplying by destination population
		gen `share'Xp_shares=`share'_shares*bl_pop_d /100
		gen `share'Xdm_shares=`share'_shares * has_mine_asm_d
		gen `share'Xlm_shares=`share'_shares * log_mines_asm
		gen `share'XpXdm_shares=`share'_shares*bl_pop_d /100 * has_mine_asm_d
		gen `share'XpXlm_shares=`share'_shares*bl_pop_d /100 * log_mines_asm
		cap gen i`share'Xp_shares= i`share'_shares*bl_pop_d /100
		}
	}
	
	***** Generate various shares based on old ones that are divided through by land area (since we are sort of looking to instrument changes in population / farming density)
	foreach share in im_od m_od m_odXdm m_odXlm  idXp DXp DurXp  DXpXdm DXpXlm  DXdm DXlm m_oXlm m_oXdm /*id_C_Xp  id_S_Xp  D_C_Xp D_S_Xp  Dur_C_Xp Dur_S_Xp */ {
		local lbl: variable label `share'_shares
		gen `share'_L_shares=`share'_shares/total_area_o
		gen `share'_p_shares=`share'_shares/bl_pop_o
	
		label var `share'_L_shares "`lbl' over origin land area"
		*winsor2 `share'_L_shares, replace
	}

	*** Saves the share variables for later since we need them when doing the inversion
	savesome  ipums_id_o ipums_id_d *shares  using "${final}/shares_`w'.dta", replace

	*** Combine the shifts and shares togetehr to make the component parts of the shift shares
	*** both for the internal case and the one including international pairs

	foreach shift in $shifts {
		disp "shift = `shift'"
	
		cap gen `shift'_d=.
		foreach share in $shares {
			disp "share = `share'"
			gen B`share'_`shift' = `share'_shares * (`shift'_d)
		}
	}

	foreach shift in $shifts {
		disp "shift = `shift'"
	
		cap gen `shift'_d=.
		foreach share in $ishares {
			disp "share = `share'"
		
			gen iB`share'_`shift' = i`share'_shares * (`shift'_d)
		}
	}

	*** Collapse down the matrix at the origin level to get the shift share instruments (the B variables) and the sum of the shares (the *shares varaiables) which we need to control for
	gcollapse (sum) B* iB* *shares, by(ipums_id_o country_name) // the "missing" prevents turning NAs into zeros

	rename ipums_id_o ipums_id 

	foreach share in $shares {
		capture ren `share'_shares `share'_shares_sum
	}
	foreach share in $ishares {
		capture ren i`share'_shares i`share'_shares_sum
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