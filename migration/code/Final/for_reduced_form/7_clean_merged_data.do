use "${final}/data_merged.dta", clear


************************** Choose sample *********************************
*drop if bl_treecover == 0 | bl_treecover == .

gen bl_croppedarea=bl_sharecrop*total_area


foreach var of varlist defor degra regro {
	replace `var'=`var'/total_area
}

gen gr_nonforest=(total_loss_t30)/(total_area-bl_treecover_t30)



*************************** Alter variables
replace log_emig = . if log_emig == 0
replace ilog_emig = . if ilog_emig == 0

gen net_emig_ann=emig_ann-immig_ann



*replace delta_log_ntla=0 if el_ntla==0 | bl_ntla==0
*replace delta_log_ntlv=0 if el_ntlv==0 | bl_ntlv==0
*replace delta_ntla=0 if el_ntla==0 | bl_ntla==0

* Winsorize
*winsor2 delta_log_share,cuts(5 95)
*winsor2 delta_log_loss,cuts(5 90)


* No info on sector of employment in Cameroon
foreach var of varlist *ag*{
	*replace `var'=. if country=="Cameroon"
}

foreach var of varlist *delta*urban* *delta*pop* *nonag* *emig* {
	disp "`var'"
	*replace `var'=. if country=="Burkina Faso" || country=="Zambia" || country=="Malawi" || country=="Sudan" || country=="Togo" || country=="Zimbabwe"
	cap replace `var'=. if  wave==2
*	replace `var'=. if el_pop==.
}

*replace bl_ag=bl_pop if Country=="CMR"
replace bl_ag=bl_rural_pop if country=="CMR"





* No info on emigration in Ghana or Cameroon
foreach var of varlist *emig*{
    cap replace `var'=. if country=="GHA" 
}

*gegen sum_bl_pa=sum(bl_pa), by(country_name)
*gen emig_ann_sum_pa= emig_ann /sum_bl_pa 
*gen iemig_ann_sum_pa= iemig_ann /sum_bl_pa
 
gen el_iemig_rate = iemig_ann / bl_pop
gen emig_ann_pop=emig_ann/bl_pop
gen emig_ann_L=emig_ann/total_area
replace emig_ann_L=. if wave=="2" 
cap label var emig_ann_L "Annual emigration rate over origin area"
gen net_emig_ann_L=net_emig_ann/total_area
cap label var emig_ann_L "Annual net emigration rate over origin area"
gen iemig_ann_L=iemig_ann/total_area
cap label var iemig_ann_L "Annual emigration rate over origin area, including international pairs"

*************************** Generate extra variables **********************************
// gen region_code=substr(ipums_id,1,length(ipums_id)-3)
// encode country_name, gen(ctry3)

*gen bl_log_treecover=log(bl_treecover+1)
destring wave, replace
egen deforestation_t30_w1=max(deforestation_t30*(wave==1)), by(ipums_id)
replace deforestation_t30_w1=0 if deforestation_t30_w1==. | wave==1




/*
gen start=real(substr(range,1,4))
gen end=real(substr(range,6,9))
gen length_period=end-start
gen delta_log_share_forest=log(((bl_treecover-total_loss)/bl_treecover)^(1/length_period))
*/


* Additional cleaning of GAEZ variables
*gen GAEZ_cash_crop_share_area=GAEZ_cropped_area_cash_2000/GAEZ_cropped_area_2000
gen GAEZ_cash_crop_share_prod= GAEZ_production_cash_2000/GAEZ_production_2000
*su GAEZ_cash_crop_share_area,d
*gen GAEZ_cash_crop_high_share=GAEZ_cash_crop_share_area>`r(p50)' if !missing(GAEZ_cash_crop_share_area)

foreach y in 2000 2010 {
	gen GAEZ_cropped_cash_share_`y'=GAEZ_cropped_area_cash_`y'/total_area
	gen GAEZ_cropped_share_`y'=GAEZ_cropped_area_`y'/total_area
}
gen GAEZ_delta_cropped_share=GAEZ_cropped_share_2010 -GAEZ_cropped_share_2000 
gen GAEZ_delta_cropped_cash_share=GAEZ_cropped_cash_share_2010 -GAEZ_cropped_cash_share_2000 

foreach var of varlist GAEZ_production_???? GAEZ_cropped_area_???? GAEZ_avg_yield_???? {
	gen ln_`var'=ln(`var')
}
rename ln_GAEZ* GAEZ_ln*
foreach var of varlist GAEZ_delta*{
*	winsor2 `var', cuts(5 95) replace
}

*gen delta_cropshrubgrass_LC_Type5=delta_crop_LC_Type5+delta_shrub_LC_Type5+delta_grass_LC_Type5


************************** cap label variables **************************************


foreach e in sum  {
	*cap label var delta_nonag_ann_`e'_pa "$\Delta$ non-ag pop / `e' pop"
}
/*	
foreach var of varlist Bm_d_* {
	local shift=substr("`var'",6,.)
	cap local lab: variable label `shift'
	cap label var `var' "$\text{B}_{m}$ (`lab')"
}

foreach var of varlist iBm_d_* {
	local shift=substr("`var'",6,.)
	cap local lab: variable label `shift'
	cap label var `var' "$\text{iB}_{m}$ (`lab')"
}


foreach var of varlist Bm_o_* {
	local shift=substr("`var'",6,.)
	cap local lab: variable label `shift'
	cap label var `var' "$\text{B}_{mo}$ (`lab')"
}


foreach var of varlist Bd_* {
	local shift=substr("`var'",4,.)
	cap local lab: variable label `shift'
	cap label var `var' "$\text{B}_{t}$ (`lab')"
}

foreach var of varlist iBd_* {
	local shift=substr("`var'",4,.)
	cap local lab: variable label `shift'
	cap label var `var' "$\text{iB}_{t}$ (`lab')"
}
*/


cap label var Bm_d_gr_nonag "$\text{B}_{m}$ ($\Delta$ log non-ag pop)"
cap label var Bd_gr_nonag "$\text{B}_{t}$ ($\Delta$ log non-ag pop)"
cap label var iBm_d_gr_nonag "$\text{iB}_{m}$ ($\Delta$ log non-ag pop)"
cap label var iBd_gr_nonag "$\text{iB}_{t}$ ($\Delta$ log non-ag pop)"

cap label var Bd_nlv "$\text{B}_{t}$ (Night-light Intensity, new)"
cap label var Bd_nla "$\text{B}_{t}$ (Night-light Area, new)"
cap label var Bd_nlv_ra "$\text{B}_{t}$ (Night-light Intensity, new, ra)"
cap label var Bd_nla_ra "$\text{B}_{t}$ (Night-light Area, new, ra)"
cap label var iBd_nlv "$\text{iB}_{t}$ (Night-light Intensity, new)"
cap label var iBd_nla "$\text{iB}_{t}$ (Night-light Area, new)"
cap label var iBd_nlv_ra "$\text{iB}_{t}$ (Night-light Intensity, new, ra)"
cap label var iBd_nla_ra "$\text{iB}_{t}$ (Night-light Area, new, ra)"

cap label var Bm_nlvo "$\text{B}_{m}$ (Night-light Intensity, new)"
cap label var Bm_nlao "$\text{B}_{m}$ (Night-light Area, new)"
cap label var Bm_nlvo_ra "$\text{B}_{m}$ (Night-light Intensity, new, ra)"
cap label var Bm_nlao_ra "$\text{B}_{m}$ (Night-light Area, new, ra)"
cap label var iBm_nlvo "$\text{iB}_{m}$ (Night-light Intensity, new)"
cap label var iBm_nlao "$\text{iB}_{m}$ (Night-light Area, new)"
cap label var iBm_nlvo_ra "$\text{iB}_{m}$ (Night-light Intensity, new, ra)"
cap label var iBm_nlao_ra "$\text{iB}_{m}$ (Night-light Area, new, ra)"

/*
cap label var Bm_o_delta_log_nlarea_ra "$\text{B}_{m}$ (Night-light Area)"
cap label var Bm_o_delta_log_nvarea_ra "$\text{B}_{m}$ (Night-light Intensity)"
cap label var Bd_delta_log_nlarea_ra "$\text{B}_{d}$ (Night-light Area)"
cap label var Bd_delta_log_nvarea_ra "$\text{B}_{d}$ (Night-light Intensity)"
cap label var delta_log_nvarea_ra "Local shift (Night-light Intensity)"
cap label var delta_log_nlarea_ra "Local shift (Night-light Area)"
*/

* recap label vars for the tables with shorter names
*cap label var O_log_gr_ag "$\Delta$ log ag empl"
*cap label var O_log_age_ypl "log youth pop"
*cap label var O_log_age_opl "log aging pop"
cap label var delta_log_sharecrop "$\Delta$ log share cropped"




cap label var delta_sharecrop_ann "$\Delta$ share cropped"
cap label var delta_forest_LC_Type5 "$\Delta$ share forested"
cap label var delta_cropshrubgrass_LC_Type5 "$\Delta$ share agriculture"
cap label var delta_built_LC_Type5 "$\Delta$ share built"


cap label var deforestation "Share deforested"
cap label var d_shares_sum "Sum Inv. Distance"
cap label var m_d_shares_sum "Sum Mig. Shares"
cap label var el_emig_rate "Emig. Rate"
cap label var id_shares_sum "Sum Inv. Distance, international"
cap label var im_d_shares_sum "Sum Mig. Shares, international"
cap label var iel_emig_rate "Emig. Rate, including international pairs"


* relabel bartiks for tables so that they appear in the same row
cap label var O_delta_nonag_ann "Local shift (Non-Ag Employment)"
cap label var O_delta_log_nonag_ann "Local shift (Delta log Non-Ag Empl)"
cap label var O_delta_pop_ann "Local shift (Population)"
cap label var O_delta_log_ntlv "Local shift (Night-Light Intensity)"
cap label var O_delta_ntlla "Local shift (Night-Light Area)"

cap label var Bm_d_nonag "$\text{B}_{m}$ (Non-Ag Employment)"
cap label var Bd_nonag "$\text{B}_{t}$ (Non-Ag Employment)"
cap label var iBm_d_nonag "$\text{iB}_{m}$ (Non-Ag Employment)"
cap label var iBd_nonag "$\text{iB}_{t}$ (Non-Ag Employment)"

*cap label var Bm_o_gr_nonag "$\text{B}_{m}$ (Delta log Non-Ag Empl)"
cap label var Bd_gr_nonag "$\text{B}_{t}$ (Delta log Non-Ag Empl)"
cap label var iBd_gr_nonag "$\text{iB}_{t}$ (Delta log Non-Ag Empl)"

cap label var Bm_ntllao "$\text{B}_{m}$ (Night-Light Area)"
cap label var Bd_ntlla "$\text{B}_{t}$ (Night-Light Area)"
cap label var Bm_d "$\text{B}_{m}$ (Population)"
cap label var Bd "$\text{B}_{t}$ (Population)"
cap label var Bm_ntlvo "$\text{B}_{m}$ (Night-Light Intensity)"
cap label var Bd_ntlv "$\text{B}_{t}$ (Night-Light Intensity)"
cap label var Bm_ntlv "$\text{B}_{m}$ (Night-Light Intensity Dest. Shares)"
cap label var Bm_d_mining "$\text{B}_{m}$ (Mining Employment)"
cap label var Bd_mining "$\text{B}_{t}$ (Mining Employment)"

cap label var iBm_ntllao "$\text{iB}_{m}$ (Night-Light Area)"
cap label var iBd_ntlla "$\text{iB}_{t}$ (Night-Light Area)"
cap label var iBm_d "$\text{iB}_{m}$ (Population)"
cap label var iBd "$\text{iB}_{t}$ (Population)"
cap label var iBm_ntlvo "$\text{iB}_{m}$ (Night-Light Intensity)"
cap label var iBd_ntlv "$\text{iB}_{t}$ (Night-Light Intensity)"
cap label var iBm_ntlv "$\text{iB}_{m}$ (Night-Light Intensity Dest. Shares)"
cap label var iBm_d_mining "$\text{iB}_{m}$ (Mining Employment)"
cap label var iBd_mining "$\text{iB}_{t}$ (Mining Employment)"


***** Standardizing
gen ${migration_control}_nl=${migration_control}
gen ${trade_control}_nl=${trade_control}

foreach var of varlist  $migration_ss $trade_ss   $imigration_ss $itrade_ss $migration_control  $trade_control {
*	winsor2 `var', replace cuts(1 99)
	sum `var' [aweight = $wgt] if !missing(gr_nonag_ann)
	replace `var'=(`var'-r(mean))/(r(sd)*100) if !missing(gr_nonag_ann)
	replace `var'=. if missing(gr_nonag_ann)
}

foreach var of varlist $migration_nl_ss $trade_nl_ss $migration_alt_nl_ss $trade_alt_nl_ss $migration_control_nl $trade_control_nl  $migration_p_ss $trade_p_ss $trade_p_control $migration_p_control {
*	winsor2 `var', replace cuts(1 99)
	sum `var' [aweight = $wgt] if country!="MDG"
	replace `var'=(`var'-r(mean))/(r(sd)*100) if country!="MDG"

}
 


save "${final}/data_clean.dta", replace





