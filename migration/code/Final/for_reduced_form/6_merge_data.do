**** Merge files

use  "${final}/trade",  clear
keep if !missing(country_name)
keep if year==2000 
keep country_name trade_import_ag_gdp trade_export_ag_gdp trade_import_ind_gdp trade_export_ind_gdp trade_balance_ag_gdp trade_balance_ind_gdp trade_balance_gdp
tempfile trade
save `trade', replace

use "${final}\forestloss_cuts.dta" , clear
gen wave = 1
append using "${final}/forestloss_cuts_period2.dta"
replace wave = 2 if missing(wave)

drop if missing(fl_end_yr)


*********************************** Merging in new Bartiks (created by 0_make_new_Bartiks) ********************

merge 1:1 ipums_id wave using "${final}/new_bartiks.dta", nogen keep(master match)


*** Merging in newly created shifts (so that we can control for local shifts)
ren ipums_id ipums_id_d
merge 1:1 ipums_id_d wave using "${final}/shifts.dta", nogen keep(1 3 ) 
ren ipums_id_d ipums_id


merge 1:1 ipums_id wave using  "${final}/sharecrop_new.dta", nogen keep(master match) 
ren delta_sharecrop delta_sharecrop_ann

replace ipums_id=trim(ipums_id)


*********************************  Merging in alternative outcomes *****************************
merge m:1 ipums_id using "${final}/population.dta",  keep(master match) nogen
merge m:1 ipums_id using "${final}/employment.dta",  keep(master match) nogen
merge m:1 ipums_id using "${final}/GAEZ.dta",  keep(master match) nogen
merge m:1 ipums_id using "${final}/urban.dta", keep(master match) nogen
// merge m:1 ipums_id using "${final}/nightlight.dta",  keep(delta_ntlla, delta_log_ntlv) nogen
merge m:1 ipums_id using "${final}/immigration_census1.dta",  keep(master match) nogen
merge m:1 ipums_id using "${final}/immigration_census2.dta",  keep(master match) nogen
//merge m:1 ipums_id using "${final}/emigration_census1.dta",  keep(master match) nogen
merge m:1 ipums_id using "${final}/emigration_census2.dta",  keep(master match) nogen
merge m:1 ipums_id using "${final}/nightlight.dta", ///
    keep(master match) nogen ///
    keepusing(delta_ntlla delta_log_ntlv)

foreach cut in 10 20 30 40 50 60 70 80 90 {
	gen deforestation_t`cut'=avg_loss_t`cut'/total_area
}

tostring wave, replace
merge 1:1 ipums_id wave using "${final}/TMF_wide.dta", keep(master match) nogen
merge 1:1 ipums_id wave using "${final}/MODIS_VCF_wide.dta", nogen keep(master match)
// merge 1:1 ipums_id wave using "${final}/MODIS_LC_wide.dta", nogen keep(master match)
// merge 1:1 ipums_id wave using "${final}/ESA_wide", nogen keep(master match)

// preserve
//     use "${final}/id.dta", clear
//     keep country_name country
//     duplicates drop country_name, force
//     tempfile id_clean
//     save `id_clean'
// restore
//
// merge m:1 country_name using `id_clean', keep(master match) nogen
merge m:1 country_name using `trade', ///
    keep(master match) nogen


******************************* Merging in potential controls 
//merge m:1 ipums_id using "${build}/Africa/WDPA/ipums_share_pa", nogen keep(master match)
//replace share_pa=0 if share_pa==. / ipums_share_pa isn't updated for nigeria, gabon etc, we wouldn't false zeros 
merge 1:1 ipums_id wave using "${final}/geo_controls.dta", nogen keep(master match)

drop *yr*
drop *_prd_len

compress

save "${final}/data_merged.dta", replace
