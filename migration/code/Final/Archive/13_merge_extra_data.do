use "${final}/data_merged.dta", clear

capture drop *shares* 


*********************************** Merging in new Bartiks (created by 0_make_new_Bartiks) ********************
merge 1:1 ipums_id using "${final}/new_bartiks.dta", nogen keep(master match)

*** Merging in newly created shifts (so that we can control for local shifts)
ren ipums_id ipums_id_d
merge 1:1 ipums_id_d using "${final}/shifts.dta", nogen keep(1 3 4 5) update replace

ren ipums_id_d ipums_id

foreach var of varlist delta_sharecrop* {
	ren `var' `var'_old
}
merge 1:1 ipums_id using  "${final}/sharecrop_new.dta", nogen keep(master match) 
ren delta_sharecrop delta_sharecrop_ann


replace ipums_id=trim(ipums_id)
*********************************  Merging in alternative outcomes *****************************
merge 1:1 ipums_id using "${final}/GAEZ.dta",  keep(master match) nogen
merge 1:1 ipums_id using "${final}/TMF_wide.dta", keep(master match) nogen
merge 1:1 ipums_id using "${final}/MODIS_VCF_wide", nogen keep(master match)
merge 1:1 ipums_id using "${final}/MODIS_LC_wide", nogen keep(master match)
merge 1:1 ipums_id using "${final}/ESA_wide", nogen keep(master match)

merge m:1 country_name using "${final}/trade", nogen keep(master match)

foreach var of varlist defor degra regro {
	replace `var'=`var'/total_area
}


******************************* Merging in potential controls 
merge 1:1 ipums_id using "${build}/Africa/WDPA/ipums_share_pa", nogen keep(master match)
*merge 1:1 ipums_id using "${projdir}\data\Build\Africa\WDPA\ipums_share_pa", nogen keep(master match)
replace share_pa=0 if share_pa==.




*********************************** Merging in potential instruments ************************
merge 1:1 ipums_id using "${final}/instruments.dta", nogen keep(master match)
compress

save "${final}/data_merged_twice.dta", replace
