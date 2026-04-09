





***** First, just fiddle a bit with the mine instruments 
use "${final}/mine_instrument.dta", clear

merge 1:1 ipums_id using "${build}/Africa/Artisinal/Artisinal.dta", nogen keep(master match)


foreach t in snl asm {
	gen `t'_mine=(d_log_`t'_price!=.)
	replace d_log_`t'_price=0 if d_log_`t'_price==.
	replace gr_`t'_price=0 if gr_`t'_price==.
	
}
replace asm_prob=0 if asm_prob==.

replace asm_prob=0 if asm_mine==0 | asm_prob==.
replace snl_mines=0 if snl_mines==.
gen ln_snl_mines1=ln(snl_mines+1)

replace p_gr_real=0 if p_gr_real==.
replace p_gr_nom=0 if p_gr_nom==.

gen p_gr_real_pos=(p_gr_real>0)

replace AR1_resi=0 if AR1_resi==.

gen are_mines=(N_mines>0)
gen N_mines_c=N_mines
replace N_mines_c=5 if N_mines_c>5

save "${final}/mine_instrument_adj.dta", replace




















use "${final}/data_merged.dta", clear

*** This to get start and end years 
*merge 1:1 ipums_id using "${final}/MODIS_VCF_wide", nogen keep(master match)
merge 1:1 ipums_id using "${final}/forestloss.dta", nogen keep(master match) keepusing(fl_*_yr)


*********************************** Merging in potential instruments ************************

**** mobile phone coverage
merge 1:1 ipums_id using "${projdir}/data/Build/Africa/GSM/ipums_share_coverage", nogen keep(master match)
foreach var of varlist int_area* {
	replace `var'=0 if `var'==.
}
gen gsm_coverage_end=int_area_2007 if fl_end_yr<2008
replace gsm_coverage_end=int_area_2008 if fl_end_yr==2008
replace gsm_coverage_end=int_area_2009 if fl_end_yr==2009
replace gsm_coverage_end=int_area_2011 if fl_end_yr==2010 | fl_end_yr==2011
replace gsm_coverage_end=int_area_2012 if  fl_end_yr>=2012
gen gsm_coverage_diff=gsm_coverage_end
replace gsm_coverage_diff=gsm_coverage_end-int_area_2009 if fl_start_yr==2009
replace gsm_coverage_diff=gsm_coverage_end-int_area_2011 if fl_start_yr==2011

***** electricity infrastructure
merge 1:1 ipums_id using "${projdir}/data/Build/Africa/electricity/ipums_share_elec", nogen keep(master match)
foreach year in 2007 2017 {
	replace has_elec_`year'=0 if has_elec_`year'==.
}
gen diff_elec=has_elec_2017-has_elec_2007
replace diff_elec=diff_elec/10*max(0,fl_end_yr-2007)

****** broadband network
merge 1:1 ipums_id using "${projdir}/data/Build/Africa/broadband/ipums_share_bb", nogen keep(master match)
replace has_bb=0 if has_bb==.


*** Roads from Jedwab and Storeygard
merge 1:1 ipums_id using "${final}/js_change_MA.dta", nogen keep(master match)
foreach var of varlist *change_MA* {
	replace  `var'=0 if `var'==.
}

*** Mines from mrds 
merge 1:1 ipums_id using "${final}/minerals.dta", nogen keep(master match) 
gen has_mine=(mines!=.)
replace mines=0 if mines==.


*** Roads (and some other things) from Jedwab and Storeygard
merge 1:1 ipums_id using "${final}/js_change_MA.dta", nogen keep(master match)
foreach var of varlist *change_MA* {
	gen missing_`var'=(`var'==.)
	replace  `var'=0 if `var'==.
}
foreach var of varlist dist2poo dist2anymine murd_sh {
	gen ln_`var'=ln(`var')
	gen missing_`var'=(`var'==.)
	replace ln_`var'=0 if `var'==.
	replace `var'=0 if `var'==.
}



****** WB and Chinese projects
drop year
foreach t in start end {
	gen year=fl_`t'_yr
	merge 1:1 ipums_id year using "${projdir}/data/Build/Africa/Chinese/ipums_chinese.dta", nogen keep(master match) keepusing(cum*)
	merge 1:1 ipums_id year using "${projdir}/data/Build/Africa/WB projects/ipums_wb.dta", nogen keep(master match) keepusing(cum*)
	foreach var of varlist cum_*_projects cum_*_spending {
		ren `var' `var'_`t'
		replace `var'_`t'=ln(1+`var'_`t')
		replace `var'_`t'=0 if `var'_`t'==.
	}
	drop year
}
foreach var in cum_chinese_projects cum_chinese_spending cum_wb_projects cum_wb_spending {
		gen D_`var'=`var'_end-`var'_start
}
gen D_cum_projects=D_cum_wb_projects+D_cum_chinese_projects



compress
gen wave=1
save "${final}/instruments", replace
