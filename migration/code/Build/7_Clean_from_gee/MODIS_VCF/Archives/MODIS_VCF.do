foreach w in 1 2 {
import delimited "$dropbox\Migration Africa\data\raw\Africa\MODIS_VCF\MODIS_vcf.csv", clear varnames(1)
drop if ipums_id==. | ipums_id==888888 | ipums_id==888888888
gen year=real(substr(systemindex,3,4))
gcollapse (sum) count sum, by(ipums_id year)

ren sum modis_vcf
replace modis_vcf=modis_vcf/100
gen modis_vcf_avg=modis_vcf/count

drop count
gen bot=(ipums_id>72000 & ipums_id<72100)
tostring ipums_id, replace
replace ipums_id="0" + ipums_id if bot==1

if `w'==1 {
	merge m:1 ipums_id using "${final}/forestloss.dta", nogen keep(match) keepusing(fl_*_yr Country)
}
else {
	merge m:1 ipums_id using "${final}/forestloss_period2.dta", nogen keep(match) keepusing(fl_*_yr Country)
}

tab Country

keep if year==fl_start_yr | year==fl_end_yr
sort ipums_id year

foreach var of varlist modis_vcf modis_vcf_avg {
	gen delta_`var'=(`var'-`var'[_n-1])/(fl_end_yr-fl_start_yr) if year==fl_end_yr
	gen delta_log_`var'=log((`var'/`var'[_n-1])^(1/(fl_end_yr-fl_start_yr))) if year==fl_end_yr
}

drop delta_log_*avg
compress
keep if year==fl_end_yr
 
drop *year 

save "${dropbox}\Migration Africa\data\final\MODIS_VCF_wide_`w'.dta", replace
}

clear

append using "${final}\MODIS_VCF_wide_1" "${final}\MODIS_VCF_wide_2", gen(wave)

save "${final}\MODIS_VCF_wide", replace