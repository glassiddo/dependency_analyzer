

foreach w in 1 2 {
clear 
import delimited "${dropbox}\Migration Africa\data\Raw\Africa\Tropical Moist Forests\MPIOTMF.csv"

reshape long intact defor degra regro, i(ipums_id) j(year)

tostring ipums_id, replace
*** This to get start and end years 
if `w'==1 {
	merge m:1 ipums_id using "${final}/forestloss.dta", nogen keep(match) keepusing(fl_*_yr Country)
}
else {
	merge m:1 ipums_id using "${final}/forestloss_period2.dta", nogen keep(match) keepusing(fl_*_yr Country)
}



gegen intact_start=max(intact*(year==fl_start_yr)), by(ipums_id)

keep if year>fl_start_yr & year<=fl_end_yr


gen years=1
gcollapse (max) intact_start (sum) years defor degra regro, by(ipums_id)

foreach var of varlist defor degra regro {
	replace `var'=. if intact_start==0
	replace `var'=`var'/years
}

compress

save "${final}\TMF_wide_`w'", replace

}

clear

append using "${final}\TMF_wide_1" "${final}\TMF_wide_2", gen(wave)

save "${final}\TMF_wide", replace