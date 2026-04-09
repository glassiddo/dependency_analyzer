


foreach l in 1  2 {
	import delimited "D:\Dropbox\Liam\Migration Africa\data\Raw\Africa\WB Projects\data\locations.csv", clear
	geoinpoly latitude longitude using "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `l'\AFRADM`l'_coords.dta"
	merge m:1 _ID using "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `l'\AFRADM`l'_data.dta", nogen keepusing(ipums_id) keep(match)
	keep project_id ipums_id
	
	compress
	drop if ipums_id==""
	save "D:\Dropbox\Liam\Migration Africa\data\Build\Africa\WB Projects\WB_`l'.dta", replace
}

clear

append using "D:\Dropbox\Liam\Migration Africa\data\Build\Africa\WB Projects\WB_1.dta" "D:\Dropbox\Liam\Migration Africa\data\Build\Africa\WB Projects\WB_2.dta"
drop if ipums_id==""

save "D:\Dropbox\Liam\Migration Africa\data\Build\Africa\WB Projects\WB_project_ipums_mapping.dta", replace

import delimited "D:\Dropbox\Liam\Migration Africa\data\Raw\Africa\WB Projects\data\projects.csv", clear


merge 1:m project_id using "D:\Dropbox\Liam\Migration Africa\data\Build\Africa\WB Projects\WB_project_ipums_mapping.dta", keep(match) nogen

ren transactions_end_year year
ren total_disbursements wb_spending
replace wb_spending=total_commitments if wb_spending==.
gcollapse (sum) wb_spending, by(ipums_id year)

gen wb_projects=1

encode ipums_id, gen(ipums_code)
tsset ipums_code year
tsfill, full
foreach var in spending projects {
	gen cum_wb_`var'=max(0,wb_`var') if year==1998
	replace cum_wb_`var'=L.cum_wb_`var'+max(0,wb_`var') if year>1998
}
compress
drop ipums_id
decode ipums_code, gen(ipums_id) 
isid ipums_id year 
save "D:\Dropbox\Liam\Migration Africa\data\Build\Africa\WB Projects\ipums_wb.dta", replace