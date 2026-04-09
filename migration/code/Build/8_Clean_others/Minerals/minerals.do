
cap shp2dta using "${dropbox}\Migration Africa\data\Raw\Africa\Minerals\mrds-2023-05-23-09-51-26\mrds-2023-05-23-09-51-26.shp", data("${dropbox}\Migration Africa\data\Build\Africa\Minerals\map_data") coord("${dropbox}\Migration Africa\data\Build\Africa\Minerals\map_coord") replace

import excel "${dropbox}\Migration Africa\data\Raw\Africa\Minerals\mrds-2023-05-23-09-51-26\dbfinexcel.xlsx", clear first

gen mine_ID=_n

save "${dropbox}\Migration Africa\data\Build\Africa\Minerals\map_data", replace


foreach l in 1  2 {
	shp2dta using "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `l'\AFRADM`l'_renamed.shp", data("${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `l'\AFRADM`l'_renamed.dta") coord("${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `l'\AFRADM`l'_renamed_shp.dta")  replace
	use "${dropbox}\Migration Africa\data\Build\Africa\Minerals\map_coord", clear
	ren _ID mine_ID
	geoinpoly _Y _X using "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `l'\AFRADM`l'_renamed_shp.dta"
	
	merge m:1 _ID using "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `l'\AFRADM`l'_renamed.dta", nogen keepusing(ipums_id) keep(match)
	
	merge 1:1 mine_ID using "${dropbox}\Migration Africa\data\Build\Africa\Minerals\map_data", keep(match) nogen
	
	compress
	save "${dropbox}\Migration Africa\data\Build\Africa\Minerals\map_data_in_ipums`l'", replace
	
}
clear 
append using  "${dropbox}\Migration Africa\data\Build\Africa\Minerals\map_data_in_ipums1" "${dropbox}\Migration Africa\data\Build\Africa\Minerals\map_data_in_ipums2"

keep if ipums_id!=""

keep mine_ID ipums_id commod1 dev_stat yr_lst_prd
save "${dropbox}\Migration Africa\data\Build\Africa\Minerals\map_data_in_ipums", replace

use "${dropbox}\Migration Africa\data\Build\Africa\Minerals\map_data_in_ipums", clear 

gen mines=1

drop if dev_stat=="Prospect" | (yr_lst_prd<2000 & yr_lst_prd!=0)

collapse (sum) mines, by(ipums_id)

compress
save "${dropbox}\Migration Africa\data\final\minerals", replace

