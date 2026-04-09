



forval i=1/2 {
	
	cd "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `i'"
	
	*shp2dta using AFRADM`i'_renamed.shp, data(AFRADM`i'_data) coord(AFRADM`i'_coords) replace
	
	/*
	use AFRADM`i'_coords, clear
	fieldarea _X _Y, gen(area) id(_ID) unit(sqkm)
	
	compress
	
	save ipums`i'_areas_temp.dta, replace
	
	
	
	use ipums_id _ID using AFRADM`i'_data, clear
	
	merge 1:1 _ID using ipums`i'_areas_temp.dta, keep(match) nogen
	drop _ID
	compress
	drop if ipums_id==""
	gcollapse (sum) area, by(ipums_id)
	save ipums`i'_areas.dta, replace
	
	
	erase AFRADM`i'_coords.dta
	erase AFRADM`i'_data.dta
	erase ipums`i'_areas_temp.dta
	*/
	
	cd "${dropbox}\Migration Africa\data\Build\Africa\Chinese\"
	
	
	
		
		
		
		
		*shp2dta using electricity_`y'_ipums`i'.shp, data(electricity_`y'_ipums`i'_data) coord(electricity_`y'_ipums`i'_coords) replace
		spshape2dta chinese_ipums`i'.shp,  replace
		
		
		use ipums_id Implementa Amount__Co using chinese_ipums`i'.dta, clear
		
		drop if Implementa==""
		destring Implementa, gen(year)
		destring Amount, gen(chinese_spending)
		drop Implementa Amount
		
		gcollapse (sum) chinese_spending, by(ipums_id year)
		
		
		
		
		compress
	
		
	
	*merge 1:1 ipums_id using "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `i'\ipums`i'_areas.dta", keep(match) nogen
	
	
	compress
	save ipums_`i'_share_coverage, replace
}


clear 

append using ipums_1_share_coverage ipums_2_share_coverage
drop if ipums_id==""
compress 

gen chinese_projects=1

encode ipums_id, gen(ipums_code)
tsset ipums_code year
tsfill, full
foreach var in spending projects {
	gen cum_chinese_`var'=max(0,chinese_`var') if year==1998
	replace cum_chinese_`var'=L.cum_chinese_`var'+max(0,chinese_`var') if year>1998
}
compress
drop ipums_id
decode ipums_code, gen(ipums_id) 
isid ipums_id year 
save ipums_chinese, replace