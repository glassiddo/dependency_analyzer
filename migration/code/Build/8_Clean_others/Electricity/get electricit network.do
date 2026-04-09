



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
	
	cd "${dropbox}\Migration Africa\data\Build\Africa\Electricity\"
	
	
	foreach y in 2007 2017 {
		
		
		
		
		*shp2dta using electricity_`y'_ipums`i'.shp, data(electricity_`y'_ipums`i'_data) coord(electricity_`y'_ipums`i'_coords) replace
		spshape2dta electricity_`y'_int`i'.shp,  replace
		
		
		use ipums_id _ID using electricity_`y'_int`i'.dta, clear
		
		gen has_elec_`y'=1
		
		gcollapse (max) has_elec_`y', by(ipums_id)
		
		
		
		
		compress
	
		save int`i'_`y'_areas.dta, replace
	
	
		
		*erase ipums`i'_areas_temp.dta
		*erase map`m'_ipums`i'_data.dta
		*erase map`m'_ipums`i'_coords.dta
		
	}
	
	clear 
	
	use int`i'_2007_areas.dta, clear
	merge 1:1 ipums_id using int`i'_2017_areas.dta, nogen 
	
	replace has_elec_2007=0 if has_elec_2007==.
	replace has_elec_2017=1 if has_elec_2017==.
	
	
	
	
	merge 1:1 ipums_id using "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `i'\ipums`i'_areas.dta", keep(match) nogen
	
	
	compress
	save ipums_`i'_share_coverage, replace
}


clear 

append using ipums_1_share_coverage ipums_2_share_coverage

compress 

save ipums_share_elec, replace