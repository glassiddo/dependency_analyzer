



forval i=1/2 {
	
	cd "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `i'"
	
	shp2dta using AFRADM`i'_renamed.shp, data(AFRADM`i'_data) coord(AFRADM`i'_coords) replace
	
	
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
	forval m=0/2 {
		
		
		cd "${dropbox}\Migration Africa\data\Build\Africa\WDPA\"
		
		shp2dta using map`m'_ipums`i'.shp, data(map`m'_ipums`i'_data) coord(map`m'_ipums`i'_coords) replace
		use map`m'_ipums`i'_coords.dta, clear
		
		fieldarea _X _Y, gen(int_area) id(_ID) unit(sqkm)
		drop if _ID==.
		save ipums`i'_areas_temp.dta, replace
		
		use ipums_id _ID using map`m'_ipums`i'_data, clear
		
		merge 1:1 _ID using ipums`i'_areas_temp.dta, keep(match) nogen
		drop _ID
		compress
	
		save int`i'_`m'_areas.dta, replace
	
	
		erase ipums`i'_areas_temp.dta
		erase map`m'_ipums`i'_data.dta
		erase map`m'_ipums`i'_coords.dta
		
	}
	
	
	clear 
	
	append using int`i'_0_areas.dta int`i'_1_areas.dta int`i'_2_areas.dta
	
	gcollapse (sum) int_area, by(ipums_id)
	
	merge 1:1 ipums_id using "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `i'\ipums`i'_areas.dta", keep(match) nogen
	
	
	gen share_pa=int_area/area
	drop int_area area
	compress
	save ipums_`i'_share_pa, replace
}


clear 

append using ipums_1_share_pa ipums_2_share_pa

compress 

save ipums_share_pa, replace