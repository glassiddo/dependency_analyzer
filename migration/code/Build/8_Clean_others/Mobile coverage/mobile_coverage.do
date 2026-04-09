



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
	
	cd "${dropbox}\Migration Africa\data\Build\Africa\GSM\"
	
	
	foreach y in 2007 2008 2009 2011 2012 {
		
		
		
		
		*shp2dta using coverage_`y'_ipums`i'.shp, data(coverage_`y'_ipums`i'_data) coord(coverage_`y'_ipums`i'_coords) replace
		spshape2dta coverage_`y'_ipums`i'.shp,  replace
		
		use coverage_`y'_ipums`i'_shp.dta, clear
		
		fieldarea _X _Y, gen(int_area_`y') id(_ID) unit(sqkm)
		drop if _ID==.
		save ipums`i'_areas_temp.dta, replace
		
		use ipums_id _ID using coverage_`y'_ipums`i'.dta, clear
		
		merge 1:1 _ID using ipums`i'_areas_temp.dta, keep(match) nogen
		gcollapse (sum) int_area, by(ipums_id)
		
		compress
	
		save int`i'_`y'_areas.dta, replace
	
	
		
		*erase ipums`i'_areas_temp.dta
		*erase map`m'_ipums`i'_data.dta
		*erase map`m'_ipums`i'_coords.dta
		
	}
	
	clear 
	
	use int`i'_2007_areas.dta, clear
	foreach y in 2008 2009 2011 2012 {
		merge 1:1 ipums_id using int`i'_`y'_areas.dta, nogen 
	}
	
	
	
	merge 1:1 ipums_id using "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `i'\ipums`i'_areas.dta", keep(match) nogen
	
	foreach var of varlist int_area_* {
		replace `var'=`var'/area
		replace `var'=0 if `var'==.
	}
	drop area
	compress
	save ipums_`i'_share_coverage, replace
}


clear 

append using ipums_1_share_coverage ipums_2_share_coverage

compress 

save ipums_share_coverage, replace