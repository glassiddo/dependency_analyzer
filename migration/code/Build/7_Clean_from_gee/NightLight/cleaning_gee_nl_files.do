**********************************************************************
* THIS PROGRAM CLEANS NIGHTTIME LIGHT AND JOINS WITH LAND COVER AND GDP *
**********************************************************************

********************************************************************
* Part 1: Nighttime light, land cover and gdp at the country level *
********************************************************************

// takes the gee files on world units and cleans the way the data is formatted, generating a long data of country (iso3) x year x satellite and the various measures. if there's more than a single satellite value, we take the average measure and label it as the first satellite, to get to country x year data. then the MODIS LC data and WDI data (both country x year format) are joined into the satellite data. log versions of the variables are created, years are allocated into the relevant decade (e.g., 1994 into 1990). 

// Hence WorldNTLGDP.dta is country x year long df with various nightlight pixels and area values - measured by different satellites - and other measures (gdp by industry, area of grassland/forest etc).

*<<< DMSP NTL
import delimited "${projdir}\DATA\RAW\WORLD\GEE\worldUnitsDMSP.csv", clear 

collapse (first) adm0_name geotype (sum) areaha-ntlvf18y2013, by(iso3)

qui reshape long ntlpf10y ntlpf12y ntlpf14y ntlpf15y ntlpf16y ntlpf18y ntllpf10y ntllpf12y ntllpf14y ntllpf15y ntllpf16y ntllpf18y ntlmaxf10y ntlmaxf12y ntlmaxf14y ntlmaxf15y ntlmaxf16y ntlmaxf18y ntllaf10y ntllaf12y ntllaf14y ntllaf15y ntllaf16y ntllaf18y ntlvf10y ntlvf12y ntlvf14y ntlvf15y ntlvf16y ntlvf18y, i(iso3) j(year)

rename n*y n*
qui reshape long ntlpf ntlmaxf ntllpf ntllaf ntlvf, i(iso3 year) j(satellite)
rename n*f n*

drop if mi(ntlp)

*<<< VIIRS NTL

preserve
	import delimited "${projdir}\DATA\RAW\WORLD\GEE\worldUnitsVIIRS.csv", clear 
	collapse (first) adm0_name geotype (sum) areaha-ntlv2023, by(iso3)
	qui reshape long ntlp ntllp ntlla ntlv, i(adm0_name) j(year)
	drop if year == 2012
	gen satellite = 20 // Virrs is coded as satellite 20
	gen ntlmax = 0 // No saturation with VIRSS
	compress
	tempfile WORLDUNITVIIRS
	save `"`WORLDUNITVIIRS'"', replace
restore

merge 1:1 iso3 satellite year using `"`WORLDUNITVIIRS'"', keep(master using matched) nogen

*<<< For years when several measurement are available we take the average measure 

collapse (first) adm0_name geotype areaha satellite (mean) ntlp ntllp ntlmax ntlla ntlv, by(iso3 year)

la var iso3 "ISO3 country code"
la var adm0_name "UN country name"
la var geotype "Unit shape"
la var areaha "Area (ha)"
la var ntlp "Total pixels"
la var ntllp "Lit pixels"
la var ntlmax "Lit saturated pixels"
la var ntlla "Lit area (ha)"
la var ntlv "Avg annual brightness"
la var year "Year"
la var satellite "Satellite"

*<<< MODIS LAND COVER

preserve
	import delimited "${projdir}\DATA\RAW\WORLD\GEE\worldUnitsMODISLCArea.csv", clear 
	collapse (first) adm0_name geotype (sum) areaha-grasslandha2023, by(iso3)
	qui reshape long forestha grasslandha croplandha, i(adm0_name) j(year)
	
	la var forestha "Forest cover (ha)"
	la var grasslandha "Grassland (ha)"
	la var croplandha "Cropland (ha)"
	
	egen croplandha_ext = rowtotal(grasslandha croplandha)
	la var croplandha_ext "Grassland + Cropland (ha)"
	
	compress
	tempfile WORLDUNITMODIS
	save `"`WORLDUNITMODIS'"', replace
restore

merge 1:1 iso3 year using `"`WORLDUNITMODIS'"', keep(master using matched) nogen

*<<< GDP DATA

* Temporarily rename countrycode to iso3 in the using dataset
rename iso3 country_code
merge 1:1 country_code year using "${projdir}\DATA\BUILD\WORLD\WDI\WDI CLEAN.dta", ///
    keep(matched) keepusing(country* *region* income* gdp_cd gdp_ag_cd gdp_ind_cd gdp_serv_cd) ///
    nogen
rename country_code iso3 // rename back

gen gdp_nag_cd = gdp_ind_cd + gdp_serv_cd
la var  gdp_nag_cd "GDP outside agriculture (Current USD)"

/// create log (1+x) variables for the vars below
foreach v of varlist gdp_cd gdp_ag_cd gdp_nag_cd gdp_ind_cd gdp_serv_cd ntlla ntlv ntlmax forestha grasslandha croplandha croplandha_ext {
	cap drop l`v'
	gen l`v' = log(1+`v')			
	la var l`v' "Log `: var la `v''"
}

la var lntlmax "Log Lit saturated pixels"

// place each year into its decade
keep if inrange(year, 1993, 2023)
recode year (1991/2000 = 1990) (2001/2010 = 2000) (2011/2020 = 2010), gen(decade)
la var decade "Decades"

gen viirs = (year >= 2013 & year <= 2023)
la var viirs "Nighttimioe lights measured with VIIRS Satellites"

save "${projdir}\DATA\BUILD\WORLD\WorldNTLGDP.dta", replace

***************************************************************
* Part 2: Nighttime light, land cover at the admin unit level *
***************************************************************
// like part 1 - SampleNTLGDP is iniitally a year x admin unit dataset with nightlight pixel and area values (from different satellites), as well as data on area by land classification (grass, forest etc.) in this part, if there is more than a single satellite for a given unit x year, we take the maximum rather, unlike in the country level calculation where we took the average 

*<<< DMSP NTL

import delimited "${projdir}\DATA\RAW\AFRICA\NIGHT LIGHT\sampleUnitsDMSP.csv", clear 

qui reshape long ntlpf10y ntlpf12y ntlpf14y ntlpf15y ntlpf16y ntlpf18y ntllpf10y ntllpf12y ntllpf14y ntllpf15y ntllpf16y ntllpf18y ntlmaxf10y ntlmaxf12y ntlmaxf14y ntlmaxf15y ntlmaxf16y ntlmaxf18y ntllaf10y ntllaf12y ntllaf14y ntllaf15y ntllaf16y ntllaf18y ntlvf10y ntlvf12y ntlvf14y ntlvf15y ntlvf16y ntlvf18y, i(ipums_id) j(year)

rename n*y n*
qui reshape long ntlpf ntlmaxf ntllpf ntllaf ntlvf, i(ipums_id year) j(satellite)
rename n*f n*

drop if mi(ntlp)

*<<< VIIRS NTL

preserve
	import delimited "${projdir}\DATA\RAW\AFRICA\NIGHT LIGHT\sampleUnitsVIIRS.csv", clear 
	qui reshape long ntlp ntllp ntlla ntlv, i(ipums_id) j(year)
	gen satellite = 20 // Virrs is coded as satellite 20
	gen ntlmax = 0 // No saturation with VIRSS
	compress
	tempfile ADMINUNITVIIRS
	save `"`ADMINUNITVIIRS'"', replace
restore

merge 1:1 ipums_id satellite year using `"`ADMINUNITVIIRS'"', keep(master using matched) nogen

*<<< For years when several measurement are available we take the measure with the highest values

collapse (first) country admin_name geotype areaha satellite (max) ntlp ntllp ntlmax ntlla ntlv, by(ipums_id year)

la var country "Country"
la var admin_name "Admin unit"
la var ipums_id "IPUMS ID"
la var geotype "Unit shape"
la var areaha "Area (ha)"
la var ntlp "Total pixels"
la var ntllp "Lit pixels"
la var ntlmax "Lit saturated pixels"
la var ntlla "Lit area (ha)"
la var ntlv "Avg annual brightness"
la var year "Year"
la var satellite "Satellite"

*<<< MODIS LAND COVER

preserve
	import delimited "${projdir}\DATA\RAW\AFRICA\MODIS_LC\sampleUnitsMODISLCArea.csv", clear 
	qui reshape long forestha grasslandha croplandha, i(ipums_id) j(year)
	
	la var forestha "Forest cover (ha)"
	la var grasslandha "Grassland (ha)"
	la var croplandha "Cropland (ha)"
	
	egen croplandha_ext = rowtotal(grasslandha croplandha)
	la var croplandha_ext "Grassland + Cropland (ha)"
	
	compress
	tempfile WORLDUNITMODIS
	save `"`WORLDUNITMODIS'"', replace
restore

merge 1:1 ipums_id year using `"`WORLDUNITMODIS'"', keep(master using matched) nogen


*<<< Data cleaning

foreach v of varlist ntlla ntlv ntlmax forestha grasslandha croplandha croplandha_ext {
	cap drop l`v'
	gen l`v' = log(1+`v')			
	la var l`v' "Log `: var la `v''"
}
la var lntlmax "Log Lit saturated pixels"

keep if inrange(year, 1993, 2023)
recode year (1991/2000 = 1990) (2001/2010 = 2000) (2011/2020 = 2010), gen(decade)
la var decade "Decades"

gen viirs = (year >= 2013 & year <= 2023)
la var viirs "Nighttimioe lights measured with VIIRS Satellites"

xtset ipums_id year, yearly

compress
save "${projdir}\DATA\BUILD\AFRICA\NIGHT LIGHT\SampleNTLGDP_no_predict.dta", replace