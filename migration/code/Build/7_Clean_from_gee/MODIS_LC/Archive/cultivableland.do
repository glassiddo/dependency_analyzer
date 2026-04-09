****************************************************************************
*  THIS PROGRAM CREATES A DATASET REPORTING AREA OF NON FORESTED CROPLAND  *
* OVER TIME AND ACROSS ADMINISTRATIVE UNITS USING MODIS LAND COVER DATASET *
****************************************************************************

**************
*  PREAMBLE  *
**************

// cd "${projdir}"
cd "C:\USERS\HOUNGBEDJI\DROPBOX (COMPTE PERSONNEL)\MIGRATION AFRICA"
//cd "C:\USERS\IDDO2\DROPBOX\MIGRATION AFRICA"
global modis  "..\MIGRATION AFRICA\DATA\RAW\AFRICA\MODIS_LC"
global build "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\MODIS_LC"
global final "..\MIGRATION AFRICA\DATA\FINAL"

cap log close CULTIVABLELAND

log using "${build}\CULTIVABLELAND.txt", name(CULTIVABLELAND) replace text

** importing Modis data aggregated at the admin level over time

global layers "lc_prop2 lc_type1 lc_type2 lc_type3 lc_type4 lc_type5"

foreach layer of global layers {
	import delimited "${modis}\sampleUnits`layer'.csv", clear
	keep ipums_id country admin_name areaha geotype `layer'*a*
	di as text "test"
	rename `layer'_*a* `layer'_*[2]a*[1]
	if "`layer'" == "lc_type1" {
		reshape long `layer'_01a `layer'_02a `layer'_03a `layer'_04a `layer'_05a `layer'_06a `layer'_07a `layer'_08a `layer'_09a `layer'_10a `layer'_11a `layer'_12a `layer'_13a `layer'_14a `layer'_15a `layer'_16a `layer'_17a, i(ipums_id) j(year)
		egen tarea_`layer'= rowtotal(`layer'_*a)
		egen farea_`layer'= rowtotal(`layer'_01a `layer'_02a `layer'_03a `layer'_04a `layer'_05a)
		egen carea_`layer'= rowtotal(`layer'_12a `layer'_14a)
		egen barea_`layer'= rowtotal(`layer'_13a)
		cap drop `layer'_*a
	}	
	if "`layer'" == "lc_type2" {
		reshape long `layer'_00a `layer'_01a `layer'_02a `layer'_03a `layer'_04a `layer'_05a `layer'_06a `layer'_07a `layer'_08a `layer'_09a `layer'_10a `layer'_11a `layer'_12a `layer'_13a `layer'_14a `layer'_15a, i(ipums_id) j(year)
		egen tarea_`layer'= rowtotal(`layer'_*a)
		egen farea_`layer'= rowtotal(`layer'_01a `layer'_02a `layer'_03a `layer'_04a `layer'_05a)
		egen carea_`layer'= rowtotal(`layer'_12a `layer'_14a)
		egen barea_`layer'= rowtotal(`layer'_13a)
		cap drop `layer'_*a
	}	
	if "`layer'" == "lc_type3" {
		reshape long `layer'_00a `layer'_01a `layer'_02a `layer'_03a `layer'_04a `layer'_05a `layer'_06a `layer'_07a `layer'_08a `layer'_09a `layer'_10a, i(ipums_id) j(year)
		egen tarea_`layer'= rowtotal(`layer'_*a)
		egen farea_`layer'= rowtotal(`layer'_05a `layer'_06a `layer'_07a `layer'_08a)
		egen carea_`layer'= rowtotal(`layer'_01a `layer'_03a)
		egen barea_`layer'= rowtotal(`layer'_10a)
		cap drop `layer'_*a
	}
	if "`layer'" == "lc_type4" {
		reshape long `layer'_00a `layer'_01a `layer'_02a `layer'_03a `layer'_04a `layer'_05a `layer'_06a `layer'_07a `layer'_08a, i(ipums_id) j(year)
		egen tarea_`layer'= rowtotal(`layer'_*a)
		egen farea_`layer'= rowtotal(`layer'_01a `layer'_02a `layer'_03a `layer'_04a)
		egen carea_`layer'= rowtotal(`layer'_05a `layer'_06a)
		egen barea_`layer'= rowtotal(`layer'_08a)
		cap drop `layer'_*a
	}
	if "`layer'" == "lc_type5" {
		reshape long `layer'_00a `layer'_01a `layer'_02a `layer'_03a `layer'_04a `layer'_05a `layer'_06a `layer'_07a `layer'_08a `layer'_09a `layer'_10a `layer'_11a, i(ipums_id) j(year)
		egen tarea_`layer'= rowtotal(`layer'_*a)
		egen farea_`layer'= rowtotal(`layer'_01a `layer'_02a `layer'_03a `layer'_04a)
		egen carea_`layer'= rowtotal(`layer'_07a `layer'_08a)
		egen barea_`layer'= rowtotal(`layer'_09a)
		cap drop `layer'_*a
	}
	if "`layer'" == "lc_prop2" {
		reshape long `layer'_01a `layer'_03a `layer'_09a `layer'_10a `layer'_20a `layer'_25a `layer'_30a `layer'_35a `layer'_36a `layer'_40a, i(ipums_id) j(year)
		egen tarea_`layer'= rowtotal(`layer'_*a)
		egen farea_`layer'= rowtotal(`layer'_10a `layer'_20a)
		egen carea_`layer'= rowtotal(`layer'_25a `layer'_35a `layer'_36a)
		egen barea_`layer'= rowtotal(`layer'_09a)
		cap drop `layer'_*a
	}
	compress
	save "${build}\sampleUnits`layer'.dta", replace
}

** merging modis data

use "${build}\sampleUnitslc_prop2.dta", clear

foreach layer of global layers {
	if "`layer'"!="lc_prop2"  {
		merge 1:1 ipums_id year using "${build}\sampleUnits`layer'.dta", nogen 
	}
}


01 03 09 10 20 25 30 35 36 40

import delimited "${ntld}\sampleUnitsDmspNightLight.csv", clear 

reshape long ntlpf10y ntlpf12y ntlpf14y ntlpf15y ntlpf16y ntlpf18y ntllpf10y ntllpf12y ntllpf14y ntllpf15y ntllpf16y ntllpf18y ntllaf10y ntllaf12y ntllaf14y ntllaf15y ntllaf16y ntllaf18y ntlvf10y ntlvf12y ntlvf14y ntlvf15y ntlvf16y ntlvf18y, i(ipums_id) j(year)

rename n*y n*
reshape long ntlpf ntllpf ntllaf ntlvf, i(ipums_id year) j(satellite)
rename n*f n*

** VIIRS NTL

preserve
	import delimited "${ntld}\sampleUnitsViirsNightLight.csv", clear 
	reshape long ntlp ntllp ntlla ntlv, i(ipums_id) j(year)
	drop if year == 2012
	gen satellite = 20 // Virrs is coded as satellite 20
	compress
	tempfile ADMINUNITVIIRS
	save `"`ADMINUNITVIIRS'"', replace
restore

merge 1:1 ipums_id satellite year using `"`ADMINUNITVIIRS'"', keep(master using matched) nogen

la var country "Country"
la var admin_name "Admin unit"
la var ipums_id "IPUMS ID"
la var geotype "Admin unit shape"
la var areaha "Area (ha)"
la var ntlp "Total pixels"
la var ntllp "Lit pixels"
la var ntlla "Lit area (ha)"
la var ntlv "Avg annual brightness"
la var year "Year"
la var satellite "Satellite"

** ISO-3 codes

preserve
	import delimited "${ntld}\country_iso_codes.csv", varnames(1) clear 
	rename iso3code countrycode
	tempfile COUNTRYCODE
	save `"`COUNTRYCODE'"', replace
restore

merge m:1 country using `"`COUNTRYCODE'"', keep(master using matched) nogen

** Non agriculture value added at the country level

preserve
	clear
	wbopendata, language(en - English) country()  indicator( "NV.IND.TOTL.KD"; "NV.SRV.TOTL.KD"; "NV.AGR.TOTL.KD") clear long
	keep countrycode countryname year nv_agr_totl_kd nv_ind_totl_kd nv_srv_totl_kd
	tempfile WDI
	save `"`WDI'"', replace
restore

merge m:1 countrycode year using `"`WDI'"', keep(master matched) nogen

egen nv_nag_totl_kd = rowtotal(nv_ind_totl_kd nv_srv_totl_kd)
la var  nv_nag_totl_kd "Non agriculture, value added (constant 2015 US$)"

** Non agriculture value added across admin unit

bys country satellite year (ipums_id): egen ntlv_tot = total(ntlv)
la var ntlv_tot "`: var la ntlv' - Country level"

gen ntlv_shr = ntlv/ntlv_tot
la var ntlv_shr "`: var la ntlv' - % Country"

gen nonaggdp_kd = nv_nag_totl_kd*ntlv_shr
la var  nonaggdp_kd "Total non agriculture value added in admin unit (constant 2015 US$)"

** For years when several measurement are available we take the one based on the highest number of lit pixels across the country

bys country satellite year (ipums_id): egen ntllp_tot = total(ntllp)
bys country year (ipums_id): egen ntllp_tot_max = max(ntllp_tot)

sort country ipums_id year ntllp_tot
bys country ipums_id year (ntllp_tot): keep if _n == _N

cap drop ntllp_tot*

order country countrycode admin_name ipums_id areaha year ntlv_shr nonaggdp_kd
keep  country countrycode admin_name ipums_id areaha year ntlv_shr nonaggdp_kd

compress
save "${work}\nonaggdp.dta", replace
