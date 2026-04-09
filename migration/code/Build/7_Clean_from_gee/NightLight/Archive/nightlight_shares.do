*************************************************************************
* THIS PROGRAM CREATES A DATASET DESCRIBING HOW AREA LIT AND NIGHT TIME *
*     LIGHT INTENSITY VARY OVER TIME AND ACROSS ADMINISTRATIVE UNITS    *
*************************************************************************

*  PREAMBLE  *


global work "${projdir}\DATA\BUILD\AFRICA\NIGHT LIGHT"
global ntld "${projdir}\DATA\RAW\AFRICA\NIGHT LIGHT"

//global ntld "..\MIGRATION AFRICA\DATA\RAW\AFRICA\NIGHT LIGHT"
//global work "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\NIGHT LIGHT"

cap log close nightlight_shares

log using "${work}\nightlight_shares.txt", name(nightlight_shares) replace text

** DMSP NTL

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
** instead of merging based on country name (which is inconsistent, eg cote divoire vs ivory coast) - use the country id in ipums which is the first three digits. keep the 0 (like botswana, 072/074 etc)
gen ipums_code_num = floor(ipums_id/1000)
gen str3 ipums_code = string(ipums_code_num, "%03.0f")

preserve
	import delimited "${ntld}\ipums_iso3_codes.csv", varnames(1) clear
	* force 3-digit string for matching
	tostring ipums_code, replace format(%03.0f)
	rename iso3 country_code
	tempfile COUNTRYCODE
	save `COUNTRYCODE', replace
restore

merge m:1 ipums_code using `"`COUNTRYCODE'"', keep(master using matched) nogen


** Non agriculture value added across admin unit

bys country_code satellite year (ipums_id): egen ntlv_tot = total(ntlv)
la var ntlv_tot "`: var la ntlv' - Country level"

gen ntlv_shr = ntlv/ntlv_tot
la var ntlv_shr "`: var la ntlv' - % Country"

** For years when several measurement are available we take the one based on the highest number of lit pixels across the country

bys country_code satellite year (ipums_id): egen ntllp_tot = total(ntllp)
bys country_code year (ipums_id): egen ntllp_tot_max = max(ntllp_tot)

sort country country_code ipums_id year ntllp_tot
bys country country_code ipums_id year (ntllp_tot): keep if _n == _N

cap drop ntllp_tot*

order country country_code admin_name ipums_id areaha year ntlv_shr ntlv ntlla
keep  country country_code admin_name ipums_id areaha year ntlv_shr ntlv ntlla

compress
save "${work}\nightlight_shares.dta", replace
