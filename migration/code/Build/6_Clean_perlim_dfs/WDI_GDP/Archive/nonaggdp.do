*************************************************************************
* THIS PROGRAM CREATES A DATASET DESCRIBING HOW AREA LIT AND NIGHT TIME *
*     LIGHT INTENSITY VARY OVER TIME AND ACROSS ADMINISTRATIVE UNITS    *
*************************************************************************

/*
	wbopendata, language(en - English) country()  indicator( "NV.IND.TOTL.KD"; "NV.SRV.TOTL.KD"; "NV.AGR.TOTL.KD") clear long
	keep countrycode countryname year nv_agr_totl_kd nv_ind_totl_kd nv_srv_totl_kd
	save "${work}\WDI.dta", replace 
*/
*  PREAMBLE  *

if "`c(username)'" == "houngbedji"{
	global projdir = "C:\USERS\HOUNGBEDJI\DROPBOX (COMPTE PERSONNEL)\MIGRATION AFRICA"
}

cd "${projdir}"

global ntld "..\MIGRATION AFRICA\DATA\RAW\AFRICA\NIGHT LIGHT"
global work "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\NIGHT LIGHT"

cap log close NONAGGDP

log using "${work}\NONAGGDP.txt", name(NONAGGDP) replace text

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

preserve
	import delimited "${ntld}\country_iso_codes.csv", varnames(1) clear 
	rename iso3code countrycode
	tempfile COUNTRYCODE
	save `"`COUNTRYCODE'"', replace
restore

merge m:1 country using `"`COUNTRYCODE'"', keep(master using matched) nogen

** Non agriculture value added at the country level




merge m:1 countrycode year using "${work}\WDI.dta", keep(master matched) nogen

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
