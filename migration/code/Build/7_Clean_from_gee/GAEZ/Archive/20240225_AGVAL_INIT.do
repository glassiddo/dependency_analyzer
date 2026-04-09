*****************************************************************************************
* THIS PROGRAM CREATES A DATASET REPORTING THE VALUE OF CROPS PRODUCED IN 2000 AND 2010 *
*                         ACROSS ADMINISTRATIVE UNITS                                   *
*****************************************************************************************

**************
*  PREAMBLE  *
**************

if "`c(username)'" == "houngbedji"{
	global projdir = "C:\USERS\HOUNGBEDJI\DROPBOX (COMPTE PERSONNEL)\MIGRATION AFRICA"
}

cd "${projdir}"

global rgaez "..\MIGRATION AFRICA\DATA\\RAW\AFRICA\GAEZ DATA"
global bgaez "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\GAEZ DATA"

cap log close GAEZVALDATA

log using "${bgaez}\GAEZVALDATA.txt", name(GAEZVALDATA) replace text

***************************************
* IMPORTING GAEZ DATA INTO STATA FILE *
***************************************

import delimited "${rgaez}\GAEZADMVAL.csv", clear bindquote(strict)
cap drop if mi(geo)

rename (*2000* *2010*) (**2000 **2010)

order *, alphabetic 
order cntry_name cntry_code bpl_code geo_id admin_name

order *2010, after(rtsval2000)

reshape long allval cerval oilval rtsval, i(cntry_code geo_id) j(year)

la var allval "All main crops (GK$)"
la var cerval "Cereal crops (GK$)"
la var oilval "Oil crops (GK$)"
la var rtsval "Root crops (GK$)"

rename geo_id adm_code
rename admin_name adm_name

keep  cntry_name cntry_code bpl_code adm_code adm_name year *val 
order *, alphabetic 
order cntry_name cntry_code bpl_code adm_code adm_name year

compress
save "${bgaez}\GAEZADMVAL.dta", replace	

*NOW WE ADD SOME NEW DATA

preserve
	use "..\MIGRATION AFRICA\DATA\FINAL\DATA_CLEAN.dta", clear
	keep ipums_id country_name geo_lvl admin_id admin_name region_name district_name Country geolevel2 geolevel1 admin_id_l1 admin_name_l1 admin_id_l2 admin_name_l2
	gen adm_code = cond(geo_lvl == 1, geolevel1, geolevel2)
	destring adm_code, replace
	ren geo_lvl adm_lvl
	ren Country cntry_iso
	ren country_name cntry_name
	tempfile ADMUNITS
	save `"`ADMUNITS'"', replace
restore

merge m:1 cntry_name adm_code using `"`ADMUNITS'"', keepusing(cntry_iso ipums_id admin_id admin_name) keep(using matched)

list cntry_iso cntry_name bpl_code adm_code adm_name ipums_id admin_id admin_name if _m == 2 // GAEZ DATA ARE NOT AVAILABLE FOR SUDAN

drop if _m ==2
drop _m

order cntry_name cntry_code cntry_iso adm_code adm_name ipums_id admin_id admin_name year
sort cntry_name adm_code adm_name year
compress
save "${bgaez}\YGAEZ.dta", replace	

keep if year == 2000

*NOW WE ADD DATA ON NIGHT TIME LIGHT

preserve
	use "..\MIGRATION AFRICA\DATA\FINAL\NIGHTLIGHT.dta", clear
	gen adm_code = cond(geo_lvl == 1, geolevel1, geolevel2)
	destring adm_code, replace
	ren geo_lvl adm_lvl
	ren Country cntry_iso
	ren country_name cntry_name
	keep cntry_name adm_lvl adm_code t0_ntlv delta_nlv delta_ntlv
	tempfile NIGHTLIGHT
	save `"`NIGHTLIGHT'"', replace
restore

merge 1:1 cntry_name adm_code using `"`NIGHTLIGHT'"', keepusing(t0_ntlv delta_nlv) keep(master matched) nogen

*EXPORTING VALUE OF AGRICULTURAL PRODUCTION TO CSV

keep cntry_name adm_name adm_code ipums_id allval t0_ntlv delta_nlv

keep cntry_name adm_name adm_code allval t0_ntlv delta_nlv
sort cntry_name adm_name adm_code 

export delimited using "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\GAEZ DATA\AGVALNTL.csv", replace

compress
save "${bgaez}\INITPARAM.dta", replace	

log close GAEZVALDATA