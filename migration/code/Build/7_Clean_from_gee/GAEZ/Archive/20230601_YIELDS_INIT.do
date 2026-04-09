**********************************************************************************
* THIS PROGRAM CREATES A DATASET REPORTING YIELD, POTENTIAL YIELD AND YIELD GAPS *
*                      ACROSS ADMINISTRATIVE UNITS                               *
*********************************************************************************

**************
*  PREAMBLE  *
**************

if "`c(username)'" == "houngbedji"{
	global projdir = "C:\USERS\HOUNGBEDJI\DROPBOX (COMPTE PERSONNEL)\MIGRATION AFRICA"
}

cd "${projdir}"

global rgaez "..\MIGRATION AFRICA\DATA\\RAW\AFRICA\GAEZ DATA"
global bgaez "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\GAEZ DATA"


cap log close GAEZDATA

log using "${bgaez}\GAEZDATA.txt", name(GAEZDATA) replace text

***************************************
* IMPORTING GAEZ DATA INTO STATA FILE *
***************************************

forvalues i = 1/2{
	
	import delimited "${rgaez}\AYGAEZADM`i'.csv", clear bindquote(strict)
	cap drop if mi(geo)
	
	preserve
		import delimited "${rgaez}\PYGAEZADM`i'.csv", clear bindquote(strict)
		cap drop if mi(geo)
		tempfile YIELDGAP
		save `"`YIELDGAP'"', replace
	restore
	
	merge 1:1 cntry_code geolevel using `"`YIELDGAP'"', keepusing(*00* *10*) keep(master matched) nogen
	
	rename (*2000* *2010*) (**2000 **2010)
	
	order *, alphabetic 
	order cntry_name cntry_code geolevel
	order *2010, after(wheyld2000)
	
	reshape long banhar banprd banqga banyld brlhar brlprd brlqga brlyld cc2har cc2prd cc2yld cothar cotprd cotqga cotyld fddhar fddprd fddyld frthar frtprd frtyld grdhar grdprd grdqga grdyld mlthar mltprd mltqga mltyld mzehar mzeprd mzeqga mzeyld neshar nesprd nesyld ocehar oceprd oceqga oceyld olphar olpprd olpqga olpyld olvhar olvprd olvqga olvyld plshar plsprd plsqga plsyld rcwhar rcwprd rcwqga rcwyld rsdhar rsdprd rsdqga rsdyld rt1har rt1prd rt1qga rt1yld rt2har rt2prd rt2qga rt2yld rt3har rt3prd rt3qga rt3yld sflhar sflprd sflqga sflyld soyhar soyprd soyqga soyyld srghar srgprd srgqga srgyld subhar subprd subqga subyld suchar sucprd sucqga sucyld tobhar tobprd tobqga tobyld veghar vegprd vegyld whehar wheprd wheqga wheyld, i(cntry_code geolevel) j(year)
	
	egen allhar = rowtotal(*har)
	egen allqga = rowtotal(*qga)
	egen allprd = rowtotal(*prd)
	gen  allyld = allprd/allhar
	
	la var allhar "Area cultivated (ha)"
	la var allprd "Production harvested (t)"
	la var allqga "Production gap (t)"
	la var allyld "Yield (t/ha)"
	
	foreach var of varlist *qga {
		local pvar `:subinstr local var "qga"  "pga" '
		local rvar `:subinstr local var "qga"  "prd" '
		local avar `:subinstr local var "qga"  "har" '
		local pyld `:subinstr local var "qga"  "pot" '
		egen `pvar' = rowtotal(`var' `rvar') // potential output
		gen  `pyld' = `pvar'/(`avar') // potential yield
	}
	
	rename geolevel adm_code
	gen adm_lvl = `i'
	
	keep cntry_name cntry_code adm_lvl adm_code year *yld *pot 
	order *, alphabetic 
	order cntry_name cntry_code adm_lvl adm_code year
	
	compress
	save "${bgaez}\YGAEZADM`i'.dta", replace	
}

*NOW WE COMBINE ADM1 AND ADM2 DATA

use "${bgaez}\YGAEZADM1.dta", clear

append using "${bgaez}\YGAEZADM2.dta", force
sort cntry_name cntry_code adm_lvl adm_code year

preserve
	use "..\MIGRATION AFRICA\DATA\FINAL\DATA_CLEAN.dta", clear
	bys ipums_id (fl_end_yr_t00): keep if _n == 1
	keep ipums_id country_name geo_lvl admin_id admin_name Country geolevel2 geolevel1 admin_id_l1 admin_name_l1 admin_id_l2 admin_name_l2
	gen adm_code = cond(geo_lvl == 1, geolevel1, geolevel2)
	destring adm_code, replace
	ren geo_lvl adm_lvl
	ren Country cntry_iso
	ren country_name cntry_name
	tempfile ADMUNITS
	save `"`ADMUNITS'"', replace
restore

merge m:1 cntry_name adm_lvl adm_code using `"`ADMUNITS'"', keepusing(cntry_iso ipums_id admin_id admin_name) keep(using matched)

list cntry_iso cntry_name adm_lvl adm_code ipums_id admin_id admin_name if _m == 2 // GAEZ DATA ARE NOT AVAILABLE FOR SUDAN

drop if _m ==2
drop _m

order cntry_name cntry_code cntry_iso adm_lvl adm_code ipums_id admin_id admin_name year
sort cntry_name adm_lvl adm_code year
compress
save "${bgaez}\YGAEZ.dta", replace	

keep if year == 2000

*NOW WE COMBINE ADM1 AND ADM2 DATA

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

merge 1:1 cntry_name adm_lvl adm_code using `"`NIGHTLIGHT'"', keepusing(t0_ntlv delta_nlv) keep(master matched) nogen

*EXPORTING POTENTIAL YIELDS TO CSV

keep cntry_name adm_lvl adm_code allpot t0_ntlv delta_nlv

rename allpot pyld
keep cntry_name adm_lvl adm_code pyld t0_ntlv delta_nlv
sort cntry_name adm_lvl adm_code 

export delimited using "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\GAEZ DATA\YIELDSNTL.csv", replace

compress
save "${bgaez}\INITPARAM.dta", replace	

log close GAEZDATA