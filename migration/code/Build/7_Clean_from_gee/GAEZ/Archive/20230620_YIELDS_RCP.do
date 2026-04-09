**********************************************************************************
* THIS PROGRAM CREATES A DATASET REPORTING POTENTIAL YIELD ACROSS ADMINISTRATIVE *
*            UNITS BY 2040 UNDER DIFFERENT CO2 CONCENTRATION PATHWAYS            *
**********************************************************************************

**************
*  PREAMBLE  *
**************

if "`c(username)'" == "houngbedji"{
	global projdir = "C:\USERS\HOUNGBEDJI\DROPBOX (COMPTE PERSONNEL)\MIGRATION AFRICA"
}

cd "${projdir}"

global rgaez "..\MIGRATION AFRICA\DATA\RAW\AFRICA\GAEZ DATA"
global bgaez "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\GAEZ DATA"

global work "C:\Users\houngbedji\Dropbox (IRD)\PROJECTS\MIGRATION\WORK"

cap log close GAEZRCP

log using "${bgaez}\GAEZRCP.txt", name(GAEZRCP) replace text

***************************************
* IMPORTING GAEZ DATA INTO STATA FILE *
***************************************

forvalues i = 1/2{
	
	import delimited "${rgaez}\PY2040GAEZADM`i'.csv", clear bindquote(strict)
	cap drop if mi(geo)
	
	order *, alphabetic 
	order cntry_name cntry_code geolevel
	
	ren *040r*s *2040rs*
	ren *040r*y *2040ry*
		
	reshape long alfa2040rs bana2040rs barl2040rs bckw2040rs bean2040rs bhsg2040rs blsg2040rs bsrg2040rs btsg2040rs cabb2040rs carr2040rs casv2040rs chck2040rs citr2040rs cocc2040rs coch2040rs cocn2040rs coco2040rs cofa2040rs coff2040rs cofr2040rs cott2040rs cowp2040rs cyam2040rs dpea2040rs flax2040rs fmlt2040rs gram2040rs gras2040rs grlg2040rs grnd2040rs gyam2040rs hmze2040rs hsrg2040rs jatr2040rs lmze2040rs lsrg2040rs maiz2040rs misc2040rs mllt2040rs mzsi2040rs napr2040rs oats2040rs oilp2040rs oliv2040rs onio2040rs pigp2040rs pmlt2040rs prub2040rs rape2040rs rcgr2040rs ricd2040rs ricw2040rs ryes2040rs sbrl2040rs sorg2040rs soyb2040rs spot2040rs srye2040rs sugb2040rs sugc2040rs sunf2040rs swgr2040rs swhe2040rs teas2040rs tmze2040rs toba2040rs toma2040rs tsrg2040rs wbrl2040rs whea2040rs wpot2040rs wrye2040rs wwhe2040rs wyam2040rs yams2040rs yyam2040rs alfa2040ry bana2040ry barl2040ry bckw2040ry bean2040ry bhsg2040ry blsg2040ry bsrg2040ry btsg2040ry cabb2040ry carr2040ry casv2040ry chck2040ry citr2040ry cocc2040ry coch2040ry cocn2040ry coco2040ry cofa2040ry coff2040ry cofr2040ry cott2040ry cowp2040ry cyam2040ry dpea2040ry flax2040ry fmlt2040ry gram2040ry gras2040ry grlg2040ry grnd2040ry gyam2040ry hmze2040ry hsrg2040ry jatr2040ry lmze2040ry lsrg2040ry maiz2040ry misc2040ry mllt2040ry mzsi2040ry napr2040ry oats2040ry oilp2040ry oliv2040ry onio2040ry pigp2040ry pmlt2040ry prub2040ry rape2040ry rcgr2040ry ricd2040ry ricw2040ry ryes2040ry sbrl2040ry sorg2040ry soyb2040ry spot2040ry srye2040ry sugb2040ry sugc2040ry sunf2040ry swgr2040ry swhe2040ry teas2040ry tmze2040ry toba2040ry toma2040ry tsrg2040ry wbrl2040ry whea2040ry wpot2040ry wrye2040ry wwhe2040ry wyam2040ry yams2040ry yyam2040ry, i(cntry_code geolevel) j(rcp)
	
	rename *2040ry *2040yld
	rename *2040rs *2040har
	
	foreach var in alfa2040 bana2040 barl2040 bckw2040 bean2040 bhsg2040 blsg2040 bsrg2040 btsg2040 cabb2040 carr2040 casv2040 chck2040 citr2040 cocc2040 coch2040 cocn2040 coco2040 cofa2040 coff2040 cofr2040 cott2040 cowp2040 cyam2040 dpea2040 flax2040 fmlt2040 gram2040 gras2040 grlg2040 grnd2040 gyam2040 hmze2040 hsrg2040 jatr2040 lmze2040 lsrg2040 maiz2040 misc2040 mllt2040 mzsi2040 napr2040 oats2040 oilp2040 oliv2040 onio2040 pigp2040 pmlt2040 prub2040 rape2040 rcgr2040 ricd2040 ricw2040 ryes2040 sbrl2040 sorg2040 soyb2040 spot2040 srye2040 sugb2040 sugc2040 sunf2040 swgr2040 swhe2040 teas2040 tmze2040 toba2040 toma2040 tsrg2040 wbrl2040 whea2040 wpot2040 wrye2040 wwhe2040 wyam2040 yams2040 yyam2040 {
		qui replace `var'yld = . if (`var'yld == 0 | `var'har == 0)
		qui replace `var'har = . if (`var'yld == 0 | `var'har == 0)
		qui gen _`var' = `var'yld*`var'har
	}
	
	egen allc2040yld = rowmean(_*)
	la var allc2040yld "Yield (Kg/ha)"
	cap drop _*
	
	rename geolevel adm_code
	gen adm_lvl = `i'
	
	keep cntry_name cntry_code adm_lvl adm_code rcp *yld
	order *, alphabetic 
	order cntry_name cntry_code adm_lvl adm_code rcp
	
	compress
	save "${bgaez}\PY20140GAEZADM`i'.dta", replace	
}

*NOW WE COMBINE ADM1 AND ADM2 DATA

use "${bgaez}\PY20140GAEZADM1.dta", clear

append using "${bgaez}\PY20140GAEZADM2.dta", force
sort cntry_name cntry_code adm_lvl adm_code rcp

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

merge m:1 cntry_name adm_lvl adm_code using `"`ADMUNITS'"', keepusing(cntry_iso ipums_id admin_id admin_name) keep(using matched)

list cntry_iso cntry_name adm_lvl adm_code ipums_id admin_id admin_name if _m == 2 // GAEZ DATA ARE NOT AVAILABLE FOR SUDAN

drop if _m ==2
drop _m

order cntry_name cntry_code cntry_iso adm_lvl adm_code ipums_id admin_id admin_name rcp
sort cntry_name adm_lvl adm_code rcp
compress
save "${bgaez}\PY2040GAEZ.dta", replace	

*EXPORTING AVERAGE YIELDS BY 2040 TO CSV

keep cntry_name adm_lvl adm_code rcp allc2040yld

rename allc2040yld p2040yld
keep cntry_name adm_lvl adm_code rcp p2040yld
sort cntry_name adm_lvl adm_code rcp 

export delimited using "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\GAEZ DATA\Y2040.csv", replace

compress
save "${bgaez}\PY2040RCP.dta", replace	

log close GAEZRCP