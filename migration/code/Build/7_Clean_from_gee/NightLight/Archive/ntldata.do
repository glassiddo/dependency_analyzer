*************************************************************************
* THIS PROGRAM CREATES A DATASET DESCRIBING HOW AREA LIT AND NIGHT TIME *
*     LIGHT INTENSITY VARY OVER TIME AND ACROSS ADMINISTRATIVE UNITS    *
*************************************************************************

**************
*  PREAMBLE  *
**************

cd "${projdir}"

global ntld "..\MIGRATION AFRICA\DATA\RAW\AFRICA\NIGHT LIGHT"
global work "..\MIGRATION AFRICA\DATA\BUILD\AFRICA\NIGHT LIGHT"


cap log close NTLDATA

log using "${work}\NTLDATA.txt", name(NTLDATA) replace text

***************************************************
* IMPORTING NTL DATA INTO A STRUCTURED STATA FILE *
***************************************************

forvalues i = 1/2{
	
	** HARMONIZED GLOBAL NTL
	
	import delimited "${ntld}\HGNTLAREAADM`i'.csv", clear 
	
	cap drop if mi(codeadm`i')
	
	preserve
		import delimited "${ntld}\HGNTLVALUEADM`i'.csv", clear 
		cap drop if mi(codeadm`i')
		tempfile HGNTLVALUEADM`i'
		save `"`HGNTLVALUEADM`i''"', replace
	restore
	
	merge 1:1 codeadm0 codeadm`i' using `"`HGNTLVALUEADM`i''"', keepusing(ntlv*) keep(master matched) nogen
	
	reshape long ntladmsp ntlaviirs ntlvdmsp ntlvviirs, i(codeadm0 codeadm`i') j(year)
	
	gen ntlahg = cond(year<= 2013, ntladmsp, ntlaviirs)
	
	gen ntlvhg = cond(year<= 2013, ntladmsp, ntlaviirs)
	
	compress
	save "${work}\HGNTLADM`i'.dta", replace
	
	** CONSISTENT AND CORRECTED NTL SERIES FROM ZHAO ET AL. (2020)
	
	import delimited "${ntld}\CCNTLAREAADM`i'.csv", clear 
	
	cap drop if mi(codeadm`i')
	
	preserve
		import delimited "${ntld}\CCNTLVALUEADM`i'.csv", clear 
		cap drop if mi(codeadm`i')
		tempfile CCNTLVALUEADM`i'
		save `"`CCNTLVALUEADM`i''"', replace
	restore
	
	merge 1:1 codeadm0 codeadm`i' using `"`CCNTLVALUEADM`i''"', keepusing(ccntlv*) keep(master matched) nogen
	
	reshape long ccntlay ccntlvy, i(codeadm0 codeadm`i') j(year)
	
	rename ccntl*y ntl*cc
	
	compress
	save "${work}\CCNTLADM`i'.dta", replace
	
	** RAW DMSP NTL SERIES AT DUSK (1992-2013)
	
	import delimited "${ntld}\NTLAREAADM`i'.csv", clear 
	
	cap drop if mi(codeadm`i')
	
	preserve
		import delimited "${ntld}\NTLVALUEADM`i'.csv", clear 
		cap drop if mi(codeadm`i')
		tempfile NTLVALUEADM`i'
		save `"`NTLVALUEADM`i''"', replace
	restore
	
	merge 1:1 codeadm0 codeadm`i' using `"`NTLVALUEADM`i''"', keepusing(ntlv*) keep(master matched) nogen
	
	reshape long ntlaf10y ntlvf10y ntlaf12y ntlvf12y ntlaf14y ntlvf14y ntlaf15y ntlvf15y ntlaf16y ntlvf16y ntlaf18y ntlvf18y, i(codeadm0 codeadm`i') j(year)
	
	rename (ntla*y ntlv*y) (ntla* ntlv*)
	
	egen ntladu = rowmean(ntlaf1*)
	
	egen ntlvdu = rowmean(ntlvf1*)
		
	keep codeadm0 codeadm`i' year ntl*
	
	compress
	save "${work}\DUNTLADM`i'.dta", replace
	
	** RAW DMSP NTL SERIES AT DAWN (2013-21)
	
	import delimited "${ntld}\NTLDAWNADM`i'.csv", bindquote(strict) clear 
	
	rename (cntry_name cntry_code admin_name geolevel`i') (nameadm0 codeadm0 nameadm`i' codeadm`i')
	
	cap drop if mi(codeadm`i')
	
	rename nl* ntl*
	
	reshape long ntlaf15 ntlvf15 ntlaf16 ntlvf16, i(codeadm0 codeadm`i') j(year)
	
	egen ntlada = rowmean(ntlaf1*)
	
	egen ntlvda = rowmean(ntlvf1*)
	
	drop if year > 2020
	
	keep codeadm0 codeadm`i' year ntl*
	
	compress
	save "${work}\DANTLADM`i'.dta", replace
	
	** MERGING NTL SERIES TOGETHER
	
	use "${work}\HGNTLADM`i'.dta", clear
	
	merge 1:1 codeadm0 codeadm`i' year using "${work}\CCNTLADM`i'.dta", keepusing(ntl*) keep(master matched) nogen
	merge 1:1 codeadm0 codeadm`i' year using "${work}\DUNTLADM`i'.dta", keepusing(ntl*) keep(master matched) nogen
	merge 1:1 codeadm0 codeadm`i' year using "${work}\DANTLADM`i'.dta", keepusing(ntl*) keep(master matched) nogen
	
	if "`i'" == "1" {
		local newnameadm "region_name"		
	}
	else {
		local newnameadm "district_name"
	}
	
	rename (codeadm0 nameadm0 codeadm`i' nameadm`i') (country_code country geolevel`i' `newnameadm')
	
	gen ipums_id = geolevel`i'
	gen geo_lvl = `i'
	
	compress
	save "${work}\NTLLVL`i'.dta", replace
	
}
