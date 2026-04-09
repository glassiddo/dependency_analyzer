************************************************************************************************
* THIS PROGRAM ESTIMATES POTENTIAL YIELD ACROSS ADMINISTRATIVE UNITS AND THE REST OF THE WORLD *
************************************************************************************************

*  PREAMBLE  *
cap which vallist
if _rc ssc install vallist

* Importing potential output and land cultivated for different crops

import delimited "${projdir}\DATA\\RAW\AFRICA\GAEZ DATA\sampleUnitsGaezPotentialOutput.csv", clear bindquote(strict)

levelsof country, local(lcountry)

preserve
	import delimited "${projdir}\DATA\RAW\WORLD\GEE\worldUnitsGaezPotentialOutput.csv", clear bindquote(strict)
	
	gen row = 1
	foreach c of local lcountry {
		replace row = 0 if iso3 == "`c'"
	}
	
	tab row
	keep if row == 1
	
	collapse (first) geotype (sum) areaha *2000*
	gen country = "Rest of the World"
	gen admin_name = "ROW"
	gen ipums_id = 1
	tempfile ROWPotentialOutput
	save `"`ROWPotentialOutput'"', replace
restore

qui append using `"`ROWPotentialOutput'"'

drop *count

rename areaha adminarea

rename (*2000pot*) (*[2]*[1])

gen year = 2000

reshape long area qty, i(ipums_id) j(cropid) string

* adding details about measurement unit, commodity group, conversion factor and price weights associated to each crop and 

preserve
	import excel "${projdir}\DATA\\RAW\AFRICA\GAEZ DATA\Agro-climatic Potential Yield.xlsx", sheet("Agro-climatic Potential Yield") firstrow clear
	rename *, lower
	
	keep cropid crop dataunits commodityid commodities cropgroupgaez faointernationalpricegkt conversionfactor
	
	rename (commodityid commodities faointernationalpricegkt conversionfactor) (commid comm price cf)
	
	la var price "Price in 2000 Int USD/t"
	
	gen pyunit2t = cond(dataunit == "10kg DW/ha", 1/100, 1/1000) 
	la var pyunit2t "conversion factor to metric ton"
	
	keep if ~mi(price) & ~mi(cf)
	tempfile pricecf
	save "`pricecf'", replace
restore

merge m:1 cropid using "`pricecf'", keep(matched) keepusing(commid comm cf price pyunit2t) nogen

gen val = ((qty*pyunit2t)/cf)*price
la var val "Value of potential output in 2000 Int USD"

cap drop cf price pyunit2t

* Now we aggregate potential production by commodity group within each admin unit

collapse (sum) area qty val (first) country admin_name adminarea comm, by(ipums_id commid year)

order ipums_id country admin_name adminarea year commid comm area qty val

gen yld = val/area
la var yld "Yield in 2000 Int USD/ha"

* Importing area of land allocated to production of each commodity within each admin unit

preserve
	
	* Land area allocated to each commodity across the rest of the world
	
	import delimited "${projdir}\DATA\RAW\WORLD\GEE\worldUnitsGaezActualOutput.csv", clear bindquote(strict)
	gen row = 1

	foreach c of local lcountry {
		replace row = 0 if iso3 == "`c'"
	}
	
	tab row
	keep if row == 1

	keep row geotype areaha *har
	
	collapse (first) geotype (sum) areaha *har
	
	gen country = "Rest of the World"
	gen admin_name = "ROW"
	gen ipums_id = 1
	
	tempfile ROWActualOutput
	save `"`ROWActualOutput'"', replace
	
	* Land area allocated to each commodity across administrative units of the countries included in the study
	
	import delimited "${projdir}\DATA\RAW\AFRICA\GAEZ DATA\sampleUnitsGaezActualOutput.csv", clear bindquote(strict)
	
	keep ipums_id country admin_name geotype areaha *har
	
	qui append using `"`ROWActualOutput'"'
	
	rename (areaha *2000* *2010*) (adminarea *[2]2000*[1] *[2]2010*[1])
	
	reshape long har2000 har2010, i(ipums_id) j(commid) string
	
	reshape long har, i(ipums_id commid) j(year)
	
	tempfile actualOutput
	save "`actualOutput'", replace
	
restore

merge 1:1 ipums_id commid year using "`actualOutput'", keep(matched) nogen

* adding details about measurement unit of actual output

preserve
	import excel "${projdir}\DATA\\RAW\AFRICA\GAEZ DATA\Actual Yield and Production.xlsx", sheet("Actual Yield and Production") firstrow clear
	
	rename *, lower
	
	keep  if strpos(lower(variablename), "area") >0
	keep commid crop timeperiod dataunits
	
	rename (timeperiod crop dataunits) (year comm unithar)
	
	replace unithar = "1000" if unithar == "1000 ha"
	
	destring unithar, replace
	
	tempfile unithar
	save "`unithar'", replace
restore

merge m:1 commid year using "`unithar'", keep(matched) nogen

* Area of land allocated to each commodity

replace har = har*unithar
la var har "Land area cultivated (ha)"

cap drop unithar

* Now we aggregate the potential yield across the admin unit area over the actual land area cultivated

gen ypot = yld*har

collapse (sum) har ypot (first) country admin_name adminarea, by(ipums_id year)

gen poty = ypot/har

keep ipums_id country admin_name adminarea year poty har

order country ipums_id admin_name adminarea year

ren adminarea areaha

la var country "Country"
la var admin_name "Admin unit"
la var ipums_id "IPUMS ID"
la var areaha "Area (ha)"
la var year "Year"
la var poty "Potential yield (2000 Int USD/ha)"
la var har "Land area cultivated (ha)"

export delimited using "${projdir}\DATA\BUILD\AFRICA\GAEZ DATA\agpotentialyield.csv", replace

compress
save "${projdir}\DATA\BUILD\AFRICA\GAEZ DATA\agpotentialyield.dta", replace	
