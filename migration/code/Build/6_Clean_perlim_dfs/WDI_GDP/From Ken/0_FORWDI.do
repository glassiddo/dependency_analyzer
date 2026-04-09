******************************************************************************************
* THIS PROGRAM AGGREGATES DATASETS ON INDICATORS OF POVERTY, ECONOMIC ACTIVITIES, LIVING *
*   CONDITIONS AND FOREST COVER DYNAMICS ACROSS COUNTRIES OVER THE LAST THREE DECADES    *
******************************************************************************************

**************
*  PREAMBLE  *
**************

clear
clear matrix
clear mata
set more off
set mem 10000m

if lower(c(username)) == "houngbedji" {
	capture cd "C:\USERS\HOUNGBEDJI\DROPBOX (COMPTE PERSONNEL)\MIGRATION AFRICA\DATA\BUILD\WORLD"
}

global fao  	"..\..\RAW\WORLD\FRA"
global gee  	"..\..\RAW\WORLD\GEE"
global maps 	"..\..\RAW\WORLD\MAPS"
global wdi  	"..\..\RAW\WORLD\WDI"
global work 	"..\WORLD"

** INSTALLING PACKAGES NEEDED

foreach pkg in geo2xy geonear schemepack palettes colrspace {
	cap which `pkg'
	if _rc != 0 ssc install `pkg', replace
}

net install cleanplots, replace from("https://tdmize.github.io/data/cleanplots")
net install palettes, replace from("https://raw.githubusercontent.com/benjann/palettes/master/")
net install colrspace, replace from("https://raw.githubusercontent.com/benjann/colrspace/master/")
set scheme cleanplots, perm

** CUSTOMIZATION

set scheme white_tableau, perm
graph set window fontface "Arial Narrow"

**********************************
* 1: DATASETS ON TREE COVER LOSS *
**********************************

import delimited "${gee}\ALLTC2000.csv", clear

preserve
	import delimited "${gee}\ALLTCLOSS.csv", clear
	save "${work}\ALLTCLOSS.dta", replace
restore

merge 1:1 codeiso using "${work}\ALLTCLOSS.dta", keep(master using matched) nogen

preserve
	import delimited "${gee}\CGABOUNDTCFLOSS.csv", clear
	save "${work}\CGABOUNDTCFLOSS.dta", replace
restore

merge 1:1 codeiso using "${work}\CGABOUNDTCFLOSS.dta", keep(master using matched) nogen

sort codeiso

foreach var of varlist tcloss* {
	local year = substr("`var'",-4,.)
	la var `var' "Tree cover loss in `year' (ha)"
}

foreach var of varlist tcfloss* {
	local year = substr("`var'",-4,.)
	la var `var' "Tree cover loss by fire in `year' (ha)"
}

la var codeiso  "Country ISO code"
la var tc2000  "Tree cover in 2000 (ha)"

compress
save "${work}\ALLTCDATA.dta", replace

keep codeiso 
save "${work}\LISTCOUNTRIES.dta", replace

*********************************************
* 2: DATASETS ON TREE COVER USING MODIS VCF *
*********************************************

import delimited "${gee}\ALLTCVCF.csv", clear

ren codeiso gaul
	
replace gaul = 53    if countryname == "China"  & gaul == 147295
replace gaul = 925   if countryname == "Taiwan" & gaul == 147296
replace gaul = 40764 if countryname == "Sudan"  & gaul == 6

collapse (first) countryname (sum) tcvcf*, by(gaul)

preserve
	import delimited "${wdi}\country-codes_csv.csv", clear 
	keep official_name_en cldrdisplayname gaul iso31661alpha3 regioncode regionname subregioncode subregionname developed
	
	drop if gaul == "91,267"
	destring gaul, replace
	drop if mi(gaul)
	
	ren iso31661alpha3 codeiso
	ren official_name_en countrynameoffic
	ren cldrdisplayname  countrynameshort
	ren developeddevelopingcountries development
	
	save "${work}\COUNTRYCODES.dta", replace
restore

merge 1:1 gaul using "${work}\COUNTRYCODES.dta", keepusing(codeiso countryname* regioncode regionname subregioncode subregionname development) keep(matched) nogen

compress
save "${work}\ALLTCVCF.dta", replace

********************************
* 3: DATA ON LAND USE FROM FAO *
********************************

preserve
	
	* IMPORTING DATA FROM FAOSTAT
	
	import delimited "${fao}\FAOSTATLANDUSE.csv", clear
	
	rename areacodeiso3 countrycode
	rename area countryname
	
	keep if inlist(itemcode, 6600, 6601, 6610, 6620, 6646, 6714, 6716)
	
	keep countrycode countryname item itemcode year value flag flagd
	
	save "${work}\FAOLANDUSE.dta", replace
	
	* NOW WE TRANSFORM THE DATABASE IN A SHAPE FIT FOR REGRESSION ANALYSES
	
	tab itemcode
	
	gen faoitem = .
	levelsof itemcode, local(valitem) 
	local i = 1
	foreach l of local valitem {
		replace faoitem = `i' if itemcode == `l'
		qui levelsof item if itemcode == `l', local(labval)
		la def lfaoitem `i' `labval', modify
		local  nfaoitem`i' = subinstr(lower(`labval'), " ", "", .)
		local  lfaoitem`i' = `labval'
		local ++i
	}
	la var faoitem "FAO variables"
	la val faoitem lfaoitem
	
	rename flag _flag
	
	gen flag = .
	levelsof _flag, local(valflag) 
	local i = 1
	foreach l of local valflag {
		replace flag = `i' if _flag == "`l'"
		qui levelsof flagdescription if _flag == "`l'", local(labflag)
		la def lflag `i' `labflag', modify
		local  lflag`i' = `labflag'
		local ++i
	}
	la var flag "Flag"
	la val flag lflag
	
	sort countrycode countryname faoitem year
	
	keep   countrycode countryname faoitem year value flag
	order  countrycode countryname faoitem year value flag
	
	
	qui tab faoitem 
	local nvar = r(r)
	local faoitems " "
	forvalues v = 1(1)`nvar' {
		local faoitems "`faoitems' value`v'"
	}
	
	keep if flag == 1
	
	reshape wide value, i(countrycode year) j(faoitem)
	
	forvalues v = 1(1)`nvar' {
		la var value`v' "`lfaoitem`v''"
		ren value`v' `nfaoitem`v''
	}
	
	drop flag
	
	gen codeiso = countrycode
	la var codeiso  "Country ISO code"
	
	compress
	order codeiso countryname year
	sort  codeiso countryname year
	save "${work}\FAODATA.dta", replace
restore

********************************************************
* 4: DATA FROM WDI ON GDP PER CAPITA AND POVERTY RATES *
********************************************************

preserve
	
	* IMPORTING DATA FROM WDI
	
	import excel "${wdi}\WDIAGGREGATES.xlsx", sheet("Data") firstrow clear
	
	rename *, lower
	drop in -5/l
	
	keep countrycode countryname seriesname seriescode yr1960-yr2020
	save "${work}\WDICOUNTRIES.dta", replace
	
	* IMPORTING DATA FROM WDI GNI
	
	import excel "${wdi}\WDIGNI.xlsx", sheet("Data") firstrow clear
	
	rename *, lower
	drop in -5/l
	
	keep countrycode countryname seriesname seriescode yr1960-yr2020
	save "${work}\WDIGNI.dta", replace
	
	* IMPORTING DATA OR URBAN AND RURAL DIVIDE FROM WDI
	
	import excel "${wdi}\WDIRURALCOUNTRIES.xlsx", sheet("Data") firstrow clear
	
	rename *, lower
	drop in -5/l
	
	keep countrycode countryname seriesname seriescode yr1960-yr2020
		
	append using "${work}\WDICOUNTRIES.dta"
	append using "${work}\WDIGNI.dta"
	
	* DEALING WITH NAME VARIATIONS OF COUNTRIES
	
	bys countrycode (seriescode): gen countryname_1 = countryname[1]
	bys countrycode (seriescode): gen countryname_2 = countryname[_N]
	tab countryname if countryname_1 != countryname_2
	replace countryname = countryname_1 if countryname_1 != countryname_2
	drop countryname_1 countryname_2
	
	save "${work}\WDICOUNTRIES.dta", replace
	
	* NOW WE TRANSFORM THE DATABASE IN A SHAPE FIT FOR REGRESSION ANALYSES
	
	replace seriescode = subinstr(seriescode,".", "", .)
	tab seriescode
	
	gen wdivar = .
	levelsof seriescode, local(valseries) 
	local i = 1
	foreach l of local valseries {
		replace wdivar = `i' if seriescode == "`l'"
		qui levelsof seriesname if seriescode == "`l'", local(labval)
		la def lwdivar `i' `labval', modify
		local  nwdivar`i' = lower("`l'")
		local  lwdivar`i' = `labval'
		local ++i
	}
	la var wdivar "WDI variables"
	la val wdivar lwdivar
	
	sort countrycode countryname wdivar 
	
	keep   countrycode countryname wdivar yr1960-yr2020
	order  countrycode countryname wdivar yr1960-yr2020
	
	qui tab wdivar 
	local nvar = r(r)
	local wdivars " "
	forvalues v = 1(1)`nvar' {
		local wdivars "`wdivars' wdivar`v'_"
	}
	
	ren yr* yr_*_
	unab allvars: yr_1960_-yr_2020_
	reshape wide `allvars', i(countrycode) j(wdivar)
	
	ren yr_*_* wdivar*[2]_*[1]
	reshape long `wdivars' , i(countrycode) j(year)
	
	forvalues v = 1(1)`nvar' {
		qui replace wdivar`v'_ = "" if wdivar`v'_ == ".."
		qui destring wdivar`v'_, force replace
		la var wdivar`v'_ "`lwdivar`v''"
		ren wdivar`v'_ `nwdivar`v''
	}
	label drop lwdivar
	la var year "Year"
	
	gen codeiso = countrycode
	la var codeiso  "Country ISO code"
	
	compress
	order codeiso countrycode countryname year
	sort  codeiso countrycode countryname year
	save "${work}\WDIVARS.dta", replace
	
	merge m:1 codeiso using "${work}\LISTCOUNTRIES.dta", keep(matched) nogen
	
	compress
	save "${work}\WDIDATA.dta", replace		
restore

****************************************************
* 5: COMBINING THE DATA SETS IN A SINGLE DATA BASE *
****************************************************

use "${work}\ALLTCDATA.dta", clear

merge 1:1 codeiso using "${work}\ALLTCVCF.dta", keep(matched) keepusing(tcvcf* develop) nogen

reshape long tcvcf tcloss tcfloss, i(codeiso) j(year)
la var tcvcf  "Yearly tree cover (ha)"
la var tcloss  "Yearly tree cover loss (ha)"
la var tcfloss "Yearly tree cover loss by fire (ha)"

merge 1:1 codeiso year using "${work}\WDIDATA.dta", keep(using matched) nogen

merge 1:1 codeiso year using "${work}\FAODATA.dta", keep(master matched) keepusing(countryarea landarea agricultural cropland forestland) nogen

rename agriculturalland agriland

sort codeiso year

preserve
	import delimited "${wdi}\ISOCODES.csv", clear
	gen codeiso = alpha3
	compress
	save "${work}\CODEISOREGIONS.dta", replace		
restore

merge m:1 codeiso using "${work}\CODEISOREGIONS.dta", keep(matched) keepusing(region regioncode subregion subregioncode) nogen

rename region regionname
rename subregion subregionname
	
order regioncode regionname subregioncode subregionname codeiso countrycode countryname develop year

gen gdp = nygdppcapkd*sppoptotl
la var gdp "Yearly GDP (Constant 2015 USD)"

// gen farea = aglndfrstk2*100
gen farea = forestland*1000
la var farea "Forest area (ha)"

* forest area is sometime missing for some countries such as US in 2018 and for India in 2020
* In these cases, we replace the missing information with the value imputed in the wdi 

replace farea = aglndfrstk2*100 if inlist(year, 1990, 2000, 2010, 2015, 2016, 2017, 2018, 2019, 2020) & farea == . & (aglndfrstk2!=0 | aglndfrstk2 !=.)

* COUNTRIES WITH TROPICAL RAINFOREST 

global amazon `" "Argentina" "Bolivia" "Brazil" "Colombia" "Ecuador" "Guyana" "Panama" "Paraguay" "Peru" "Suriname" "Venezuela" "'
global congo `" "Cameroon" "Central African Republic" "Congo" "Equatorial Guinea" "Gabon" "'
global australiasia `" "Australia" "New Guinea" "'
global sundaland `" "Brunei" "Indonesia" "Malaysia" "Singapore" "'
global indoburma `" "Bangladesh" "Cambodia" "China" "India" "Lao" "Myanmar" "Thailand" "Vietnam" "'
global mesoamerica `" "Belize" "Costa Rica" "El Salvador" "Guatemala" "Honduras" "Mexico" "Nicaragua" "'
global westafrica `" "Benin" "Ivoire" "Ghana" "Guinea" "Liberia" "Nigeria" "Sierra Leone" "Togo" "'

global rainforest "${amazon} ${congo} ${australiasia} ${sundaland} ${indoburma} ${mesoamerica} ${westafrica}"

gen rainforest = 0
foreach cty of global rainforest {
	qui replace rainforest = 1 if strpos(lower(countryname), lower("`cty'")) >0 
}
la var rainforest "Countries with tropical rainforest"

* SHARE OF LAND AREA WITH TREE COVER 

cap drop stcvcf
gen stcvcf = 100*(tcvcf/(100*aglndtotlk2))
la var stcvcf "Tree cover area (% of land area)"

* MEASURES OF POVERTY PER DECADES

bys codeiso (year): egen _tpop9100 = total(cond(year >=1991 & year <= 2000 & sipovdday !=., sppoptotl, .))
bys codeiso (year): egen _tpop0110 = total(cond(year >=2001 & year <= 2010 & sipovdday !=., sppoptotl, .))
bys codeiso (year): egen _tpop1120 = total(cond(year >=2011 & year <= 2020 & sipovdday !=., sppoptotl, .))

foreach pov in povdday povlmic povumic {
	
	* 1991-2000
	
	cap drop _t`pov'9100
	bys codeiso (year): egen _t`pov'9100 = total(cond(year >=1991 & year <= 2000 & si`pov' !=., (si`pov'/100)*sppoptotl, .))
	
	cap drop m`pov'9100
	gen m`pov'9100 = _t`pov'9100*100/_tpop9100
	la var m`pov'9100 "Average poverty rate at `:word 5 of `:var la si`pov''' a day over 1991-2000 (2011 PPP)"
	
	* 2001-2010
	
	cap drop _t`pov'0110
	bys codeiso (year): egen _t`pov'0110 = total(cond(year >=2001 & year <= 2010 & si`pov' !=., (si`pov'/100)*sppoptotl, .))
	
	cap drop m`pov'0110
	gen m`pov'0110 = _t`pov'0110*100/_tpop0110
	la var m`pov'0110 "Average poverty rate at `:word 5 of `:var la si`pov''' a day over 2001-2010 (2011 PPP)"
	
	* 2011-2020
	
	cap drop _t`pov'1120
	bys codeiso (year): egen _t`pov'1120 = total(cond(year >=2011 & year <= 2020 & si`pov' !=., (si`pov'/100)*sppoptotl, .))
	
	cap drop m`pov'1120
	gen m`pov'1120 = _t`pov'1120*100/_tpop1120
	la var m`pov'1120 "Average poverty rate at `:word 5 of `:var la si`pov''' a day over 2011-2020 (2011 PPP)"
	
	gen m`pov' = .
	replace m`pov' = m`pov'9100 if year >=1991 & year <=2000
	replace m`pov' = m`pov'0110 if year >=2001 & year <=2010
	replace m`pov' = m`pov'1120 if year >=2011 & year <=2020
	la var m`pov' "Average poverty rate at `:word 5 of `:var la si`pov''' a day (2011 PPP)"
	
}

cap drop _t*

* MAIN VARIABLES ACROSS AREA WITH AND WITHOUT RAINFOREST

bys rainforest year: egen _tpoptotlrf = total(cond(sppoptotl !=., sppoptotl, .))

bys rainforest year: egen farearf = total(cond(farea !=., farea*10^(-6), .))
replace farearf = . if farearf == 0
la var farearf "Forest area across countries with rainforest (Million ha)"

bys rainforest year: egen tcvcfrf = total(cond(tcvcf !=., tcvcf*10^(-6), .))
la var tcvcfrf "Tree cover across countries with rainforest (Million ha)"

bys rainforest year: egen tclossrf = total(cond(tcloss !=., tcloss*10^(-6), .))
la var tclossrf "Tree cover loss across countries with rainforest (Million ha)"

bys rainforest year: egen gdprf = total(cond(nygdppcapkd !=., nygdppcapkd*sppoptotl, .))
gen gdppcrf = gdprf/_tpoptotlrf
la var gdppcrf "GDP per capita across countries with rainforest (Constant 2015 USD)"

foreach var in povdday povlmic povumic {
	cap drop _`var'totrf
	bys rainforest year: egen _`var'totrf = total(cond(si`var' !=., (si`var'/100)*sppoptotl, .))
	replace _`var'totrf = . if _`var'totrf == 0
	cap drop m`var'rf
	gen m`var'rf = _`var'totrf*100/_tpoptotlrf
	la var m`var'rf "Average poverty rate at `:word 5 of `:var la si`var''' a day across countries with rainforest (2011 PPP)"
}

cap drop _*rf
sort rainforest year codeiso
egen tagrf = tag(rainforest year)

* MAIN VARIABLES ACROSS CONTINENT WITH AND WITHOUT RAINFOREST

bys regionname year: egen _tpoptotlrfc = total(cond(rainforest == 1 & sppoptotl !=., sppoptotl, .))

bys regionname year: egen farearfc = total(cond(rainforest == 1 & farea !=., farea*10^(-6), .))
replace farearfc = . if farearfc == 0
la var farearfc "Forest area in countries with rainforest (Million ha)"

bys regionname year: egen tcvcfrfc = total(cond(rainforest == 1 & tcvcf !=., tcvcf*10^(-6), .))
la var tcvcfrfc "Tree cover in countries with rainforest (Million ha)"

bys regionname year: egen tclossrfc = total(cond(rainforest == 1 & tcloss !=., tcloss*10^(-6), .))
la var tclossrfc "Tree cover loss in countries with rainforest (Million ha)"

bys regionname year: egen gdprfc = total(cond(rainforest == 1 & nygdppcapkd !=., nygdppcapkd*sppoptotl, .))
gen gdppcrfc = gdprfc/_tpoptotlrfc
la var gdppcrfc "GDP per capita across countries with rainforest (Constant 2015 USD)"

foreach var in povdday povlmic povumic {
	cap drop _`var'totrfc
	bys regionname year: egen _`var'totrfc = total(cond(rainforest == 1 & si`var' !=., (si`var'/100)*sppoptotl, .))
	replace _`var'totrfc = . if _`var'totrfc == 0
	cap drop m`var'rfc
	gen m`var'rfc = _`var'totrfc*100/_tpoptotlrfc
	la var m`var'rfc "Average poverty rate at `:word 5 of `:var la si`var''' a day across countries with rainforest (2011 PPP)"
}

cap drop _*rfc
sort regioncode rainforest year codeiso
egen tagrfc = tag(regioncode rainforest year)

* LOG VALUE OF MAIN VARIABLES 

cap drop llarea
gen llarea = log(aglndtotlk2*100)
la var llarea "Log land area"

cap drop lfarea
gen lfarea = log(farea)
la var lfarea "Log forest area"

cap drop ltcvcf
gen ltcvcf = log(tcvcf)
la var ltcvcf "Log tree cover area"

cap drop ltcloss
gen ltcloss = log(tcloss)
la var ltcloss "Log tree cover loss"

cap drop lgdppc
gen lgdppc = log(nygdppcapkd)
la var lgdppc "Log gdp per capita (Constant 2015 USD)"

gen lfarearf = log(farearf)
la var lfarearf "Forest area across countries with rainforest (Log million ha)"

gen ltcvcfrf = log(tcvcfrf)
la var ltcvcfrf "Tree cover across countries with rainforest (Log million ha)"

gen ltclossrf = log(tclossrf)
la var ltclossrf "Tree cover loss across countries with rainforest (Log million ha)"

gen lgdppcrf = log(gdppcrf)
la var lgdppcrf "GDP per capita across countries with rainforest (constant 2015 USD)"

gen lfarearfc = log(farearfc)
la var lfarearfc "Forest area across countries with rainforest (Log million ha)"

gen ltcvcfrfc = log(tcvcfrfc)
la var ltcvcfrfc "Tree cover across countries with rainforest (Log million ha)"

gen ltclossrfc = log(tclossrfc)
la var ltclossrfc "Tree cover loss across countries with rainforest (Log million ha)"

gen lgdppcrfc = log(gdppcrfc)
la var lgdppcrfc "GDP per capita across countries with rainforest (constant 2015 USD)"

la var year "Year"
la var regionname "Region"

egen cid = group(countrycode)
la var cid "Country id"

isid cid year
xtset cid year

compress
save "${work}\FORWDI.dta", replace

****************************************************************************************************
* 5: TREE-COVER, TREE COVER LOSS, INCOME AND POVERTY IN COUNTRIES WITH TROPICAL RAINFOREST IN 2019 *
****************************************************************************************************

use "${work}\FORWDI.dta", clear

* FOREST AREA AND GDP OVER TIME IN AREA WITH RAINFOREST

#delimit ;
	tw 
		(scatter farearf year if tagrf == 1 & year >= 1990 & rainforest == 1, sort yaxis(1) ms(Oh) connect(l) mc("146 195 51") mlw("thin") lcolor("146 195 51") lpattern("-") lw("thin"))
		(   line gdppcrf year if tagrf == 1 & year >= 1990 & rainforest == 1, sort yaxis(2) clcolor("64 105 166") clpattern("l") clw("thin")),
		xtitle("Years", height(5) size(3)) 
		ytitle("Forest area (Million ha)", axis(1) height(5) size(3))  
		ytitle("GDP per capita (constant 2015 USD)", axis(2) height(5) size(3))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.8) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(1990(2)2020, labsize(*.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2) 
			lab(1 `"Forest area"') 
			lab(2 `"GDP per capita"') 
			keygap(*0.50) 
			row(1) col(2) 
			rowgap(*0.1) 
			size(*.8) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(FAREAGDPPCRF1, replace) ;
#delimit cr

// cap graph save FAREAGDPPCRF1 "${work}\FAREAGDPPCRF1.gph", replace
// cap graph export "${work}\FAREAGDPPCRF1.png", as(png) name("FAREAGDPPCRF1") replace
// cap graph export "${work}\FAREAGDPPCRF1.eps", as(eps) fontface("Palatino") name("FAREAGDPPCRF1") replace
// !epstopdf "${work}\FAREAGDPPCRF1.eps" --outfile "${work}\FAREAGDPPCRF1.pdf"
// cap erase "${work}\FAREAGDPPCRF1.eps"

#delimit ;
	tw 
		(scatter farearf year if tagrf == 1 & year >= 1990 & rainforest == 0 , sort yaxis(1) ms(Oh) connect(l) mc("146 195 51") mlw("thin") lcolor("146 195 51") lpattern("-") lw("thin"))
		(   line gdppcrf year if tagrf == 1 & year >= 1990 & rainforest == 0 , sort yaxis(2) clcolor("64 105 166") clpattern("l") clw("thin")),
		xtitle("Years", height(5) size(3)) 
		ytitle("Forest area (Million ha)", axis(1) height(5) size(3))  
		ytitle("GDP per capita (constant 2015 USD)", axis(2) height(5) size(3))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.8) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(1990(2)2020, labsize(*.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2) 
			lab(1 `"Forest area"') 
			lab(2 `"GDP per capita"') 
			keygap(*0.50) 
			row(1) col(2) 
			rowgap(*0.1) 
			size(*.8) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(FAREAGDPPCRF0, replace) ;
#delimit cr


#delimit ;
	tw 
		(scatter farearf year if tagrf == 1 & year >= 1990 & rainforest == 1, sort yaxis(1) ms(Oh) connect(l) mc("146 195 51") mlw("thin") lcolor("146 195 51") lpattern("-") lw("thin"))
		(scatter farearf year if tagrf == 1 & year >= 1990 & rainforest == 0, sort yaxis(1) ms(Oh) connect(l) mc("146 195 51") mlw("thin") lcolor("146 195 51") lpattern("_") lw("thin"))
		(   line gdppcrf year if tagrf == 1 & year >= 1990 & rainforest == 1, sort yaxis(2) clcolor("64 105 166") clpattern("l") clw("thin"))
		(   line gdppcrf year if tagrf == 1 & year >= 1990 & rainforest == 0, sort yaxis(2) clcolor("64 105 166") clpattern("_-_") clw("thin")),
		xtitle("Years", height(5) size(3)) 
		ytitle("Forest area (Million ha)", axis(1) height(5) size(3))  
		ytitle("GDP per capita (constant 2015 USD)", axis(2) height(5) size(3))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.8) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(1990(2)2020, labsize(*.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2 3 4) 
			lab(1 `"Forest area - Rainforest region"') 
			lab(2 `"Forest area - Outside rainforest"') 
			lab(3 `"GDP per capita - Rainforest region"') 
			lab(4 `"GDP per capita - Outside rainforest"') 
			keygap(*0.50) 
			row(2) col(2) 
			rowgap(*0.1) 
			size(*.8) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(FAREAGDPPC, replace) ;
#delimit cr

* TREE COVER AND GDP OVER TIME

#delimit ;
	tw 
		(line tcvcfrf year if tagrf == 1 & year >= 2001 & rainforest == 1, sort yaxis(1) clcolor("146 195 51") clpattern("l") clw("thin")) 
		(line gdppcrf year if tagrf == 1 & year >= 2001 & rainforest == 1, sort yaxis(2) clcolor("64 105 166") clpattern("l") clw("thin")),
		xtitle("Years", height(5) size(2)) 
		ytitle("Area with tree cover (Million ha)", axis(1) height(5) size(2))  
		ytitle("GDP per capita (constant 2015 USD)", axis(2) height(5) size(2))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(2001(1)2020, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2) 
			lab(1 `"Tree cover"') 
			lab(2 `"GDP per capita"') 
			keygap(*0.50) 
			row(1) col(2) 
			rowgap(*0.1) 
			size(*.5) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(TCVCFGDPPCRF1, replace) ;
#delimit cr

#delimit ;
	tw 
		(line tcvcfrf year if tagrf == 1 & year >= 2001 & rainforest == 0, sort yaxis(1) clcolor("146 195 51") clpattern("l") clw("thin")) 
		(line gdppcrf year if tagrf == 1 & year >= 2001 & rainforest == 0, sort yaxis(2) clcolor("64 105 166") clpattern("l") clw("thin")),
		xtitle("Years", height(5) size(2)) 
		ytitle("Area with tree cover (Million ha)", axis(1) height(5) size(2))  
		ytitle("GDP per capita (constant 2015 USD)", axis(2) height(5) size(2))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(2001(1)2020, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2) 
			lab(1 `"Tree cover"') 
			lab(2 `"GDP per capita"') 
			keygap(*0.50) 
			row(1) col(2) 
			rowgap(*0.1) 
			size(*.5) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(TCVCFGDPPCRF0, replace) ;
#delimit cr

* TREE COVER LOSS AND GDP OVER TIME

#delimit ;
	tw 
		(line tclossrf year if tagrf == 1 & year >= 2001 & year <= 2010 & rainforest == 1, sort yaxis(1) clcolor("146 195 51") clpattern("l") clw("thin")) 
		(line  gdppcrf year if tagrf == 1 & year >= 2001 & year <= 2010 & rainforest == 1, sort yaxis(2) clcolor("64 105 166") clpattern("l") clw("thin")),
		xtitle("Years", height(5) size(2)) 
		ytitle("Tree cover loss (Million ha)", axis(1) height(5) size(2))  
		ytitle("GDP per capita (constant 2015 USD)", axis(2) height(5) size(2))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(2001(1)2010, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2) 
			lab(1 `"Tree cover loss"') 
			lab(2 `"GDP per capita"') 
			keygap(*0.50) 
			row(1) col(2) 
			rowgap(*0.1) 
			size(*.5) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(TCLOSSGDPPCRF1A, replace) ;
#delimit cr

#delimit ;
	tw 
		(line tclossrf year if tagrf == 1 & year >= 2011 & year <= 2020 & rainforest == 1, sort yaxis(1) clcolor("146 195 51") clpattern("l") clw("thin")) 
		(line  gdppcrf year if tagrf == 1 & year >= 2011 & year <= 2020 & rainforest == 1, sort yaxis(2) clcolor("64 105 166") clpattern("l") clw("thin")),
		xtitle("Years", height(5) size(2)) 
		ytitle("Tree cover loss (Million ha)", axis(1) height(5) size(2))  
		ytitle("GDP per capita (constant 2015 USD)", axis(2) height(5) size(2))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(2011(1)2020, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2) 
			lab(1 `"Tree cover loss"') 
			lab(2 `"GDP per capita"') 
			keygap(*0.50) 
			row(1) col(2) 
			rowgap(*0.1) 
			size(*.5) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(TCLOSSGDPPCRF1B, replace) ;
#delimit cr

#delimit ;
	tw 
		(line tclossrf year if tagrf == 1 & year >= 2001 & year <= 2010 & rainforest == 0, sort yaxis(1) clcolor("146 195 51") clpattern("l") clw("thin")) 
		(line  gdppcrf year if tagrf == 1 & year >= 2001 & year <= 2010 & rainforest == 0, sort yaxis(2) clcolor("64 105 166") clpattern("l") clw("thin")),
		xtitle("Years", height(5) size(2)) 
		ytitle("Tree cover loss (Million ha)", axis(1) height(5) size(2))  
		ytitle("GDP per capita (constant 2015 USD)", axis(2) height(5) size(2))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(2001(1)2010, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2) 
			lab(1 `"Tree cover loss"') 
			lab(2 `"GDP per capita"') 
			keygap(*0.50) 
			row(1) col(2) 
			rowgap(*0.1) 
			size(*.5) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(TCLOSSGDPPCRF0A, replace) ;
#delimit cr

#delimit ;
	tw 
		(line tclossrf year if tagrf == 1 & year >= 2011 & year <= 2020 & rainforest == 0, sort yaxis(1) clcolor("146 195 51") clpattern("l") clw("thin")) 
		(line  gdppcrf year if tagrf == 1 & year >= 2011 & year <= 2020 & rainforest == 0, sort yaxis(2) clcolor("64 105 166") clpattern("l") clw("thin")),
		xtitle("Years", height(5) size(2)) 
		ytitle("Tree cover loss (Million ha)", axis(1) height(5) size(2))  
		ytitle("GDP per capita (constant 2015 USD)", axis(2) height(5) size(2))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(2011(1)2020, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2) 
			lab(1 `"Tree cover loss"') 
			lab(2 `"GDP per capita"') 
			keygap(*0.50) 
			row(1) col(2) 
			rowgap(*0.1) 
			size(*.5) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(TCLOSSGDPPCRF0B, replace) ;
#delimit cr

* FOREST AREA AND GDP OVER TIME IN COUNTRIES WITH RAINFOREST BY CONTINENT

foreach reg in africa america asia oceania {
	local Reg = proper("`reg'")
	local abv = substr(upper("`reg'"), 1,3)
	#delimit ;
		tw 
			(scatter farearfc year if tagrfc == 1 & year >= 1990 & year <= 2019 & rainforest == 1 & strpos(lower(regionname), "`reg'") > 0, sort yaxis(1) ms(Oh) connect(l) mc("146 195 51") mlw("thin") lcolor("146 195 51") lpattern("-") lw("thin"))
			(   line gdppcrfc year if tagrfc == 1 & year >= 1990 & year <= 2019 & rainforest == 1 & strpos(lower(regionname), "`reg'") > 0, sort yaxis(2) clcolor("64 105 166") clpattern("l") clw("thin"))
			,
			xtitle("Years", height(5) size(2)) 
			ytitle("Forest area in rainforest countries across `Reg'" "(Million ha)", axis(1) height(5) size(2))  
			ytitle("GDP per capita in rainforest countries across `Reg'" "(constant 2015 USD)", axis(2) height(5) size(2))  
			graphregion(fcolor(none) lcolor(none) style(none)) 
			plotregion(lcolor(none) ilcolor(none)) 
			ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
			ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
			xlabel(1990(2)2019, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
			legend(
				title(" ", height(2) size(*.5)) 
				order(1 2) 
				lab(1 `"Forest area"') 
				lab(2 `"GDP per capita"') 
				keygap(*0.50) 
				row(1) col(2) 
				rowgap(*0.1) 
				size(*.5) 
				ring(1) 
				position(6)
				region(lstyle(none) fcolor(none)) 
				margin(zero) 
				bmargin(zero)
			)
			scheme(s1mono)
			caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
			name(FAREAGDPPCRF`abv'1, replace) ;
	#delimit cr
}

* TREE COVER AREA AND GDP OVER TIME IN COUNTRIES WITH RAINFOREST BY CONTINENT

foreach reg in africa america asia oceania {
	local Reg = proper("`reg'")
	local abv = substr(upper("`reg'"), 1,3)
	#delimit ;
		tw 
			(line tcvcfrfc year if tagrfc == 1 & year >= 2001 & rainforest == 1 & strpos(lower(regionname), "`reg'") > 0, sort yaxis(1) clcolor("146 195 51") clpattern("l") clw("thin")) 
			(line gdppcrfc year if tagrfc == 1 & year >= 2001 & rainforest == 1 & strpos(lower(regionname), "`reg'") > 0, sort yaxis(2) clcolor("64 105 166") clpattern("l") clw("thin"))
			,
			xtitle("Years", height(5) size(2)) 
			ytitle("Tree cover in rainforest countries across `Reg'" "(Million ha)", axis(1) height(5) size(2))  
			ytitle("GDP per capita in rainforest countries across `Reg'" "(constant 2015 USD)", axis(2) height(5) size(2))  
			graphregion(fcolor(none) lcolor(none) style(none)) 
			plotregion(lcolor(none) ilcolor(none)) 
			ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
			ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
			xlabel(2001(1)2020, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
			legend(
				title(" ", height(2) size(*.5)) 
				order(1 2) 
				lab(1 `"Tree cover"') 
				lab(2 `"GDP per capita"') 
				keygap(*0.50) 
				row(1) col(2) 
				rowgap(*0.1) 
				size(*.5) 
				ring(1) 
				position(6)
				region(lstyle(none) fcolor(none)) 
				margin(zero) 
				bmargin(zero)
			)
			scheme(s1mono)
			caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
			name(TCVCFGDPPCRF`abv'1, replace) ;
	#delimit cr
}

* VARIATION OF FOREST AREA AND GDP BETWEEN 1990 1ND 2020 ACROSS COUNTRIES WITH RAINFOREST

preserve
	cap drop lfarea*0
	cap drop lgdppc*0
	bys countryname (year): egen lfarea1990 = total(lfarea*(year == 1990))
	bys countryname (year): egen lfarea2020 = total(lfarea*(year == 2020))
	bys countryname (year): egen lgdppc1990 = total(lgdppc*(year == 1990))
	bys countryname (year): egen lgdppc2020 = total(lgdppc*(year == 2020))
	
	keep if year == 1990
	keep if rainforest == 1
	drop if lgdppc1990 == 0 | lgdppc2020 == 0 | lfarea1990 == 0 | lfarea2020 == 0
	sort countryname
	gen cty = _n
	count
	local obs = r(N)
	forvalues n = 1(1)`obs' {
		 label def lcty  `n' "`=countryname[`n']'", modify
	}
	label val cty lctygdp
	
	gen lfarea9020 = lfarea2020 - lfarea1990
	gen lgdppc9020 = lgdppc2020 - lgdppc1990
	
	gen poslfg = 3
	replace poslfg = 12 if strpos(lower(countryname),  "australia") > 0
	replace poslfg = 12 if strpos(lower(countryname),  "gabon") > 0
	replace poslfg = 10 if strpos(lower(countryname),  "costa rica") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "congo, rep") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "brunei") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "cameroon") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "sierra") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "bissau") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "ecuador") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "mexico") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "thailand") > 0
	replace poslfg = 6  if strpos(lower(countryname),  "belize") > 0
	replace poslfg = 6  if strpos(lower(countryname),  "argentina") > 0
	replace poslfg = 5  if strpos(lower(countryname),  "nigeria") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "panama") > 0
	replace poslfg = 4  if strpos(lower(countryname),  "bangladesh") > 0
	replace poslfg = 3  if strpos(lower(countryname),  "togo") > 0
	replace poslfg = 2  if strpos(lower(countryname),  "salvador") > 0
	
	reg lfarea9020 c.lgdppc9020##c.lgdppc9020##c.lgdppc9020 
	cap drop _lfarea9020
	predict _lfarea9020, xb
	
	#delimit ;
		tw 
			(scatter lfarea9020 lgdppc9020 if strpos(lower(regionname), "africa") > 0 , sort ms(o) mc("146 195 51") mlabc(black) mlabel(codeiso) mlabvp(poslfg) mlabs(*.5) mlabg(0.1))
			(scatter lfarea9020 lgdppc9020 if strpos(lower(regionname), "america") > 0, sort ms(o) mc("64 105 166") mlabc(black) mlabel(codeiso) mlabvp(poslfg) mlabs(*.5) mlabg(0.1))
			(scatter lfarea9020 lgdppc9020 if strpos(lower(regionname), "asia") > 0   , sort ms(o) mc("200 0 0") mlabc(black) mlabel(codeiso) mlabvp(poslfg) mlabs(*.5) mlabg(0.1))
			(lowess lfarea9020 lgdppc9020, sort lcolor(maroon) lwidth(thin) lpattern("l") bwidth(.8)),
			yline(0, lcolor(black*0.5) lp("-"))
			ytitle(" ", axis(1) height(5) size(2))  
			xline(0, lcolor(black*0.5) lp("-"))
			ytitle("Log variation of forest area between 1990 and 2020", axis(1) height(3) size(3))  
			xtitle("Log variation of GDP per capita between 1990 and 2020 (constant 2015 USD)", axis(1) height(3) size(3))  
			graphregion(fcolor(none) lcolor(none) style(none)) 
			plotregion(lcolor(none) ilcolor(none)) 
			ylabel(, labsize(*0.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
			xlabel(-0.5(0.5)3, labsize(*0.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
			legend(
				title(" ", height(2) size(*.5)) 
				order(1 2 3 4) 
				lab(1 `"Africa"') 
				lab(2 `"South-America"') 
				lab(3 `"Asia"') 
				lab(4 `"Smooth plot"') 
				keygap(*0.50) 
				row(1) col(4) 
				rowgap(*2) 
				size(*.8) 
				ring(1) 
				position(6)
				region(lstyle(none) fcolor(none)) 
				margin(zero) 
				bmargin(zero)
			)
			scheme(s1mono)
			caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
			name(FAREAGDPPC9020P1, replace) ;
	#delimit cr
restore

cap graph save FAREAGDPPC9020P1 "${work}\FAREAGDPPC9020P1.gph", replace
cap graph export "${work}\FAREAGDPPC9020P1.png", as(png) name("FAREAGDPPC9020P1") replace
cap graph export "${work}\FAREAGDPPC9020P1.eps", as(eps) fontface("Palatino") name("FAREAGDPPC9020P1") replace
!epstopdf "${work}\FAREAGDPPC9020P1.eps" --outfile "${work}\FAREAGDPPC9020P1.pdf"
cap erase "${work}\FAREAGDPPC9020P1.eps"

* VARIATION OF FOREST AREA AND GDP BETWEEN 1990 1ND 2020 ACROSS COUNTRIES WITH RAINFOREST (BY LOW- MIDDLE- OR HIGH-INCOME STATUS IN 1990)

preserve
	cap drop lfarea*0
	cap drop lgdppc*0
	bys countryname (year): egen lfarea1990 = total(lfarea*(year == 1990))
	bys countryname (year): egen lfarea2020 = total(lfarea*(year == 2020))
	bys countryname (year): egen lgdppc1990 = total(lgdppc*(year == 1990))
	bys countryname (year): egen lgdppc2020 = total(lgdppc*(year == 2020))
	
	cap drop gnipc*0
	bys countryname (year): egen gnipc1990 = max(cond(year==1990, nygnppcapcd, .))
	bys countryname (year): egen gnipc2000 = max(cond(year==2000, nygnppcapcd, .))
	bys countryname (year): egen gnipc2020 = max(cond(year==2020, nygnppcapcd, .))
		
	keep if year == 1990
	keep if rainforest == 1
	drop if lgdppc1990 == 0 | lgdppc2020 == 0 | lfarea1990 == 0 | lfarea2020 == 0
	sort countryname
	gen cty = _n
	count
	local obs = r(N)
	forvalues n = 1(1)`obs' {
		 label def lcty  `n' "`=countryname[`n']'", modify
	}
	label val cty lctygdp
	
	gen lfarea9020 = lfarea2020 - lfarea1990
	gen lgdppc9020 = lgdppc2020 - lgdppc1990
	
	gen poslfg = 3
	replace poslfg = 12 if strpos(lower(countryname),  "australia") > 0
	replace poslfg = 12 if strpos(lower(countryname),  "gabon") > 0
	replace poslfg = 10 if strpos(lower(countryname),  "costa rica") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "congo, rep") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "brunei") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "cameroon") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "sierra") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "bissau") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "ecuador") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "mexico") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "thailand") > 0
	replace poslfg = 6  if strpos(lower(countryname),  "belize") > 0
	replace poslfg = 6  if strpos(lower(countryname),  "argentina") > 0
	replace poslfg = 5  if strpos(lower(countryname),  "nigeria") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "panama") > 0
	replace poslfg = 4  if strpos(lower(countryname),  "bangladesh") > 0
	replace poslfg = 3  if strpos(lower(countryname),  "togo") > 0
	replace poslfg = 2  if strpos(lower(countryname),  "salvador") > 0
	
	reg lfarea9020 c.lgdppc9020##c.lgdppc9020##c.lgdppc9020 
	cap drop _lfarea9020
	predict _lfarea9020, xb
	
	#delimit ;
		tw 
			(scatter lfarea9020 lgdppc9020 if  gnipc1990 > 0 & gnipc1990 <= 1085, sort ms(o) mc("146 195 51") mlabc(black) mlabel(codeiso) mlabvp(poslfg) mlabs(*.5) mlabg(0.1))
			(scatter lfarea9020 lgdppc9020 if  gnipc1990 > 1085 & gnipc1990 <= 13205, sort ms(o) mc("64 105 166") mlabc(black) mlabel(codeiso) mlabvp(poslfg) mlabs(*.5) mlabg(0.1))
			(scatter lfarea9020 lgdppc9020 if  gnipc1990 > 13205, sort ms(o) mc("200 0 0") mlabc(black) mlabel(codeiso) mlabvp(poslfg) mlabs(*.5) mlabg(0.1))
			(lowess lfarea9020 lgdppc9020, sort lcolor(maroon) lwidth(thin) lpattern("l") bwidth(.8)),
			yline(0, lcolor(black*0.5) lp("-"))
			ytitle(" ", axis(1) height(5) size(2))  
			xline(0, lcolor(black*0.5) lp("-"))
			ytitle("Log variation of forest area between 1990 and 2020", axis(1) height(3) size(3))  
			xtitle("Log variation of GDP per capita between 1990 and 2020 (constant 2015 USD)", axis(1) height(3) size(3))  
			graphregion(fcolor(none) lcolor(none) style(none)) 
			plotregion(lcolor(none) ilcolor(none)) 
			ylabel(, labsize(*0.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
			xlabel(-0.5(0.5)3, labsize(*0.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
			legend(
				title(" ", height(2) size(*.5)) 
				order(1 2 3 4) 
				lab(1 `"Low-income"') 
				lab(2 `"Middle-income"') 
				lab(3 `"High-income"') 
				lab(4 `"EKC"') 
				keygap(*0.50) 
				row(1) col(4) 
				rowgap(*2) 
				size(*.8) 
				ring(1) 
				position(6)
				region(lstyle(none) fcolor(none)) 
				margin(zero) 
				bmargin(zero)
			)
			scheme(s1mono)
			caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
			name(FAREAGDPPC9020P1ALT, replace) ;
	#delimit cr
restore

cap graph save FAREAGDPPC9020P1ALT "${work}\FAREAGDPPC9020P1ALT.gph", replace
cap graph export "${work}\FAREAGDPPC9020P1ALT.png", as(png) name("FAREAGDPPC9020P1ALT") replace
cap graph export "${work}\FAREAGDPPC9020P1ALT.eps", as(eps) fontface("Palatino") name("FAREAGDPPC9020P1ALT") replace
!epstopdf "${work}\FAREAGDPPC9020P1ALT.eps" --outfile "${work}\FAREAGDPPC9020P1ALT.pdf"
cap erase "${work}\FAREAGDPPC9020P1ALT.eps"

* VARIATION OF FOREST AREA AND GDP BETWEEN 1990 AND 2020 ACROSS COUNTRIES WITHOUT RAINFOREST

preserve
	cap drop lfarea*0
	cap drop lgdppc*0
	bys countryname (year): egen lfarea1990 = total(lfarea*(year == 1990))
	bys countryname (year): egen lfarea2020 = total(lfarea*(year == 2020))
	bys countryname (year): egen lgdppc1990 = total(lgdppc*(year == 1990))
	bys countryname (year): egen lgdppc2020 = total(lgdppc*(year == 2020))
	
	keep if year == 1990
	keep if rainforest == 0
	drop if lgdppc1990 == 0 | lgdppc2020 == 0 | lfarea1990 == 0 | lfarea2020 == 0
	sort countryname
	gen cty = _n
	count
	local obs = r(N)
	forvalues n = 1(1)`obs' {
		 label def lcty  `n' "`=countryname[`n']'", modify
	}
	label val cty lctygdp
	
	gen lfarea9020 = lfarea2020 - lfarea1990
	gen lgdppc9020 = lgdppc2020 - lgdppc1990
	
	gen poslfg = 3
	
	#delimit ;
		tw 
			(scatter lfarea9020 lgdppc9020 if strpos(lower(regionname), "africa") > 0 , sort ms(o) mc("146 195 51") mlabc(black) mlabel(countryname) mlabvp(poslfg) mlabs(*.3) mlabg(0.1))
			(scatter lfarea9020 lgdppc9020 if strpos(lower(regionname), "america") > 0, sort ms(o) mc("64 105 166") mlabc(black) mlabel(countryname) mlabvp(poslfg) mlabs(*.3) mlabg(0.1))
			(scatter lfarea9020 lgdppc9020 if strpos(lower(regionname), "asia") > 0   , sort ms(o) mc("200 0 0") mlabc(black) mlabel(countryname) mlabvp(poslfg) mlabs(*.3) mlabg(0.1))
			(scatter lfarea9020 lgdppc9020 if strpos(lower(regionname), "europe") > 0,  sort ms(o) mc("127 34 147") mlabc(black) mlabel(countryname) mlabvp(poslfg) mlabs(*.3) mlabg(0.1))
			(scatter lfarea9020 lgdppc9020 if strpos(lower(regionname), "oceania") > 0, sort ms(o) mc("251 222 6") mlabc(black) mlabel(countryname) mlabvp(poslfg) mlabs(*.3) mlabg(0.1)),
			yline(0, lcolor(black*0.5) lp("-"))
			ytitle(" ", axis(1) height(5) size(2))  
			xline(0, lcolor(black*0.5) lp("-"))
			ytitle("Log variation of forest area between 1990 and 2020", axis(1) height(3) size(2))  
			xtitle("Log variation of GDP per capita between 1990 and 2020 (constant 2015 USD)", axis(1) height(3) size(2))  
			graphregion(fcolor(none) lcolor(none) style(none)) 
			plotregion(lcolor(none) ilcolor(none)) 
			ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
			xlabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
			legend(
				title(" ", height(2) size(*.5)) 
				order(1 2 3 4 5) 
				lab(1 `"Africa"') 
				lab(2 `"South-America"') 
				lab(3 `"Asia"') 
				lab(4 `"Europe"') 
				lab(5 `"Oceania"') 
				keygap(*0.50) 
				row(1) col(4) 
				rowgap(*2) 
				size(*.5) 
				ring(1) 
				position(6)
				region(lstyle(none) fcolor(none)) 
				margin(zero) 
				bmargin(zero)
			)
			scheme(s1mono)
			caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
			name(FAREAGDPPC9020P0, replace) ;
	#delimit cr
restore

cap graph save FAREAGDPPC9020P0 "${work}\FAREAGDPPC9020P0.gph", replace
cap graph export "${work}\FAREAGDPPC9020P0.png", as(png) name("FAREAGDPPC9020P0") replace
cap graph export "${work}\FAREAGDPPC9020P0.eps", as(eps) fontface("Palatino") name("FAREAGDPPC9020P0") replace
!epstopdf "${work}\FAREAGDPPC9020P0.eps" --outfile "${work}\FAREAGDPPC9020P0.pdf"
cap erase "${work}\FAREAGDPPC9020P0.eps"

* VARIATION OF FOREST AREA AND GDP BETWEEN 1990 1ND 2020 ACROSS COUNTRIES WITHOUT RAINFOREST (BY LOW- MIDDLE- OR HIGH-INCOME STATUS IN 1990)

preserve
	cap drop lfarea*0
	cap drop lgdppc*0
	bys countryname (year): egen lfarea1990 = total(lfarea*(year == 1990))
	bys countryname (year): egen lfarea2020 = total(lfarea*(year == 2020))
	bys countryname (year): egen lgdppc1990 = total(lgdppc*(year == 1990))
	bys countryname (year): egen lgdppc2020 = total(lgdppc*(year == 2020))
	
	cap drop gnipc*0
	bys countryname (year): egen gnipc1990 = max(cond(year==1990, nygnppcapcd, .))
	bys countryname (year): egen gnipc2000 = max(cond(year==2000, nygnppcapcd, .))
	bys countryname (year): egen gnipc2020 = max(cond(year==2020, nygnppcapcd, .))
		
	keep if year == 1990
	keep if rainforest == 0
	drop if lgdppc1990 == 0 | lgdppc2020 == 0 | lfarea1990 == 0 | lfarea2020 == 0
	sort countryname
	gen cty = _n
	count
	local obs = r(N)
	forvalues n = 1(1)`obs' {
		 label def lcty  `n' "`=countryname[`n']'", modify
	}
	label val cty lctygdp
	
	gen lfarea9020 = lfarea2020 - lfarea1990
	gen lgdppc9020 = lgdppc2020 - lgdppc1990
	
	gen poslfg = 3
	
	reg lfarea9020 c.lgdppc9020##c.lgdppc9020##c.lgdppc9020 
	cap drop _lfarea9020
	predict _lfarea9020, xb
	
	#delimit ;
		tw 
			(scatter lfarea9020 lgdppc9020 if  gnipc1990 > 0 & gnipc1990 <= 1085, sort ms(o) mc("146 195 51") mlabc(black) mlabel(codeiso) mlabvp(poslfg) mlabs(*.5) mlabg(0.1))
			(scatter lfarea9020 lgdppc9020 if  gnipc1990 > 1085 & gnipc1990 <= 13205, sort ms(o) mc("64 105 166") mlabc(black) mlabel(codeiso) mlabvp(poslfg) mlabs(*.5) mlabg(0.1))
			(scatter lfarea9020 lgdppc9020 if  gnipc1990 > 13205, sort ms(o) mc("200 0 0") mlabc(black) mlabel(codeiso) mlabvp(poslfg) mlabs(*.5) mlabg(0.1))
			(lowess lfarea9020 lgdppc9020, sort lcolor(maroon) lwidth(thin) lpattern("l") bwidth(.8)),
			yline(0, lcolor(black*0.5) lp("-"))
			ytitle(" ", axis(1) height(5) size(2))  
			xline(0, lcolor(black*0.5) lp("-"))
			ytitle("Log variation of forest area between 1990 and 2020", axis(1) height(3) size(3))  
			xtitle("Log variation of GDP per capita between 1990 and 2020 (constant 2015 USD)", axis(1) height(3) size(3))  
			graphregion(fcolor(none) lcolor(none) style(none)) 
			plotregion(lcolor(none) ilcolor(none)) 
			ylabel(, labsize(*0.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
			xlabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
			legend(
				title(" ", height(2) size(*.5)) 
				order(1 2 3 4) 
				lab(1 `"Low-income"') 
				lab(2 `"Middle-income"') 
				lab(3 `"High-income"') 
				lab(4 `"EKC"') 
				keygap(*0.50) 
				row(1) col(4) 
				rowgap(*2) 
				size(*.8) 
				ring(1) 
				position(6)
				region(lstyle(none) fcolor(none)) 
				margin(zero) 
				bmargin(zero)
			)
			scheme(s1mono)
			caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
			name(FAREAGDPPC9020P0ALT, replace) ;
	#delimit cr
restore

cap graph save FAREAGDPPC9020P0ALT "${work}\FAREAGDPPC9020P0ALT.gph", replace
cap graph export "${work}\FAREAGDPPC9020P0ALT.png", as(png) name("FAREAGDPPC9020P0ALT") replace
cap graph export "${work}\FAREAGDPPC9020P0ALT.eps", as(eps) fontface("Palatino") name("FAREAGDPPC9020P0ALT") replace
!epstopdf "${work}\FAREAGDPPC9020P0ALT.eps" --outfile "${work}\FAREAGDPPC9020P0ALT.pdf"
cap erase "${work}\FAREAGDPPC9020P0ALT.eps"

*FOREST AREA AND GDP ACROSS COUNTRIES WITH RAIN FOREST BETWEEN 2000 AND 2020

preserve
	cap drop lfarea*0
	cap drop lgdppc*0
	bys countryname (year): egen lfarea2000 = total(lfarea*(year == 2000))
	bys countryname (year): egen lfarea2020 = total(lfarea*(year == 2020))
	bys countryname (year): egen lgdppc2000 = total(lgdppc*(year == 2000))
	bys countryname (year): egen lgdppc2020 = total(lgdppc*(year == 2020))
	
	keep if year == 2000
	keep if rainforest == 1
	drop if lgdppc2000 == 0 | lgdppc2020 == 0 | lfarea2000 == 0 | lfarea2020 == 0
	sort countryname
	gen cty = _n
	count
	local obs = r(N)
	forvalues n = 1(1)`obs' {
		 label def lcty  `n' "`=countryname[`n']'", modify
	}
	label val cty lctygdp
	
	gen lfarea0020 = lfarea2020 - lfarea2000
	gen lgdppc0020 = lgdppc2020 - lgdppc2000
	
	gen poslfg = 3
	replace poslfg = 12  if strpos(lower(countryname),  "colombia") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "brazil") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "congo, dem") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "congo, rep") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "brunei") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "ghana") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "honduras") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "malaysia") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "sierra") > 0
	
	#delimit ;
		tw 
			(scatter lfarea0020 lgdppc0020 if strpos(lower(regionname), "africa") > 0 , sort ms(o) mc("146 195 51") mlabc(black) mlabel(countryname) mlabvp(poslfg) mlabs(*.3) mlabg(0.1))
			(scatter lfarea0020 lgdppc0020 if strpos(lower(regionname), "america") > 0, sort ms(o) mc("64 105 166") mlabc(black) mlabel(countryname) mlabvp(poslfg) mlabs(*.3) mlabg(0.1))
			(scatter lfarea0020 lgdppc0020 if strpos(lower(regionname), "asia") > 0   , sort ms(o) mc("200 0 0") mlabc(black) mlabel(countryname) mlabvp(poslfg) mlabs(*.3) mlabg(0.1))
			(scatter lfarea0020 lgdppc0020 if strpos(lower(regionname), "oceania") > 0, sort ms(o) mc("251 222 6") mlabc(black) mlabel(countryname) mlabvp(poslfg) mlabs(*.3) mlabg(0.1)),
			yline(0, lcolor(black*0.5) lp("-"))
			ytitle(" ", axis(1) height(5) size(2))  
			xline(0, lcolor(black*0.5) lp("-"))
			ytitle("Log variation of forest area between 2000 and 2020", axis(1) height(3) size(3))  
			xtitle("Log variation of GDP per capita between 2000 and 2020 (constant 2015 USD)", axis(1) height(3) size(3))  
			graphregion(fcolor(none) lcolor(none) style(none)) 
			plotregion(lcolor(none) ilcolor(none)) 
			ylabel(, labsize(*0.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
			xlabel(, labsize(*0.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
			legend(
				title(" ", height(2) size(*.5)) 
				order(1 2 3 4) 
				lab(1 `"Africa"') 
				lab(2 `"South-America"') 
				lab(3 `"Asia"') 
				lab(4 `"Oceania"') 
				keygap(*0.50) 
				row(1) col(4) 
				rowgap(*2) 
				size(*.8) 
				ring(1) 
				position(6)
				region(lstyle(none) fcolor(none)) 
				margin(zero) 
				bmargin(zero)
			)
			scheme(s1mono)
			caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
			name(FAREAGDPPC0020P1, replace) ;
	#delimit cr
restore

cap graph save FAREAGDPPC0020P1 "${work}\FAREAGDPPC0020P1.gph", replace
cap graph export "${work}\FAREAGDPPC0020P1.png", as(png) name("FAREAGDPPC0020P1") replace
cap graph export "${work}\FAREAGDPPC0020P1.eps", as(eps) fontface("Palatino") name("FAREAGDPPC0020P1") replace
!epstopdf "${work}\FAREAGDPPC0020P1.eps" --outfile "${work}\FAREAGDPPC0020P1.pdf"
cap erase "${work}\FAREAGDPPC0020P1.eps"


*TREE COVER AND GDP ACROSS COUNTRIES WITH RAIN FOREST BETWEEN 2001 AND 2020

preserve
	cap drop ltcvcf*0
	cap drop lgdppc*0
	bys countryname (year): egen ltcvcf2001 = total(ltcvcf*(year == 2001))
	bys countryname (year): egen ltcvcf2020 = total(ltcvcf*(year == 2020))
	bys countryname (year): egen lgdppc2001 = total(lgdppc*(year == 2001))
	bys countryname (year): egen lgdppc2020 = total(lgdppc*(year == 2020))
	
	keep if year == 2001
	keep if rainforest == 1
	drop if lgdppc2001 == 0 | lgdppc2020 == 0 | ltcvcf2001 == 0 | ltcvcf2020 == 0
	sort countryname
	gen cty = _n
	count
	local obs = r(N)
	forvalues n = 1(1)`obs' {
		 label def lcty  `n' "`=countryname[`n']'", modify
	}
	label val cty lctygdp
	
	gen ltcvcf0120 = ltcvcf2020 - ltcvcf2001
	gen lgdppc0120 = lgdppc2020 - lgdppc2001
	
	gen poslfg = 3
	replace poslfg = 10 if strpos(lower(countryname),  "cameroon") > 0
	replace poslfg = 10 if strpos(lower(countryname),  "paraguay") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "argentina") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "belize") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "brazil") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "honduras") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "salvador") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "thailand") > 0
	replace poslfg = 9  if strpos(lower(countryname),  "togo") > 0
	replace poslfg = 6  if strpos(lower(countryname),  "suriname") > 0
	replace poslfg = 5  if strpos(lower(countryname),  "papua") > 0
	
	#delimit ;
		tw 
			(scatter ltcvcf0120 lgdppc0120 if strpos(lower(regionname), "africa") > 0 , sort ms(o) mc("146 195 51") mlabc(black) mlabel(countryname) mlabvp(poslfg) mlabs(*.3) mlabg(0.1))
			(scatter ltcvcf0120 lgdppc0120 if strpos(lower(regionname), "america") > 0, sort ms(o) mc("64 105 166") mlabc(black) mlabel(countryname) mlabvp(poslfg) mlabs(*.3) mlabg(0.1))
			(scatter ltcvcf0120 lgdppc0120 if strpos(lower(regionname), "asia") > 0   , sort ms(o) mc("200 0 0") mlabc(black) mlabel(countryname) mlabvp(poslfg) mlabs(*.3) mlabg(0.1))
			(scatter ltcvcf0120 lgdppc0120 if strpos(lower(regionname), "oceania") > 0, sort ms(o) mc("251 222 6") mlabc(black) mlabel(countryname) mlabvp(poslfg) mlabs(*.3) mlabg(0.1)),
			yline(0, lcolor(black*0.5) lp("-"))
			ytitle(" ", axis(1) height(5) size(2))  
			xline(0, lcolor(black*0.5) lp("-"))
			ytitle("Log variation of tree cover between 2001 and 2020", axis(1) height(3) size(3))  
			xtitle("Log variation of GDP per capita between 2001 and 2020 (constant 2015 USD)", axis(1) height(3) size(3))  
			graphregion(fcolor(none) lcolor(none) style(none)) 
			plotregion(lcolor(none) ilcolor(none)) 
			ylabel(, labsize(*0.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
			xlabel(, labsize(*0.8) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
			legend(
				title(" ", height(2) size(*.5)) 
				order(1 2 3 4) 
				lab(1 `"Africa"') 
				lab(2 `"South-America"') 
				lab(3 `"Asia"') 
				lab(4 `"Oceania"') 
				keygap(*0.50) 
				row(1) col(4) 
				rowgap(*2) 
				size(*.8) 
				ring(1) 
				position(6)
				region(lstyle(none) fcolor(none)) 
				margin(zero) 
				bmargin(zero)
			)
			scheme(s1mono)
			caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
			name(TCVCFGDPPC0120P1, replace) ;
	#delimit cr
restore

* FOREST AREA AND POVERTY RATE IN RAINFOREST BELT 

#delimit ;
	tw 
		(scatter  lfarea year if year >= 1990 & strpos(lower(countryname), "brazil") > 0, sort yaxis(1) ms(o) connect(l) mc("146 195 51") mlw("thin") lcolor("146 195 51") lpattern("-") lw("thin"))
		(scatter  sipovdday year if year >= 1990 & strpos(lower(countryname), "brazil") > 0, sort yaxis(2) ms(o) connect(l) mc("200 0 0") mlw("thin") lcolor("200 0 0") lpattern("-") lw("thin"))
		(scatter  sipovlmic year if year >= 1990 & strpos(lower(countryname), "brazil") > 0, sort yaxis(2) ms(o) connect(l) mc("240 173 78") mlw("thin") lcolor("240 173 78") lpattern("-") lw("thin"))
		(scatter  sipovumic year if year >= 1990 & strpos(lower(countryname), "brazil") > 0, sort yaxis(2) ms(o) connect(l) mc("64 105 166") mlw("thin") lcolor("64 105 166") lpattern("-") lw("thin"))
		, xtitle("Years", height(5) size(2)) 
		ytitle("Forest area (log million ha)", axis(1) height(5) size(2))  
		ytitle("Poverty rate (%)", axis(2) height(5) size(2))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(1990(2)2020, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2 3 4) 
			lab(1 `"Forest area"') 
			lab(2 `"Poverty rate at $1.90 a day (2011 PPP)"') 
			lab(3 `"Poverty rate at $3.20 a day (2011 PPP)"') 
			lab(4 `"Poverty rate at $5.50 a day (2011 PPP)"') 
			keygap(*0.50) 
			row(2) col(2) 
			rowgap(*0.1) 
			size(*.5) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(FAREAPDDAYBRA, replace) ;
#delimit cr

#delimit ;
	tw 
		(scatter     lfarea year if year >= 1990 & strpos(lower(countryname), "dem. rep.") > 0, sort yaxis(1) ms(Oh) connect(l) mc("146 195 51") mlw("thin") lcolor("146 195 51") lpattern("-") lw("thin"))
		(scatter  sipovdday year if year >= 1990 & strpos(lower(countryname), "dem. rep.") > 0, sort yaxis(2) ms(Oh) connect(l) mc("200 0 0") mlw("thin") lcolor("200 0 0") lpattern("-") lw("thin"))
		(scatter  sipovlmic year if year >= 1990 & strpos(lower(countryname), "dem. rep.") > 0, sort yaxis(2) ms(Oh) connect(l) mc("240 173 78") mlw("thin") lcolor("240 173 78") lpattern("-") lw("thin"))
		(scatter  sipovumic year if year >= 1990 & strpos(lower(countryname), "dem. rep.") > 0, sort yaxis(2) ms(Oh) connect(l) mc("64 105 166") mlw("thin") lcolor("64 105 166") lpattern("-") lw("thin"))
		, xtitle("Years", height(5) size(2)) 
		ytitle("Forest area (log million ha)", axis(1) height(5) size(2))  
		ytitle("Poverty rate (%)", axis(2) height(5) size(2))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(1990(2)2020, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2 3 4) 
			lab(1 `"Forest area"') 
			lab(2 `"Poverty rate at $1.90 a day (2011 PPP)"') 
			lab(3 `"Poverty rate at $3.20 a day (2011 PPP)"') 
			lab(4 `"Poverty rate at $5.50 a day (2011 PPP)"') 
			keygap(*0.50) 
			row(2) col(2) 
			rowgap(*0.1) 
			size(*.5) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(FAREAPDDAYDRC, replace) ;
#delimit cr

#delimit ;
	tw
		(scatter     lfarea year if year >= 1990 & strpos(lower(countryname), "indonesia") > 0, sort yaxis(1) ms(Oh) connect(l) mc("146 195 51") mlw("thin") lcolor("146 195 51") lpattern("-") lw("thin"))
		(scatter  sipovdday year if year >= 1990 & strpos(lower(countryname), "indonesia") > 0, sort yaxis(2) ms(Oh) connect(l) mc("200 0 0") mlw("thin") lcolor("200 0 0") lpattern("-") lw("thin"))
		(scatter  sipovlmic year if year >= 1990 & strpos(lower(countryname), "indonesia") > 0, sort yaxis(2) ms(Oh) connect(l) mc("240 173 78") mlw("thin") lcolor("240 173 78") lpattern("-") lw("thin"))
		(scatter  sipovumic year if year >= 1990 & strpos(lower(countryname), "indonesia") > 0, sort yaxis(2) ms(Oh) connect(l) mc("64 105 166") mlw("thin") lcolor("64 105 166") lpattern("-") lw("thin"))
		, xtitle("Years", height(5) size(2)) 
		ytitle("Forest area (log million ha)", axis(1) height(5) size(2))  
		ytitle("Poverty rate (%)", axis(2) height(5) size(2))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(1990(2)2020, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2 3 4) 
			lab(1 `"Forest area"') 
			lab(2 `"Poverty rate at $1.90 a day (2011 PPP)"') 
			lab(3 `"Poverty rate at $3.20 a day (2011 PPP)"') 
			lab(4 `"Poverty rate at $5.50 a day (2011 PPP)"') 
			keygap(*0.50) 
			row(2) col(2) 
			rowgap(*0.1) 
			size(*.5) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(FAREAPDDAYIDN, replace) ;
#delimit cr


#delimit ;
	tw
		(scatter     lfarea year if year >= 1990 & strpos(lower(countryname), "ivoire") > 0, sort yaxis(1) ms(Oh) connect(l) mc("146 195 51") mlw("thin") lcolor("146 195 51") lpattern("-") lw("thin"))
		(scatter  sipovdday year if year >= 1990 & strpos(lower(countryname), "ivoire") > 0, sort yaxis(2) ms(Oh) connect(l) mc("200 0 0") mlw("thin") lcolor("200 0 0") lpattern("-") lw("thin"))
		(scatter  sipovlmic year if year >= 1990 & strpos(lower(countryname), "ivoire") > 0, sort yaxis(2) ms(Oh) connect(l) mc("240 173 78") mlw("thin") lcolor("240 173 78") lpattern("-") lw("thin"))
		(scatter  sipovumic year if year >= 1990 & strpos(lower(countryname), "ivoire") > 0, sort yaxis(2) ms(Oh) connect(l) mc("64 105 166") mlw("thin") lcolor("64 105 166") lpattern("-") lw("thin"))
		, xtitle("Years", height(5) size(2)) 
		ytitle("Forest area (log million ha)", axis(1) height(5) size(2))  
		ytitle("Poverty rate (%)", axis(2) height(5) size(2))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(1990(2)2020, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2 3 4) 
			lab(1 `"Forest area"') 
			lab(2 `"Poverty rate at $1.90 a day (2011 PPP)"') 
			lab(3 `"Poverty rate at $3.20 a day (2011 PPP)"') 
			lab(4 `"Poverty rate at $5.50 a day (2011 PPP)"') 
			keygap(*0.50) 
			row(2) col(2) 
			rowgap(*0.1) 
			size(*.5) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(FAREAPDDAYCIV, replace) ;
#delimit cr

#delimit ;
	tw
		(scatter     lfarea year if year >= 1990 & strpos(lower(countryname), "benin") > 0, sort yaxis(1) ms(Oh) connect(l) mc("146 195 51") mlw("thin") lcolor("146 195 51") lpattern("-") lw("thin"))
		(scatter  sipovdday year if year >= 1990 & strpos(lower(countryname), "benin") > 0, sort yaxis(2) ms(Oh) connect(l) mc("200 0 0") mlw("thin") lcolor("200 0 0") lpattern("-") lw("thin"))
		(scatter  sipovlmic year if year >= 1990 & strpos(lower(countryname), "benin") > 0, sort yaxis(2) ms(Oh) connect(l) mc("240 173 78") mlw("thin") lcolor("240 173 78") lpattern("-") lw("thin"))
		(scatter  sipovumic year if year >= 1990 & strpos(lower(countryname), "benin") > 0, sort yaxis(2) ms(Oh) connect(l) mc("64 105 166") mlw("thin") lcolor("64 105 166") lpattern("-") lw("thin"))
		, xtitle("Years", height(5) size(2)) 
		ytitle("Forest area (log million ha)", axis(1) height(5) size(2))  
		ytitle("Poverty rate (%)", axis(2) height(5) size(2))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(1990(2)2020, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2 3 4) 
			lab(1 `"Forest area"') 
			lab(2 `"Poverty rate at $1.90 a day (2011 PPP)"') 
			lab(3 `"Poverty rate at $3.20 a day (2011 PPP)"') 
			lab(4 `"Poverty rate at $5.50 a day (2011 PPP)"') 
			keygap(*0.50) 
			row(2) col(2) 
			rowgap(*0.1) 
			size(*.5) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(FAREAPDDAYBEN, replace) ;
#delimit cr


#delimit ;
	tw
		(scatter     lfarea year if year >= 1990 & strpos(lower(countryname), "senegal") > 0, sort yaxis(1) ms(Oh) connect(l) mc("146 195 51") mlw("thin") lcolor("146 195 51") lpattern("-") lw("thin"))
		(scatter  sipovdday year if year >= 1990 & strpos(lower(countryname), "senegal") > 0, sort yaxis(2) ms(Oh) connect(l) mc("200 0 0") mlw("thin") lcolor("200 0 0") lpattern("-") lw("thin"))
		(scatter  sipovlmic year if year >= 1990 & strpos(lower(countryname), "senegal") > 0, sort yaxis(2) ms(Oh) connect(l) mc("240 173 78") mlw("thin") lcolor("240 173 78") lpattern("-") lw("thin"))
		(scatter  sipovumic year if year >= 1990 & strpos(lower(countryname), "senegal") > 0, sort yaxis(2) ms(Oh) connect(l) mc("64 105 166") mlw("thin") lcolor("64 105 166") lpattern("-") lw("thin"))
		, xtitle("Years", height(5) size(2)) 
		ytitle("Forest area (log million ha)", axis(1) height(5) size(2))  
		ytitle("Poverty rate (%)", axis(2) height(5) size(2))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(1990(2)2020, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2 3 4) 
			lab(1 `"Forest area"') 
			lab(2 `"Poverty rate at $1.90 a day (2011 PPP)"') 
			lab(3 `"Poverty rate at $3.20 a day (2011 PPP)"') 
			lab(4 `"Poverty rate at $5.50 a day (2011 PPP)"') 
			keygap(*0.50) 
			row(2) col(2) 
			rowgap(*0.1) 
			size(*.5) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(FAREAPDDAYSEN, replace) ;
#delimit cr


#delimit ;
	tw 
		(line farearfc year if tagrfc == 1 & year >= 1990 & rainforest == 1 & strpos(lower(regionname), "africa") > 0, sort yaxis(1) clcolor("146 195 51") clpattern("l") clw("thin")) 
		(line farearfc year if tagrfc == 1 & year >= 1990 & rainforest == 1 & strpos(lower(regionname), "america") > 0, sort yaxis(1) clcolor("146 195 51") clpattern("-") clw("thin")) 
		(line farearfc year if tagrfc == 1 & year >= 1990 & rainforest == 1 & strpos(lower(regionname), "asia") > 0, sort yaxis(1) clcolor("146 195 51") clpattern("l") clw("_")) 
		(line farearfc year if tagrfc == 1 & year >= 1990 & rainforest == 1 & strpos(lower(regionname), "oceania") > 0, sort yaxis(1) clcolor("146 195 51") clpattern("-.") clw("thin")) 
		(line gdppcrfc year if tagrfc == 1 & year >= 1990 & rainforest == 1 & strpos(lower(regionname), "africa") > 0, sort yaxis(2) clcolor("64 105 166") clpattern("l") clw("thin"))
		(line gdppcrfc year if tagrfc == 1 & year >= 1990 & rainforest == 1 & strpos(lower(regionname), "america") > 0, sort yaxis(2) clcolor("64 105 166") clpattern("-") clw("thin"))
		(line gdppcrfc year if tagrfc == 1 & year >= 1990 & rainforest == 1 & strpos(lower(regionname), "asia") > 0, sort yaxis(2) clcolor("64 105 166") clpattern("_") clw("thin"))		
		(line gdppcrfc year if tagrfc == 1 & year >= 1990 & rainforest == 1 & strpos(lower(regionname), "oceania") > 0, sort yaxis(2) clcolor("64 105 166") clpattern("-.") clw("thin"))		
		,
		xtitle("Years", height(5) size(2)) 
		ytitle("Forest area (Million ha)", axis(1) height(5) size(2))  
		ytitle("GDP per capita (constant 2015 USD)", axis(2) height(5) size(2))  
		graphregion(fcolor(none) lcolor(none) style(none)) 
		plotregion(lcolor(none) ilcolor(none)) 
		ylabel(, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black"))
		ylabel(, labsize(*0.5) axis(2) glcolor(gs15) labcolor("black") tlcolor("black"))
		xlabel(1990(1)2020, labsize(*0.5) axis(1) glcolor(gs15) labcolor("black") tlcolor("black")) 
		legend(
			title(" ", height(2) size(*.5)) 
			order(1 2 3 4 5 6 7 8) 
			lab(1 `"Forest across Africa"') 
			lab(2 `"Forest across Americas"') 
			lab(3 `"Forest across Asia"') 
			lab(4 `"Forest across Oceania"') 
			lab(5 `"GDP per capita Africa"') 
			lab(6 `"GDP per capita America"') 
			lab(7 `"GDP per capita Asia"') 
			lab(8 `"GDP per capita Oceania"') 
			keygap(*0.50) 
			row(2) col(4) 
			rowgap(*0.1) 
			size(*.5) 
			ring(1) 
			position(6)
			region(lstyle(none) fcolor(none)) 
			margin(zero) 
			bmargin(zero)
		)
		scheme(s1mono)
		caption( "{bf:Source}: Authors' own elaboration.", height(2) size(*.5) color(black))
		name(FAREAGDPPCRFC1, replace) ;
#delimit cr
