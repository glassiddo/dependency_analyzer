**********************************************************************
* THIS PROGRAM ESTIMATES NON AG GDP USING DATA ON NTL AND LAND COVER *
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

**********************************************************************
* Part 2: Estimating elesticity of night time light on GDP by sector *
**********************************************************************
// FE regressions to estimate the elasticity of nightlight on GDP by sector. there are 12 regressions - (1) nighttime light for all countries; (2) night light + land cover controls for all countries; (3) like model (1) but for SSA only; (4) like model (2) but for SSA only. each of those 4 for each of the three sectors (nag, ind, serv)
// the coefficients, indicating how much (log of) gdp by sector changes when the nightlight brightness changes by 1%.

// the results are not saved in this part - *this part is for understanding and display only* 

encode iso3, gen(iso3_num)

* set the panel data structure
xtset iso3_num year, yearly

compress

quietly{
	foreach t in nag ind serv {
		eststo  ntl`t'allshort: xtreg lgdp_`t'_cd i.viirs#c.lntlv if inrange(year, 1993, 2023), fe vce(robust)
		eststo  ntl`t'alllong : xtreg lgdp_`t'_cd i.viirs#c.lntlv lforestha lcroplandha_ext if inrange(year, 1993, 2023), fe vce(robust)
		// ssf is the region code of sub saharan africa
		eststo  ntl`t'ssfshort: xtreg lgdp_`t'_cd i.viirs#c.lntlv if inrange(year, 1993, 2023) & region == "SSF", fe vce(robust)
		eststo  ntl`t'ssflong : xtreg lgdp_`t'_cd i.viirs#c.lntlv lforestha lcroplandha_ext if inrange(year, 1993, 2023) & region == "SSF", fe vce(robust)
	}
	
	#delimit ;
		noisily estout ntlindallshort ntlindalllong ntlservallshort ntlservalllong ntlservallshort ntlnagalllong , label margin 
			cells(`"b(star fmt(3))"' `"se(par fmt(2))"') stardetach
			equations(1) starlevels(* 0.10 ** 0.05 *** 0.01)
			drop(_cons) 
			refcat(0.viirs#c.lntlv "Nighttime light brightness", nolabel)
			title(`"Eslasticity of nighttime light on GDP by sector, All countries 1993-2023"') 
			varlabels(
				0.viirs#c.lntlv " - DMSP (1993-2012)"
				1.viirs#c.lntlv " - VIIRS (2013-2023)"
				lforestha  "Forest cover" 
				lcroplandha_ext "Cropland"
				, end("") nolast
			)
			mgroup("Industry" "Service" "Industry + Service", pattern(1 0 1 0 1 0) span) 
			mlabel("(1)" "(2)" "(1)" "(2)" "(1)" "(2)") 
			collabels(, none) eqlabels(, none) 
			stats(N_g, fmt(%12.0fc) labels(`"Number of countries"')) legend varwidth(45) nonumbers;	
			
		noisily estout ntlindssfshort ntlindssflong ntlservssfshort ntlservssflong ntlservssfshort ntlnagssflong , label margin 
			cells(`"b(star fmt(3))"' `"se(par fmt(2))"') stardetach
			equations(1) starlevels(* 0.10 ** 0.05 *** 0.01)
			drop(_cons) 
			refcat(0.viirs#c.lntlv "Nighttime light brightness", nolabel)
			title(`"Eslasticity of nighttime light on sectoral GDP, Sub-Saharan Africa 1993-2023"') 
			varlabels(
				0.viirs#c.lntlv " - DMSP (1993-2012)"
				1.viirs#c.lntlv " - VIIRS (2013-2023)"
				lforestha  "Forest cover" 
				lcroplandha_ext "Cropland"
				, end("") nolast
			)
			mgroup("Industry" "Service" "Industry + Service", pattern(1 0 1 0 1 0) span) 
			mlabel("(1)" "(2)" "(1)" "(2)" "(1)" "(2)") 
			collabels(, none) eqlabels(, none) 
			stats(N_g, fmt(%12.0fc) labels(`"Number of countries"')) legend varwidth(45) nonumbers;	
	#delimit cr
}


***************************************************************
* Part 3: Nighttime light, land cover at the admin unit level *
***************************************************************
// the initial version (before part 4) is quite similiar to part 1 - SampleNTLGDP is iniitally a year x admin unit dataset with nightlight pixel and area values (from different satellites), as well as data on area by land classification (grass, forest etc.) in this part, if there is more than a single satellite for a given unit x year, we take the maximum rather, unlike in the country level calculation where we took the average 

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
save "${projdir}\DATA\BUILD\AFRICA\NIGHT LIGHT\SampleNTLGDP.dta", replace

*******************************************************************************************************************
* Part 4: Now we use elesticity from country regressiions to predict non agricultural gdd at the admin unit level *
*******************************************************************************************************************
// it runs generally the same regressions as in part 2, but only the sub saharan african regressions this time
// what this part does - whenever we don't have gdp values per sector, we impute it using the nightlight. we use estimates on the elasticity of gdp to nightlight in ssa (how much gdp per sector changes when nightlight brightness increases by 1%) to impute values for gdp per sector in the sample units. hence 6 regressions - 3 sectors x 2 models (one with land controls, one without)
// the results are saved directly on sampleNTLGDP.dta and also collapsed to a different dataset in unit x decade (2000/2010 only) which we call nonag_shares 

quietly{
	foreach v in nag ind serv {
		
		*<<< We only use nighttime light
		
		use "${projdir}\DATA\BUILD\WORLD\WorldNTLGDP.dta", clear
		
		encode iso3, gen(iso3_num)
		xtset iso3_num year, yearly
		
		local lv = "`: var la gdp_`v'_cd'"
		
		xtreg lgdp_`v'_cd i.viirs#c.lntlv if inrange(year, 1993, 2023) & region == "SSF", fe vce(robust)
		
		use "${projdir}\DATA\BUILD\AFRICA\NIGHT LIGHT\SampleNTLGDP.dta", clear
		
		predict lgdp_`v'_cd_hat, xb
		gen gdp_`v'_cd_hata = exp(lgdp_`v'_cd_hat)
		la var gdp_`v'_cd_hata "Predicted `lv', short"		
		
		cap drop lgdp_`v'_cd_hat
		save "${projdir}\DATA\BUILD\AFRICA\NIGHT LIGHT\SampleNTLGDP.dta", replace
		
		* We use nighttime light and land cover
		
		use "${projdir}\DATA\BUILD\WORLD\WorldNTLGDP.dta", clear
		
		encode iso3, gen(iso3_num)
		xtset iso3_num year, yearly

		local lv = "`: var la gdp_`v'_cd'"
		
		xtreg lgdp_`v'_cd i.viirs#c.lntlv lforestha lcroplandha_ext if inrange(year, 2001, 2023) & region == "SSF", fe vce(robust)
		
		use "${projdir}\DATA\BUILD\AFRICA\NIGHT LIGHT\SampleNTLGDP.dta", clear
		
		predict lgdp_`v'_cd_hat, xb
		gen gdp_`v'_cd_hatb = exp(lgdp_`v'_cd_hat)
		la var gdp_`v'_cd_hatb "Predicted `lv', long"		
		
		cap drop lgdp_`v'_cd_hat
		save "${projdir}\DATA\BUILD\AFRICA\NIGHT LIGHT\SampleNTLGDP.dta", replace
		
	}
}

collapse (first) country admin_name geotype areaha (sum) gdp_*_cd_hat*, by(ipums_id decade)

quietly{
	foreach v in nag ind serv {
		
		bysort country decade (ipums_id): egen gdp_`v'_cd_hata_tot = total(gdp_`v'_cd_hata)
		
		gen gdp_`v'_shra = gdp_`v'_cd_hata/gdp_`v'_cd_hata_tot
		la var gdp_`v'_shra "Share `lv', % Country - short"
		
		bysort country decade (ipums_id): egen gdp_`v'_cd_hatb_tot = total(gdp_`v'_cd_hatb)
		
		gen gdp_`v'_shrb = gdp_`v'_cd_hatb/gdp_`v'_cd_hatb_tot
		la var gdp_`v'_shrb "Share `lv', % Country - long"
		
		cap drop gdp_`v'_cd_hata gdp_`v'_cd_hatb gdp_`v'_cd_hata_tot gdp_`v'_cd_hatb_tot
		
	}
}

la var country "Country"
la var admin_name "Admin unit"
la var ipums_id "IPUMS ID"
la var geotype "Unit shape"
la var areaha "Area (ha)"

la var gdp_nag_shra "Share of non-agricultural GDP (Current USD), % Country - short"
la var gdp_nag_shrb "Share of non-agricultural GDP (Current USD), % Country - long"

la var gdp_ind_shra "Share of Industry GDP (Current USD), % Country - short"
la var gdp_ind_shrb "Share of Industry GDP (Current USD), % Country - long"

la var gdp_serv_shra "Share of Services GDP (Current USD), % Country - short"
la var gdp_serv_shrb "Share of Services GDP (Current USD), % Country - long"

keep if inlist(decade, 2000, 2010)

rename decade year 
compress
save "${projdir}\DATA\BUILD\AFRICA\NIGHT LIGHT\nonag_shares.dta", replace

reshape wide gdp_nag_shra gdp_nag_shrb gdp_ind_shra gdp_ind_shrb gdp_serv_shra gdp_serv_shrb, i(ipums_id) j(year)
aorder
order ipums_id country admin_name geotype areaha