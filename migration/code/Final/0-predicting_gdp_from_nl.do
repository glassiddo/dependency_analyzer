**********************************************************************
* THIS PREDICTS GDP FROM NIGHTLIGHT, WITH LAND COVER AND GDP AS CONTROLS*
**********************************************************************

**********************************************************************
* Part 1: Diagnostics only - estimating elesticity of night time light on GDP by sector *
**********************************************************************
// FE regressions to estimate the elasticity of nightlight on GDP by sector. there are 12 regressions - (1) nighttime light for all countries; (2) night light + land cover controls for all countries; (3) like model (1) but for SSA only; (4) like model (2) but for SSA only. each of those 4 for each of the three sectors (nag, ind, serv)
// the coefficients, indicating how much (log of) gdp by sector changes when the nightlight brightness changes by 1%.

use "${projdir}\DATA\BUILD\WORLD\WorldNTLGDP.dta", clear 

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

*******************************************************************************************************************
* Part 2: Actual imputation - we run the estimations again and use elesticity from country regressions to predict non agricultural gdd at the admin unit level *
*******************************************************************************************************************
// only the sub saharan african regressions this time
// whenever we don't have gdp values per sector, we impute it using the nightlight. we use estimates on the elasticity of gdp to nightlight in ssa (how much gdp per sector changes when nightlight brightness increases by 1%) to impute values for gdp per sector in the sample units. hence 6 regressions - 3 sectors x 2 models (one with land controls, one without)
// the results are saved on sampleNTLGDP.dta and also collapsed to a different dataset in unit x decade (2000/2010 only) which we call nonag_shares 

use "${projdir}\DATA\BUILD\AFRICA\NIGHT LIGHT\SampleNTLGDP_no_predict.dta", clear
save "${projdir}\DATA\BUILD\AFRICA\NIGHT LIGHT\SampleNTLGDP.dta", replace

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