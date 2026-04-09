import delimited using "$projdir\data\Raw\World\WDI\ISOCODES.csv", clear
keep if region=="Africa"
keep name alpha3
rename name country_name
rename alpha3 country_code
tempfile codes
save `codes'


use "$projdir\data\Build\World\WDI\WDI Clean.dta", clear
keep if !missing(gdp)
drop if year<1991
drop if regionname=="Aggregates"
* Merging the two Sudans and dropping Djibouti
replace country_code="SDN" if country_code=="SSD"
drop if country_code=="DJI"
collapse (mean) aid_gdp (sum) gdp*, by(country_code year)
foreach var of varlist gdp* aid_gdp{
	recode `var' 0=.
}
merge m:1 country_code using `codes', keep(matched) nogen

* we have a missing data problem, with many countries not reporting the ag /non ag split in gdp before the 2000s *
foreach series in "" _cd _ppp{
foreach sector in ag ind serv{
bys country_name: egen gdp_`sector'`series'_first_year=min(year) if !missing(gdp_`sector'`series')
bys country_name: egen gdp_`sector'`series'_share=max((gdp_`sector'`series'_first_year==year)*gdp_`sector'`series'/gdp`series')
replace gdp_`sector'`series'=gdp`series'*gdp_`sector'`series'_share if missing(gdp_`sector')
	
}
}
drop *first_year *share
replace country_name="Tanzania" if regexm(country_name,"Tanzania")
keep gdp* country_name year country_code aid_gdp
save "$projdir\data\Final\gdp", replace





