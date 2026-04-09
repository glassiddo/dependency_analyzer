foreach year in 2000 2010 2020{

insheet using "$projdir\data\Raw\World\Trade\TradeData_`year'.csv", comma case clear 

keep primaryValue cmdCode cmdDesc reporterDesc reporterISO flowCode
gen import=primaryValue if flowCode=="M"
gen export=primaryValue if flowCode=="X"

gen ag=cmdCode<=24

gen trade_import_ag=import*ag
gen trade_export_ag=export*ag
gen trade_import_ind=import*(1-ag)
gen trade_export_ind=export*(1-ag)

rename reporterDesc country_name
rename reporterISO country_code
collapse (sum) trade_*, by(country_name country_code)
gen period=`year'

if `year'==2000 {
	tempfile comtrade
	save `comtrade'
}
else{
	append using `comtrade'
	save `comtrade',replace
}
}
replace country_name="Sudan" if regexm(country_name,"Sudan")
replace country_name="Tanzania" if regexm(country_name,"Tanzania")
replace country_name="Côte d'Ivoire" if regexm(country_name,"Ivoire")

	keep if country_name==	"Angola" | /// 
	country_name==	"Benin" | /// 
	country_name== "Botswana"| ///
	country_name==	 "Burkina Faso"| ///
	country_name==	 "Cameroon"| ///
	country_name== "Côte d'Ivoire"| ///
	country_name==  "Ethiopia"| ///
	country_name== "Gabon"| ///
	country_name== "Ghana"| ///
	country_name==	 "Guinea"| ///
	country_name==	"Kenya"| ///
	country_name== "Lesotho"| ///
	country_name== "Madagascar"| ///
	country_name== "Malawi"| ///
	country_name==	"Mali"| ///
	country_name==	"Mauritius"| ///
	country_name==	"Mozambique"| ///
	country_name==  "Nigeria"| ///
	country_name==	 "Rwanda"| ///
	country_name==	 "Senegal"| ///
	country_name==	 "Sierra Leone"| ///
	country_name==	 "South Africa"| ///
	country_name==	 "Sudan"| ///
	country_name==	 "Togo"| ///
	country_name==	"Tanzania"| ///
	country_name==	 "Uganda"| ///
	country_name==	 "Zambia" | ///
	country_name==	 "Zimbabwe"

save `comtrade',replace
/*

foreach flow in export import{

insheet using "$projdir\data\Raw\Africa\Trade\Trade_DetailedTradeMatrix_E_Africa.csv", comma case clear 

if "`flow'"=="import"{
rename ReporterCountries country_name
keep if Element=="Import value" 
}
if "`flow'"=="export"{
rename PartnerCountries country_name
rename  ReporterCountries PartnerCountries
keep if Element=="Export value"
}

keep if country_name==	"Angola" | /// 
country_name== "Benin"| ///
country_name== "Botswana"| ///
country_name==	 "Burkina Faso"| ///
country_name==	 "Cameroon"| ///
country_name== "Côte d'Ivoire"| ///
country_name==  "Ethiopia" ///
country_name== "Ghana"| ///
country_name==	 "Guinea"| ///
country_name==	"Kenya"| ///
country_name== "Lesotho"| ///
country_name== "Malawi"| ///
country_name==	"Mali"| ///
country_name==	"Mauritius"| ///
country_name==	"Mozambique"| ///
country_name==  "Nigeria" ///
country_name==	 "Rwanda"| ///
country_name==	 "Senegal"| ///
country_name==	 "Sierra Leone"| ///
country_name==	 "South Africa"| ///
country_name==	 "Sudan (former)"| ///
country_name==	 "Sudan"| ///
country_name==	 "Togo"| ///
country_name==	"United Republic of Tanzania"| ///
country_name==	 "Uganda"| ///
country_name==	 "Zambia" | ///
country_name==	 "Zimbabwe"
replace country_name="Tanzania" if regexm(country_name,"Tanzania")
rename Y*F YF*


keep Y* country_name PartnerCountries  Item
reshape long Y YF, i(country_name PartnerCountries  Item) j(Year)
rename Y Value
rename YF Source
* Most are actuals 
tab Source
keep if Source=="A"
drop if missing(Value)
replace country_name="Sudan" if regexm(country_name,"Sudan")
collapse (sum) Value, by(country_name  Item Year)
bys country : egen trade_first_year=min(Year)

gen period=cond(inrange(Year,1996,2000),2000,cond(inrange(Year,2006,2010),2010,cond(inrange(Year,2016,2020),2020,.)))
drop if missing(period)

replace Value=Value*1000


sort country_name  Item Year
collapse (mean) Value*, by(country_name  Item period)

collapse (sum) Value*, by(country_name period)
rename Value value_`flow'

tempfile `flow'
save ``flow''
}

use `export'
merge 1:1 country_name period using `import', nogen
tempfile trade_fao
save `trade_fao'
*/
use "$final/gdp.dta", clear
drop if missing(country_name)

keep if country_name==	"Angola" | /// 
country_name==	"Benin" | /// 
country_name== "Botswana"| ///
country_name==	 "Burkina Faso"| ///
country_name==	 "Cameroon"| ///
country_name== "Côte d'Ivoire"| ///
country_name==  "Ethiopia"| ///
country_name== "Gabon"| ///
country_name== "Ghana"| ///
country_name==	 "Guinea"| ///
country_name==	"Kenya"| ///
country_name== "Lesotho"| ///
country_name== "Madagascar"| ///
country_name== "Malawi"| ///
country_name==	"Mali"| ///
country_name==	"Mauritius"| ///
country_name==	"Mozambique"| ///
country_name==  "Nigeria"| ///
country_name==	 "Rwanda"| ///
country_name==	 "Senegal"| ///
country_name==	 "Sierra Leone"| ///
country_name==	 "South Africa"| ///
country_name==	 "Sudan"| ///
country_name==	 "Togo"| ///
country_name==	"Tanzania"| ///
country_name==	 "Uganda"| ///
country_name==	 "Zambia" | ///
country_name==	 "Zimbabwe"

gen period=cond(inrange(year,1996,2000),2000,cond(inrange(year,2006,2010),2010,cond(inrange(year,2016,2020),2020,.)))
collapse (mean) gdp_ag_cd gdp_ind_cd, by(period country_name country_code)

*merge 1:1 country_name period using `trade_fao', keep(master match) nogen
*rename (value_*) (FAO_*)

merge 1:1 country_name period using `comtrade', keep(master match) nogen

gen gdp_cd=gdp_ag_cd+gdp_ind_cd
foreach var of varlist trade_import_ag trade_export_ag trade_import_ind trade_export_ind{
gen `var'_gdp=`var'/(gdp_cd)
}



foreach var in trade_import_ag trade_import_ind trade_export_ag trade_export_ind{
recode `var' (0=.)
bys country_name: egen `var'_gdp_mean=mean(`var'_gdp)
replace `var'= `var'_gdp_mean * gdp_cd if missing(`var')
replace `var'_gdp=`var'_gdp_mean if missing(`var'_gdp)
drop `var'_gdp_mean
}

foreach sector in ag ind{
	gen trade_balance_`sector'=trade_export_`sector'-trade_import_`sector'
	gen trade_balance_`sector'_gdp=trade_export_`sector'_gdp-trade_import_`sector'_gdp
}
gen trade_balance=trade_export_ag-trade_import_ag+trade_export_ind-trade_import_ind
gen trade_balance_gdp=trade_export_ag_gdp-trade_import_ag_gdp+trade_export_ind_gdp-trade_import_ind_gdp

foreach var of varlist *gdp{
	replace `var'=`var'*100
}

foreach sector in ag ind{
label var trade_export_`sector' "`sector' Export (Volume USD)"
label var trade_import_`sector' "`sector' Import (Volume USD)"
label var trade_export_`sector'_gdp "`sector' Export (% GDP)"
label var trade_import_`sector'_gdp "`sector' Import (% GDP)"
label var trade_balance_`sector' "`sector' Trade Balance (Volume USD)"
label var trade_balance_`sector'_gdp "`sector' Trade Balance (% GDP)"
}
label var trade_balance "Trade Balance (Volume USD)"
label var trade_balance_gdp "Trade Balance (% GDP)"
rename period year


keep country_name year trade_* country_code
save "$projdir\data\Build\Africa\Trade\Trade_Value.dta", replace
save "$projdir\data\Final\trade.dta", replace


