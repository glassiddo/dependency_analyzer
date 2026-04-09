use "$projdir\data\Build\World\WDI\WDI_raw.dta", clear

* land area forward/backfill (constant data of land area, but partial data)
bys country (year): replace ag_lnd_totl_k2 = ag_lnd_totl_k2[_n-1] if missing(ag_lnd_totl_k2)
gsort country -year
bys country: replace ag_lnd_totl_k2 = ag_lnd_totl_k2[_n-1] if missing(ag_lnd_totl_k2)

gen aid_gdp=aid/gdp_cd
label var aid_gdp "ODA (% GDP)"

sort country_name
drop if region=="Aggregates"
drop if region==""
egen id=group(country_name)
*drop if  incomelevel=="HIC"

foreach var of varlist Empl* urb_pop agr{
replace `var'=`var'[_n-1] if !missing(`var'[_n-1]) & missing(`var') & id==id[_n-1]
}

gen nonag_pop=EmplShareInd+EmplShareServ
label var nonag_pop "Non-Agricultural Employment (%Workforce)"

gen gdp_ag_ppp=gdp_ppp*gdp_ag_cd/gdp_cd
gen gdp_serv_ppp=gdp_ppp*gdp_serv_cd/gdp_cd
gen gdp_ind_ppp=gdp_ppp*gdp_ind_cd/gdp_cd

global amazon `" "Argentina" "Bolivia" "Brazil" "Colombia" "Ecuador" "Guyana" "Panama" "Paraguay" "Peru" "Suriname" "Venezuela" "'
global congo `" "Cameroon" "Central African Republic" "Congo, Dem. Rep." "Congo, Rep." "Equatorial Guinea" "Gabon" "'
global australiasia `" "Australia" "New Guinea" "'
global sundaland `" "Brunei" "Indonesia" "Malaysia" "Singapore" "'
global indoburma `" "Bangladesh" "Cambodia" "China" "India" "Lao" "Myanmar" "Thailand" "Viet Nam" "'
global mesoamerica `" "Belize" "Costa Rica" "El Salvador" "Guatemala" "Honduras" "Mexico" "Nicaragua" "'
global westafrica `" "Benin" "Ivoire" "Ghana" "Guinea" "Liberia" "Nigeria" "Sierra Leone" "Togo" "'

global rainforest "${amazon} ${congo} ${australiasia} ${sundaland} ${indoburma} ${mesoamerica} ${westafrica}"

gen rainforest = 0
foreach cty of global rainforest {
	qui replace rainforest = 1 if strpos(lower(country_name), lower("`cty'")) >0 
}
la var rainforest "Countries with tropical rainforest"
save "$projdir\data\Build\World\WDI\WDI_Clean_b4_sdn.dta", replace