clear all
wbopendata, indicator(AG.LND.FRST.ZS;AG.LND.AGRI.ZS;SP.URB.TOTL.IN.ZS;SP.POP.TOTL; ///
AG.LND.TOTL.K2;NY.GDP.MKTP.CD;NY.GDP.PCAP.CD;NY.GDP.MKTP.KD;NY.GDP.PCAP.KD; NY.GDP.MKTP.PP.KD; ///
NV.AGR.TOTL.CD;NV.IND.TOTL.CD;NV.SRV.TOTL.CD;NV.AGR.TOTL.KD;NV.IND.TOTL.KD; ///
NV.SRV.TOTL.KD;SL.EMP.TOTL.SP.ZS;SP.POP.TOTL;SL.AGR.EMPL.ZS;SL.IND.EMPL.ZS;SL.SRV.EMPL.ZS) clear long


rename sl_emp_totl_sp_zs  EmpltoPopRatio
rename sl_srv_empl EmplShareServ
rename sl_ind_empl EmplShareInd
rename sl_agr_empl EmplShareAg

rename nv_srv_totl_kd gdp_serv
rename nv_ind_totl_kd gdp_ind
rename nv_agr_totl_kd gdp_ag
rename nv_srv_totl_cd gdp_serv_cd
rename nv_ind_totl_cd gdp_ind_cd
rename nv_agr_totl_cd gdp_ag_cd
rename sp_pop_totl population_total


label variable gdp_serv  "VA in Services (Constant 2015 USD)"
label variable gdp_ind  "VA in Industry (Constant 2015 USD)"
label variable gdp_ag  "VA in Agriculture (Constant 2015 USD)"
label variable gdp_serv_cd  "VA in Services (Current USD)"
label variable gdp_ind_cd  "VA in Industry (Current USD)"
label variable gdp_ag_cd  "VA in Agriculture (Current USD)"
rename ny_gdp_mktp_cd  gdp_cd
label variable gdp_cd  "GDP (Current USD)"
rename ny_gdp_mktp_kd  gdp
label variable gdp  "GDP  (Constant 2015 USD)"
rename ny_gdp_pcap_kd  gdp_pc
label variable gdp  "GDP Per Capita (Constant 2015 USD)"
rename ny_gdp_mktp_pp_kd  gdp_ppp
label variable gdp  "GDP  (Constant 2021 USD PPP)"
rename ny_gdp_pcap_cd gdp_pc_cd
label variable gdp_pc "GDP Per Capita (Current USD)"
rename ag_lnd_frst_zs forest
label variable forest "Forest area (% of land area)"
rename ag_lnd_agri_zs agr
label variable agr   "Agricultural area (% of land area)"
rename sp_urb_totl_in_zs urb_pop
label variable urb_pop  "Urban population (% of total population)"
rename population_total pop
label var pop "Population"

sort countryname
drop if region=="Aggregates"
drop if region==""
egen id=group(countryname)
*drop if  incomelevel=="HIC"

foreach var of varlist Empl* urb_pop agr{
replace `var'=`var'[_n-1] if !missing(`var'[_n-1]) & missing(`var') & id==id[_n-1]
}

gen nonag_pop=EmplShareInd+EmplShareServ
label var nonag_pop "Non-Agricultural Employment (%Workforce)"

gen gdp_ag_ppp=gdp_ppp*gdp_ag_cd/gdp_cd
gen gdp_serv_ppp=gdp_ppp*gdp_serv_cd/gdp_cd
gen gdp_ind_ppp=gdp_ppp*gdp_ind_cd/gdp_cd

save "$projdir\data\Build\World\WDI\WDI Clean.dta", replace

use "$projdir\data\Build\World\WDI\WDI Clean.dta", clear

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
save "$projdir\data\Build\World\WDI\WDI Clean.dta", replace

