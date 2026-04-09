*******************************************************************************************************************
* Aggregate Nightlight including predicted gdp into decades for using in the model
*******************************************************************************************************************

use "${projdir}\DATA\BUILD\AFRICA\NIGHT LIGHT\SampleNTLGDP.dta", clear
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