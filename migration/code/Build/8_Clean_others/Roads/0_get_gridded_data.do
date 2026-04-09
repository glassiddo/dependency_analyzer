


/*
*** Just to check I can replicat their result
use ".\citypanel.dta", clear
keep if country=="Benin" | country=="Botswana" | country=="Burkina Faso" | country=="Ghana" | country=="Guniea" | country=="Kenya" | country=="Mali" | country=="Mozambique" | country=="Senegal" | country=="Sierra Leone" | country=="Tanzania" | country=="Uganda" | country=="Zambia" 

local spatialcube "i.year|longitude i.year|longitude2 i.year|longitude3 i.year|latitude i.year|latitude2 i.year|latitude3 i.year|longlat i.year|long2lat i.year|longlat2"
xi: areg S10.lpop change_MA L10.lpop `spatialcube', robust cluster(prov60) absorb(countryyear)
xi: areg S10.lpop change_MAroads_excl15 L10.lpop if year==2010 , robust  absorb(country) cluster(prov60) 
*** spatial cube doesn't seem really necssary
xi: areg S10.lpop change_MA L10.lpop , robust cluster(prov60) absorb(countryyear)


xi: areg S10.lpop change_MAroads_excl5 L10.lpop , robust  absorb(country) cluster(prov60) 

*** Doesn't actually work for most recent period!
xi: areg S10.lpop change_MAroads_excl5 L10.lpop  if year==2010  , robust  absorb(country) cluster(prov60) 


xi: areg S10.lpop change_MAroads_excl5   L10.change_MAroads_excl5 L20.change_MAroads_excl5 L10.lpop , robust  absorb(country) cluster(prov60) 
xi: areg S10.lpop change_MAroads_excl5   L10.change_MAroads_excl5 L20.change_MAroads_excl5 L10.lpop if year==2010 , robust  absorb(country) cluster(prov60) 
lincomest change_MA + L10.change_MA + L20.change_MA


xi: areg S10.lpop change_MAroads_excl15   L10.change_MAroads_excl15 L20.change_MAroads_excl15 L10.lpop if year==2010 , robust  absorb(country) cluster(prov60) 
lincomest change_MA + L10.change_MA + L20.change_MA


*** Still seems to hold just in last period (though weakly)
xi: areg S10.lpop change_MA L10.lpop if year==2010 , robust cluster(prov60) absorb(countryyear)

xi: areg S10.lpop change_MA L10.lpop if year==2010 , robust  absorb(country) cluster(prov60) 
xi: areg S10.lpop change_MAroads L10.lpop if year==2010 , robust  absorb(country) cluster(prov60) 
xi: areg S10.lpop change_MAroads_excl5 L10.lpop if year==2010 , robust  absorb(country) cluster(prov60) 
xi: areg S10.lpop change_MAroads_excl15 L10.lpop if year==2010 , robust  absorb(country) cluster(prov60) 

stop
*/

foreach l in 1  2 {


	cd "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `l'"
	
	shp2dta using AFRADM`l'_renamed.shp, data(AFRADM`l'_data) coord(AFRADM`l'_coords) replace

	use "$dropbox\Migration Africa\data\Raw\Africa\Roads\Jedwab Storeygard\citypanel.dta", clear
	

	
	
	geoinpoly latitude longitude using "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `l'\AFRADM`l'_coords.dta"
	
	merge m:1 _ID using "${dropbox}\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `l'\AFRADM`l'_data.dta", nogen keepusing(ipums_id) keep(match)
	gen cells=1
	stop
	collapse (sum) cells pop (mean) lpop  change_MAroads change_MAroads*5 change_MAroads*10 ma1960  cashcrop foodcrop dist2poo  murd_sh dist2anymine, by(ipums_id year)
	drop if ipums_id=="" | year<1990
	compress
	save "${dropbox}\Migration Africa\data\Build\Africa\Roads\collapsed_`l'.dta", replace
	
	
}

clear 

append using "${dropbox}\Migration Africa\data\Build\Africa\Roads\collapsed_1.dta" "${dropbox}\Migration Africa\data\Build\Africa\Roads\collapsed_2.dta"

compress 

save "${dropbox}\Migration Africa\data\Build\Africa\Roads\js_decadal.dta", replace

use "${dropbox}\Migration Africa\data\Build\Africa\Roads\js_decadal.dta",  clear

gegen x=group(ipums_id)
tsset x year
foreach var of varlist change_MAroads* {
	gen L_`var'=S10.`var'
	gen L2_`var'=S20.`var'
}

keep if year==2010
compress
save "${dropbox}\Migration Africa\data\Final\js_change_MA.dta", replace

sstop
drop if year==1990



gen ln_pop=ln(pop)
*** Okay, so this works
xi: areg S10.ln_pop change_MA L10.ln_pop if year==2010 [fw=cells], robust  absorb(country)
xi: areg S10.ln_pop change_MAroad L10.ln_pop if year==2010 [fw=cells], robust  absorb(country)

*** still works
xi: areg S10.ln_pop change_MA L10.ln_pop if year==2010, robust  absorb(country)


foreach var of varlist pop ln_pop lpop cashcrop foodcrop dist2poo murd_sh dist2anymine {
	gen js_D_`var'=S10.`var'
	gen js_`var'_2000=L10.`var'
}

reghdfe js_D_ln_pop change_MAroads js_ln_pop_2000 if year==2010, absorb(country)

stop


sort ipums_id year
by ipums_id: gen decade=_n


tsset ipums_id decade




drop if year==2000

