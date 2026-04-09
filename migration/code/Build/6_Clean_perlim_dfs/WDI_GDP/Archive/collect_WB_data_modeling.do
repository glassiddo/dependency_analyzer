/*****************************************************************
PROJECT: 		SSA Enrivonment
				
TITLE:			collect_WB_data_modeling.do

DESCRIPTION: 	Download WB data for ROW in modeling

				
******************************************************************/

clear all
set more off

* Download from WB portal
wbopendata, indicator(AG.LND.TOTL.K2; AG.LND.FRST.K2; SL.TLF.TOTL.IN; /// 
SL.AGR.EMPL.ZS) year(1996:2015) clear long

* Rename and label vars
la var ag_lnd_totl_k2 "Land area (sq. km)"
rename ag_lnd_totl_k2 total_area
la var ag_lnd_frst_k2 "Forest area (sq. km)"
rename ag_lnd_frst_k2 treecover
la var sl_agr_empl_zs "Employment in agriculture (% of total employment)"
la var sl_tlf_totl_in "Labor force, total"

* Keep two regions
keep if adminregion=="SSA" | countryname=="World" | countryname=="Sub-Saharan Africa"
 /*
* Keep base & end line years
*keep if year==2000 | year==2015

*SMM these are actually 2000, 2010
keep if year==2000 | year==2010


g F0 = (treecover/total_area)*100 if year==2000
g bl_ag = (sl_agr_empl_zs*sl_tlf_totl_in)/100 if year==2000
g el_ag = (sl_agr_empl_zs*sl_tlf_totl_in)/100 if year==2010
g bl_nonag = (sl_tlf_totl_in-bl_ag) if year==2000
g el_nonag = (sl_tlf_totl_in-el_ag) if year==2010
g bl_pa = sl_tlf_totl_in if year==2000
g el_pa = sl_tlf_totl_in if year==2010
drop sl_tlf_totl_in sl_agr_empl_zs

local xi F0 bl_ag el_ag bl_nonag el_nonag bl_pa el_pa

foreach var of varlist `xi' { 
	bysort countryname (`var'): replace `var' = `var'[1]
	}

keep if year==2000

* Add an id for ROW
g ipums_id=9999
g port_region=1 // This one might no longer be necessary

* Clean unecessary vars
drop region regionname adminregion adminregionname countrycode ///
incomelevel incomelevelname lendingtype lendingtypename year
rename countryname country_name

* Create vars to match other data and leave missing (GAMS setting)
g Lambda=3
g bl_log_nlv_ra=.

g delta_log_nlv_ra=.
g delta_nonag_ann=(el_nonag-bl_nonag)/11
drop el_nonag
g delta_ag_ann=(el_ag-bl_ag)/11
drop el_ag

order ipums_id country_name total_area treecover F0 bl_nonag	bl_ag delta_nonag_ann delta_ag_ann bl_pa el_pa pyld bl_log_nlv_ra delta_log_nlv_ra	port_region
 */
 
 
*** Make this so that I get the raw counts for each year to match with the
* census year in each country 

gen F0_data = (treecover/total_area)
gen Na0 = (sl_agr_empl_zs*sl_tlf_totl_in)/100 
gen Nm0 = (sl_tlf_totl_in-Na0)
rename sl_tlf_totl N

gen Sa0_data = 1 -F0_data
rename total_area Lbar

gen Lambda = 0.3
rename countryname country_name

keep country_name year Lbar Nm0 Na0 F0_data N Lambda treecover

save "${build}/World/row.dta", replace
