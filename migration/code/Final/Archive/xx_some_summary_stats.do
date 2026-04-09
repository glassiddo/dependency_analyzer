


********* Check distribution of employment and nightlights

use "$build/Africa/Night Light/nightlight_shares.dta",clear

tostring ipums_id, gen(ipums_id_temp)
drop ipums_id 
ren ipums_id_temp ipums_id
replace ipums_id="0"+ipums_id if country=="Botswana"

merge m:1 ipums_id using  "${final}/employment.dta", keep(match)

tab country if _merge!=3

keep if year==2000 | year==2010

foreach t in bl el {
	gen `t'_mfg_share=`t'_mfg_pa/(`t'_ag_pa+`t'_mfg_pa+`t'_svc_pa)
}
sum bl_mfg_share if year==2000, det

sum bl_mfg_share if ntlv_shr==0 & year==2000, det

tab country if ntlv_shr==0 & year==2000 & bl_mfg_share>.1

tab 
stop






**** Check employment shares, first at country level

use "${final}/employment.dta", clear

collapse (sum) *_pa *_empl* *_u, by(country_name)

gen empl_ann_growth=delta_empl_ann/bl_empl
foreach t in bl el {
	gen `t'_u_rate=`t'_u/(`t'_u+`t'_empl)
	gen `t'_ag_share=`t'_ag_pa/(`t'_ag_pa+`t'_nonag_pa)
	gen `t'_svc_share=`t'_svc_pa/(`t'_ag_pa+`t'_svc_pa+`t'_mfg_pa)
}

br country empl_ann *u_rate *_share if empl_ann!=0

stop



**** Benin has only .19% annual growth in number employed












****  First load up sample we have population data on


use "$build/Africa/GPW\population.dta", clear

drop if ipums_id==1
keep if year==2000

drop year 
***** First, check land use given to built up area:

merge 1:m ipums_id using "${projdir}\data\Build\Africa\MODIS_LC\MODIS_LC_Type1.dta", nogen keep(match)
merge 1:1 ipums_id year  using "${projdir}\data\Build\Africa\MODIS_LC\MODIS_LC_Type2.dta", nogen keep(match)
merge 1:1 ipums_id year using "${projdir}\data\Build\Africa\MODIS_LC\MODIS_LC_Type3.dta", nogen keep(match)
merge 1:1 ipums_id year using "${projdir}\data\Build\Africa\MODIS_LC\MODIS_LC_Type4.dta", nogen keep(match)
merge 1:1 ipums_id year  using "${projdir}\data\Build\Africa\MODIS_LC\MODIS_LC_Type5.dta", nogen keep(match)

sum forest_LC_Type1 crop_LC_Type1 built_LC_Type1

forval i=1/5 {
sum forest_LC_Type1 crop_LC_Type`i' built_LC_Type`i' if year==2001 [aw=total_LC_Type`i']
sum forest_LC_Type1 crop_LC_Type`i' built_LC_Type`i' if year==2020 [aw=total_LC_Type`i']
}

**** Built up area goes from 0.24 % of land cover in 2001 to 0.26% in 2020