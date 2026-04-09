use "${final}/nightlight.dta", clear


merge 1:1 ipums_id using "${final}/data_merged.dta", nogen keep(match) keepusing(total_area)


replace total_area=total_area/1000

	gen bl_nlarea_ra=bl_nla_ra*total_area
	gen bl_nvarea_ra=bl_nlv_ra*total_area
	gen el_nlarea_ra=bl_nla_ra*total_area+(delta_nla_ra*nl_harm_prd_len)*total_area
	gen el_nvarea_ra=bl_nlv_ra*total_area+(delta_nlv_ra*nl_harm_prd_len)*total_area
	gen delta_log_nlarea_ra=log((el_nlarea_ra+1)/(bl_nlarea_ra+1)^(1/(nl_harm_prd_len)))
	gen delta_log_nvarea_ra=log((el_nvarea_ra+1)/(bl_nvarea_ra+1)^(1/(nl_harm_prd_len)))
	gen delta_nlarea_ra=(el_nlarea_ra-bl_nlarea_ra)/nl_harm_prd_len
	gen delta_nvarea_ra=(el_nvarea_ra-bl_nvarea_ra)/nl_harm_prd_len
	
	drop total_area

save "${final}/nightlight.dta", replace