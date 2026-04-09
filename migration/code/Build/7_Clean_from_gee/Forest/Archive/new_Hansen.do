*if c(username) == "liamw"{
	global dir = "D:\Dropbox\Liam\"
*}
	
global subdir = "$dir/Migration Africa/data/Hansen Forest Loss"
global dirinput = "$subdir/Input"
global dirtemp = "$subdir/Temp"
global diroutput= "$subdir/Output"

global idvars_SA "mn_code mn_code_st mn_mdb_c mn_name mn_name_c"
*global idvars_Botswana "geolevel1 admin_name"
global idvars_IPUMS1 "cntry_name geolevel1 admin_name"
global idvars_IPUMS2 "cntry_name geolevel2 admin_name"
global idvars_Ghana "geolevel2 admin_name"
global idvars_Uganda "v2 v3 v4 v5 v6 d"
global idvars_global "adm0_code adm1_code adm2_code adm0_name adm1_name adm2_name"

*foreach country in  SA Uganda "global" {
*foreach country in  Botswana {
/*
foreach country in  IPUMS1 IPUMS2 {
	disp "`country'"
	qui {
	import delimited using "D:\Dropbox\Liam\Migration Africa\data\Hansen Forest Loss\Input\Hansen_`country'_30.csv", clear
	keep ${idvars_`country'} tcloss*

	*fastreshape long tcloss, i( ${idvars_`country'}) j(year)

	gcollapse (sum) tcloss*, by( ${idvars_`country'})
	compress
	saveold "$dirtemp/lossbyyear.dta", replace

	import delimited using "D:\Dropbox\Liam\Migration Africa\data\Hansen Forest Loss\Input\TC_`country'_302000.csv", clear

	ren sum tc2000
	
	gcollapse (sum) tc2000, by( ${idvars_`country'})
	merge 1:1  ${idvars_`country'} using "$dirtemp/lossbyyear.dta", nogen 
	
	
	
	
	compress
	saveold "$diroutput/lossbyyear30_`country'.dta", replace

	
	}
	*assert (tcloss_share>=0 & tcloss_share<=1) | tc2000==0
	
}
*/
foreach country in  IPUMS1 IPUMS2 {
	disp "`country'"
	qui {
		
	import delimited using "D:\Dropbox\Liam\Migration Africa\data\Hansen Forest Loss\Input\simple_Hansen_`country'_30.csv", clear
	keep ${idvars_`country'} tcloss*

	*fastreshape long tcloss, i( ${idvars_`country'}) j(year)
	foreach var in  admin_name {
	cap	recast str1000 `var'
		
	}
compress
	collapse (sum) tcloss*, by( ${idvars_`country'})
	
	saveold "$dirtemp/lossbyyear_simple.dta", replace

	import delimited using "D:\Dropbox\Liam\Migration Africa\data\Hansen Forest Loss\Input\TC_simple_`country'_302000.csv", clear

	foreach var in  admin_name {
	cap	recast str1000 `var'
		
	}
	ren sum tc2000_simple
	
	
	collapse (sum) tc2000_simple, by( ${idvars_`country'})
	merge 1:1  ${idvars_`country'} using "$dirtemp/lossbyyear_simple.dta", nogen 
	
	
	
	
	compress
	saveold "$diroutput/simple_Hansen_`country'.dta", replace

	
	}
	*assert (tcloss_share>=0 & tcloss_share<=1) | tc2000==0
	
}
