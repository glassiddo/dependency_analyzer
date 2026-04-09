if c(username) == "liamw"{
	global dir = "D:\Dropbox\Liam\"
}
	
global subdir = "$dir/Migration Africa/data/Hansen Forest Loss"
global dirinput = "$subdir/Input"
global dirtemp = "$subdir/Temp"


import delimited using "D:\Dropbox\Liam\Migration Africa\data\Hansen Forest Loss\Input\Hansen_global_30.csv", clear

keep adm* tcloss*

fastreshape long tcloss, i(adm0_code adm1_code adm2_code adm0_name adm1_name adm2_name) j(year)

compress
saveold "$dirtemp/lossbyyear.dta", replace

import delimited using "D:\Dropbox\Liam\Migration Africa\data\Hansen Forest Loss\Input\TC_global_302000.csv", clear


merge 1:1 adm0_code adm1_code adm2_code adm0_name adm1_name adm2_name using "$dirtemp/lossbyyear.dta", nogen 


compress
saveold "$diroutput/lossbyyear30_Uganda.dta", replace



