if c(username) == "liamw"{
	global dir = "D:\Dropbox\Liam\"
}
	
global subdir = "$dir/Migration Africa/data/Hansen Forest Loss"
global dirinput = "$subdir/Input"
global dirtemp = "$subdir/Temp"


import delimited using "D:\Dropbox\Liam\Migration Africa\data\Hansen Forest Loss\Input\Hansen_SA_30.csv", clear

keep mn* tcloss*

fastreshape long tcloss, i(mn_code mn_code_st mn_mdb_c mn_name mn_name_c) j(year)

compress
saveold "$dirtemp/lossbyyear.dta", replace

import delimited using "D:\Dropbox\Liam\Migration Africa\data\Hansen Forest Loss\Input\TC_SA_302000.csv", clear

merge 1:1 mn_code mn_code_st mn_mdb_c mn_name mn_name_c using "$dirtemp/lossbyyear.dta", nogen 


compress
saveold "$diroutput/lossbyyear30_Uganda.dta", replace


