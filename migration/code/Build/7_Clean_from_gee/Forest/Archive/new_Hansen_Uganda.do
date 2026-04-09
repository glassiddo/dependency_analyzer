if c(username) == "liamw"{
	global dir = "D:\Dropbox\Liam\"
}
	
global subdir = "$dir/Migration Africa/data/Hansen Forest Loss"
global dirinput = "$subdir/Input"
global dirtemp = "$subdir/Temp"


import delimited using "D:\Dropbox\Liam\Migration Africa\data\Hansen Forest Loss\Input\Hansen_Uganda_30.csv", clear

keep v2 v3 v4 v5 v6 d dname2019 total2020 ttotal2020 tcloss*

fastreshape long tcloss, i(v2 v3 v4 v5 v6 d dname2019 total2020 ttotal2020) j(year)

compress
saveold "$dirtemp/lossbyyear.dta", replace

import delimited using "D:\Dropbox\Liam\Migration Africa\data\Hansen Forest Loss\Input\TC_Uganda_302000.csv", clear

merge 1:1 v2 v3 v4 v5 v6 d dname2019 total2020 ttotal2020 using "$dirtemp/lossbyyear.dta", nogen 


compress
saveold "$diroutput/lossbyyear30_Uganda.dta", replace


