/*****************************************************************
PROJECT: 		SSA Enrivonment				
TITLE:			globals.do
AUTHOR: 		Sam Marshall
DATE CREATED:	9/14/2023
LAST EDITED:	9/14/2022
DESCRIPTION: 	
ORGANIZATION:	
				
******************************************************************/

import excel using "${rawdata}/Mines/SPGlobal_Export_9-18-2023_a77f020c-5d4f-49cc-b405-4fb80f56d545.xls", ///
	cellrange(A5:Q3864) firstrow clear
	
rename *, lower

drop if prop_id == .

replace start_up_yr = "" if start_up_yr == "NA"
destring start_up_yr, replace


replace actual_closure_yr = "" if actual_closure_yr == "NA"
destring actual_closure_yr, replace
