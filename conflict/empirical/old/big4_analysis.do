cd "C:\Users\o.vandeneynde\Nextcloud\conflict and protected areas\"

use "data\intermediate\event_for_stata.dta", clear

egen country_id=group( iso_a3 )
egen country_cls_id=group(cls iso_a3 )
egen country_ethnic_id=group(ethnic iso_a3 )
egen ethnic_id=group(ethnic)

gen central=country_group=="Central Africa"
gen east=country_group=="East Africa"
gen south=country_group=="Southern Africa"
gen north=country_group=="North Africa"
gen west=country_group=="West Africa"

egen country_group_id=group(country_group)

gen big5=num_big_game==4 & rhino==1
gen big4=num_big_game>=4
gen big3=num_big_game>=3
gen big2=num_big_game>=2
gen big1=num_big_game>=1


gen log_distance_to_airport_country=log(distance_to_airport_country)
bys country_id: egen distance_to_airport_country_med=median(distance_to_airport_country)
bys country_id: egen distance_to_airport_country_m=mean(distance_to_airport_country)
bys country_id: egen distance_to_airport_country_sd=sd(distance_to_airport_country)
*gen distance_to_airport_country_st=(distance_to_airport_country-distance_to_airport_country_m)/distance_to_airport_country_sd
gen distance_to_airport_country_st=(distance_to_airport_country-distance_to_airport_country_med)


egen distance_to_airport_country_m2=mean(distance_to_airport_country) if west==0 & north==0 & big3==1
egen distance_to_airport_country_sd2=sd(distance_to_airport_country)  if  west==0 & north==0  & big3==1
egen distance_to_airport_country_med2=median(distance_to_airport_country) if west==0 &  north==0 & big3==1
gen distance_to_airport_country_st2=distance_to_airport_country-distance_to_airport_country_med2
*gen distance_to_airport_country_st2=(distance_to_airport_country-distance_to_airport_country_m2)/distance_to_airport_country_sd2


gen dist_dum=distance_to_airport_country_st<0 if distance_to_airport_country_st!=.
gen dist2_dum=distance_to_airport_country_st<0 if distance_to_airport_country_st2!=.
gen big4_dist_dum=big4*dist_dum
gen big4_dist2_dum=big4*dist2_dum
gen conflicts_mean=conflicts_dum
gen fatalities_mean=fatalities_dum

preserve 

keep if year<2020

sort gid year

collapse (first) share_area_protected over* population (mean) fatalities_mean conflicts_mean precipitation avg_distinct_birds_date (max)  any_tree  west mines majority_ethnic_neighbors_1 majority_ethnic_neighbors_0_5 conflicts_dum big4 big4_dist_dum dist_dum  big3 big4_dist2_dum dist2_dum distance_to_airport_country north (sum) total_fatalities total_events , by(gid gid_2deg country_id)
ihstrans avg_distinct_birds_date total_fatalities total_events 
gen  log_pop=log(population)
gen log_distance=log(distance_to_airport_country)


label var big4 "Big 4"
label var big4_dist2_dum  "Big 4 \times \text{Close to airport}"
label var dist2_dum  "Close to airport"


estimates clear
eststo: reghdfe ihs_avg_distinct_birds_date big4_dist2_dum big4 dist2_dum  if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe avg_distinct_birds_date big4_dist2_dum big4 dist2_dum  if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe ihs_total_fatalities big4_dist2_dum big4 dist2_dum  if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe fatalities_mean big4_dist2_dum big4 dist2_dum  if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe ihs_total_events big4_dist2_dum big4 dist2_dum  if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe conflicts_mean big4_dist2_dum big4 dist2_dum  if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
esttab using "others\Tables\main1", keep(big4_dist2_dum big4 dist2_dum)  stats(ymean ysd N N_clust  , labels("\addlinespace Mean DV" "Std Dev DV" "Observations" "Number of Clusters") fmt(3 3 0 0)) fragment nomtitles nonumbers  nowrap booktabs  nogaps  nolines eqlabels("") style(tex) wide label  replace noobs  starlevels("*" 0.10 "**" 0.05 "***" 0.01) collabels(none)  cells(b(star fmt(%9.3f)) se(par))

estimates clear
eststo: reghdfe over10 big4_dist2_dum big4 dist2_dum  if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe log_pop big4_dist2_dum big4 dist2_dum  if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe majority_ethnic_neighbors_0_5 big4_dist2_dum big4 dist2_dum  if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe mines big4_dist2_dum big4 dist2_dum  if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe precipitation big4_dist2_dum big4 dist2_dum  if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe any_tree big4_dist2_dum big4 dist2_dum  if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
esttab using "others\Tables\balance", keep(big4_dist2_dum big4 dist2_dum)  stats(ymean ysd N N_clust  , labels("\addlinespace Mean DV" "Std Dev DV" "Observations" "Number of Clusters") fmt(3 3 0 0)) fragment nomtitles nonumbers  nowrap booktabs  nogaps  nolines eqlabels("") style(tex) wide label  replace noobs  starlevels("*" 0.10 "**" 0.05 "***" 0.01) collabels(none)  cells(b(star fmt(%9.3f)) se(par))

estimates clear
eststo: reghdfe ihs_avg_distinct_birds_date big4_dist2_dum big4 dist2_dum log_pop over5 if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe avg_distinct_birds_date big4_dist2_dum big4 dist2_dum log_pop over5  if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe ihs_total_fatalities big4_dist2_dum big4 dist2_dum log_pop over5 if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe fatalities_mean big4_dist2_dum big4 dist2_dum  log_pop over5 if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe ihs_total_events big4_dist2_dum big4 dist2_dum  log_pop over5 if  west==0 &   north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
eststo: reghdfe conflicts_mean big4_dist2_dum big4 dist2_dum log_pop over5 if  west==0 &  north==0 & big3 , a(country_id) vce(cluster gid_2deg)
	estadd ysumm
esttab using "others\Tables\control", keep(big4 big4_dist2_dum dist2_dum)  stats(ymean ysd N N_clust  , labels("\addlinespace Mean DV" "Std Dev DV" "Observations" "Number of Clusters") fmt(3 3 0 0)) fragment nomtitles nonumbers  nowrap booktabs  nogaps  nolines eqlabels("") style(tex) wide label  replace noobs  starlevels("*" 0.10 "**" 0.05 "***" 0.01) collabels(none)  cells(b(star fmt(%9.3f)) se(par))
