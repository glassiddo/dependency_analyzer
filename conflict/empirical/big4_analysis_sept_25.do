******************************************************
* Master do-file: Big 4 vs Big 3 with Airport Interactions
******************************************************

cd "C:\Users\iddo2\Nextcloud2\conflict and protected areas\"

use "data\cln\event_for_stata.dta", clear

******************************************************
* Construct variables
******************************************************

* Conflict variables
gen conflicts_mean    = conflicts_dum 
gen fatalities_mean   = fatalities_dum 
gen battle_ev_mean    = events_battles_dum
gen protests_ev_mean  = events_protests_dum
gen violence_ev_mean  = eve_violence_ag_civ_dum
gen explosion_ev_mean = eve_explosions_dum
gen strageic_ev_mean  = eve_strategic_dev_dum
gen state_ev_mean     = events_inv_state_dum
gen nonstate_ev_mean  = events_non_state_dum
gen jihad_ev_mean     = events_inv_jihad_dum
gen nonjihad_ev_mean  = events_non_jihad_dum

* Generic airport distance (for baseline big4/big5)
egen distance_to_airport_country_mn2 = mean(distance_to_airport_country) if west==0 & north==0 & big3==1
gen distance_to_airport_country_st2  = distance_to_airport_country - distance_to_airport_country_mn2
gen dist2_dum                          = distance_to_airport_country_st2 < 0 if distance_to_airport_country_st2!=.
gen dist2                              = distance_to_airport_country_st2

* Cape buffalo
egen dist_airport_mn2_cape_buffalo = mean(distance_to_airport_country) if west==0 & north==0 & (big3_missing_cape_buffalo==1 | big4_incl_cape_buffalo == 1)
gen dist_airport_st2_cape_buffalo  = distance_to_airport_country - dist_airport_mn2_cape_buffalo
gen dist2_dum_cape_buffalo         = dist_airport_st2_cape_buffalo < 0 if dist_airport_st2_cape_buffalo!=.
gen dist2_cape_buffalo             = dist_airport_st2_cape_buffalo

* Rhino
egen dist_airport_mn2_rhino = mean(distance_to_airport_country) if west==0 & north==0 & (big3_missing_rhino==1 | big4_incl_rhino == 1)
gen dist_airport_st2_rhino  = distance_to_airport_country - dist_airport_mn2_rhino
gen dist2_dum_rhino         = dist_airport_st2_rhino < 0 if dist_airport_st2_rhino!=.
gen dist2_rhino             = dist_airport_st2_rhino

* Leopard
egen dist_airport_mn2_leopard = mean(distance_to_airport_country) if west==0 & north==0 & (big3_missing_leopard==1 | big4_incl_leopard==1)
gen dist_airport_st2_leopard  = distance_to_airport_country - dist_airport_mn2_leopard
gen dist2_dum_leopard         = dist_airport_st2_leopard < 0 if dist_airport_st2_leopard!=.
gen dist2_leopard             = dist_airport_st2_leopard

* Lion
egen dist_airport_mn2_lion = mean(distance_to_airport_country) if west==0 & north==0 & (big3_missing_lion==1 | big4_incl_lion==1)
gen dist_airport_st2_lion  = distance_to_airport_country - dist_airport_mn2_lion
gen dist2_dum_lion         = dist_airport_st2_lion < 0 if dist_airport_st2_lion!=.
gen dist2_lion             = dist_airport_st2_lion

* Elephant
egen dist_airport_mn2_elephant = mean(distance_to_airport_country) if west==0 & north==0 & (big3_missing_elephant==1 | big4_incl_elephant==1)
gen dist_airport_st2_elephant  = distance_to_airport_country - dist_airport_mn2_elephant
gen dist2_dum_elephant         = dist_airport_st2_elephant < 0 if dist_airport_st2_elephant!=.
gen dist2_elephant             = dist_airport_st2_elephant

******************************************************
* Interaction terms (must be created before collapse)
******************************************************
gen big4_dist2_dum            = big4 * dist2_dum
gen big5_dist2_dum            = big5 * dist2_dum
gen big5_dist2                = big5 * dist2
gen big4_dist2_dum_cape_buffalo = big4 * dist2_dum_cape_buffalo
gen big4_dist2_dum_rhino      = big4 * dist2_dum_rhino
gen big4_dist2_dum_leopard    = big4 * dist2_dum_leopard
gen big4_dist2_dum_lion       = big4 * dist2_dum_lion
gen big4_dist2_dum_elephant   = big4 * dist2_dum_elephant

******************************************************
* Collapse data to grid-cell x year
******************************************************
keep if year < 2020
sort gid year

collapse (first) share_area_protected over* population ///
         (mean) conflicts_mean fatalities_mean battle_ev_mean violence_ev_mean state_ev_mean nonstate_ev_mean jihad_ev_mean nonjihad_ev_mean protests_ev_mean ntlla_share precipitation ///
                avg_distinct_birds_date avg_distinct_birds_week avg_distinct_birds_month events_violence_against_civilian events_battles events_non_jihad events_inv_jihad events_non_state events_inv_state ///
         (max) dist2 any_tree west north mines majority_ethnic_neighbors_1 majority_ethnic_neighbors_0_5 ///
               conflicts_dum big4_dist2_dum big5_dist2_dum big3 big4 big5 dist2_dum ///
               dist2_dum_cape_buffalo dist2_dum_rhino dist2_dum_leopard dist2_dum_lion dist2_dum_elephant ///
               big4_dist2_dum_cape_buffalo big4_dist2_dum_rhino big4_dist2_dum_leopard big4_dist2_dum_lion big4_dist2_dum_elephant ///
               cape_buffalo rhino leopard lion elephant ///
               big3_missing_cape_buffalo big3_missing_rhino big3_missing_leopard big3_missing_lion big3_missing_elephant big4_incl_cape_buffalo big4_incl_elephant big4_incl_leopard big4_incl_lion big4_incl_rhino ///
         (sum) total_fatalities total_events , by(gid gid_2deg country_id)

******************************************************
* Transformations
******************************************************
ihstrans avg_distinct_birds_date avg_distinct_birds_week avg_distinct_birds_month total_fatalities total_events
gen log_pop = log(population)

******************************************************
* Labels
******************************************************
label var big4            "Big 4"
label var big5            "Big 5"
label var dist2_dum       "Close to airport"
label var big4_dist2_dum  "Big 4 × Close to airport"
label var big5_dist2_dum  "Big 5 × Close to airport"
label var big4_dist2_dum_cape_buffalo "Big 4 (buffalo) × Close to airport"

******************************************************
* MAIN TABLE: Big4 vs Big3, 6 outcomes
******************************************************
estimates clear

* 1
reghdfe ihs_avg_distinct_birds_date big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
quietly summarize ihs_avg_distinct_birds_date if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo m1

* 2
reghdfe avg_distinct_birds_date big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
quietly summarize avg_distinct_birds_date if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo m2

* 3
reghdfe ihs_total_events big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
quietly summarize ihs_total_events if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo m3

* 4
reghdfe conflicts_mean big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
quietly summarize conflicts_mean if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo m4

* 5
reghdfe ihs_total_fatalities big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
quietly summarize ihs_total_fatalities if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo m5

* 6
reghdfe fatalities_mean big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
quietly summarize fatalities_mean if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo m6

esttab m1 m2 m3 m4 m5 m6 using "others\Tables\main_big4vsbig3.tex", ///
    keep(big4 big4_dist2_dum dist2_dum) ///
    stats(ymean ysd N N_clust, labels("\addlinespace Mean DV" "Std Dev DV" "Observations" "Clusters") fmt(3 3 0 0)) ///
    label style(tex) fragment booktabs nogaps replace noobs ///
    star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
    mtitles("Bird watchers (ihs)" "Bird watchers (share)" "Events (ihs)" "Events (share)" "Fatalities (ihs)" "Fatalities (share)")

******************************************************
* BALANCE TABLE (Big4 vs Big3)
******************************************************

global controls "log_pop majority_ethnic_neighbors_0_5 mines precipitation any_tree"

estimates clear
local i = 1
foreach var in share_area_protected log_pop majority_ethnic_neighbors_0_5 mines precipitation any_tree {
    
    reghdfe `var' big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
    quietly summarize `var' if e(sample)
    estadd scalar ymean = r(mean)
    estadd scalar ysd   = r(sd)
    eststo bal`i'
    local i = `i' + 1
}

esttab bal1 bal2 bal3 bal4 bal5 bal6 using "others\Tables\balance_big4vsbig3.tex", ///
    keep(big4 big4_dist2_dum dist2_dum) ///
    stats(ymean ysd N N_clust, labels("\addlinespace Mean DV" "Std Dev DV" "Observations" "Clusters") fmt(3 3 0 0)) ///
    label style(tex) fragment booktabs nogaps replace noobs ///
    star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
    mtitles("Protected area (0/1)" "Population (log)" "Co-ethnic neighbors (0/1)" "Mining (0/1)" "Precipitation" "Valuable trees (0/1)")

******************************************************
* OTHER CONFLICT OUTCOMES: Big4 vs Big3, 5 outcomes
******************************************************
estimates clear

reghdfe conflicts_mean big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
quietly summarize conflicts_mean if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo oc1

reghdfe battle_ev_mean big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
quietly summarize battle_ev_mean if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo oc2

reghdfe violence_ev_mean big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
quietly summarize violence_ev_mean if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo oc3

reghdfe protests_ev_mean big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
quietly summarize protests_ev_mean if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo oc4

reghdfe state_ev_mean big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
quietly summarize state_ev_mean if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo oc5

reghdfe jihad_ev_mean big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
quietly summarize jihad_ev_mean if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo oc6

reghdfe ntlla_share big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3, a(country_id) vce(cluster gid_2deg)
quietly summarize ntlla_share if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo oc7

esttab oc1 oc2 oc3 oc4 oc5 oc6 oc7 using "others\Tables\various_conflicts_outcomes.tex", ///
    keep(big4 big4_dist2_dum dist2_dum) ///
    stats(ymean ysd N N_clust, labels("\addlinespace Mean DV" "Std Dev DV" "Observations" "Clusters") fmt(3 3 0 0)) ///
    label style(tex) fragment booktabs nogaps replace noobs ///
    star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
    mtitles("Events (share)" "Battles" "Violence ag. civilians" "Protests/Riots" "Inv. state" "Inv. Jihadist" "Nightlight (share)")

******************************************************
* MAIN TABLE: Big5 vs Big4, 6 outcomes
******************************************************
estimates clear

reghdfe ihs_avg_distinct_birds_date big5_dist2_dum big5 dist2_dum if west==0 & north==0 & big4, a(country_id) vce(cluster gid_2deg)
quietly summarize ihs_avg_distinct_birds_date if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo m51

reghdfe avg_distinct_birds_date big5_dist2_dum big5 dist2_dum if west==0 & north==0 & big4, a(country_id) vce(cluster gid_2deg)
quietly summarize avg_distinct_birds_date if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo m52

reghdfe ihs_total_events big5_dist2_dum big5 dist2_dum if west==0 & north==0 & big4, a(country_id) vce(cluster gid_2deg)
quietly summarize ihs_total_events if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo m53

reghdfe conflicts_mean big5_dist2_dum big5 dist2_dum if west==0 & north==0 & big4, a(country_id) vce(cluster gid_2deg)
quietly summarize conflicts_mean if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo m54

reghdfe ihs_total_fatalities big5_dist2_dum big5 dist2_dum if west==0 & north==0 & big4, a(country_id) vce(cluster gid_2deg)
quietly summarize ihs_total_fatalities if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo m55

reghdfe fatalities_mean big5_dist2_dum big5 dist2_dum if west==0 & north==0 & big4, a(country_id) vce(cluster gid_2deg)
quietly summarize fatalities_mean if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo m56

esttab m51 m52 m53 m54 m55 m56 using "others\Tables\main_big5vsbig4.tex", ///
    keep(big5 big5_dist2_dum dist2_dum) ///
    stats(ymean ysd N N_clust, labels("\addlinespace Mean DV" "Std Dev DV" "Observations" "Clusters") fmt(3 3 0 0)) ///
    label style(tex) fragment booktabs nogaps replace noobs ///
    star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
    mtitles("Bird watchers (ihs)" "Bird watchers (share)" "Events (ihs)" "Events (share)" "Fatalities (ihs)" "Fatalities (share)")

******************************************************
* BALANCE TABLE (Big5 vs Big4)
******************************************************
estimates clear
local j = 1
foreach var in share_area_protected log_pop majority_ethnic_neighbors_0_5 mines precipitation any_tree {
    reghdfe `var' big5_dist2_dum big5 dist2_dum if west==0 & north==0 & big4, a(country_id) vce(cluster gid_2deg)
    quietly summarize `var' if e(sample)
    estadd scalar ymean = r(mean)
    estadd scalar ysd   = r(sd)
    eststo bal5_`j'
    local j = `j' + 1
}

esttab bal5_1 bal5_2 bal5_3 bal5_4 bal5_5 bal5_6 using "others\Tables\balance_big5vsbig4.tex", ///
    keep(big5 big5_dist2_dum dist2_dum) ///
    stats(ymean ysd N N_clust, labels("\addlinespace Mean DV" "Std Dev DV" "Observations" "Clusters") fmt(3 3 0 0)) ///
    label style(tex) fragment booktabs nogaps replace noobs ///
    star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
    mtitles("Protected area (0/1)" "Population (log)" "Co-ethnic neighbors (0/1)" "Mining (0/1)" "Precipitation" "Valuable trees (0/1)")

******************************************************
* MAIN TABLE: Big4 vs Big3 (excluding Big5), 6 outcomes
******************************************************
estimates clear

reghdfe ihs_avg_distinct_birds_date big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3==1 & big5==0, a(country_id) vce(cluster gid_2deg)
quietly summarize ihs_avg_distinct_birds_date if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo x1

reghdfe avg_distinct_birds_date big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3==1 & big5==0, a(country_id) vce(cluster gid_2deg)
quietly summarize avg_distinct_birds_date if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo x2

reghdfe ihs_total_events big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3==1 & big5==0, a(country_id) vce(cluster gid_2deg)
quietly summarize ihs_total_events if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo x3

reghdfe conflicts_mean big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3==1 & big5==0, a(country_id) vce(cluster gid_2deg)
quietly summarize conflicts_mean if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo x4

reghdfe ihs_total_fatalities big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3==1 & big5==0, a(country_id) vce(cluster gid_2deg)
quietly summarize ihs_total_fatalities if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo x5

reghdfe fatalities_mean big4_dist2_dum big4 dist2_dum if west==0 & north==0 & big3==1 & big5==0, a(country_id) vce(cluster gid_2deg)
quietly summarize fatalities_mean if e(sample)
estadd scalar ymean = r(mean)
estadd scalar ysd   = r(sd)
eststo x6

esttab x1 x2 x3 x4 x5 x6 using "others\Tables\main_big4vsbig3_excluding_big5.tex", ///
    keep(big4 big4_dist2_dum dist2_dum) ///
    stats(ymean ysd N N_clust, labels("\addlinespace Mean DV" "Std Dev DV" "Observations" "Clusters") fmt(3 3 0 0)) ///
    label style(tex) fragment booktabs nogaps replace noobs ///
    star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
    mtitles("Bird watchers (ihs)" "Bird watchers (share)" "Events (ihs)" "Events (share)" "Fatalities (ihs)" "Fatalities (share)")

******************************************************
* ANIMAL COMPARISON TABLE (Simplified)
******************************************************
estimates clear
foreach animal in cape_buffalo rhino leopard lion elephant {
    reghdfe conflicts_mean big4_dist2_dum_`animal' big4 dist2_dum_`animal' ///
        if (big3_missing_`animal'==1 | (big4==1 & `animal'==1)), a(country_id) vce(cluster gid_2deg)
    
    quietly summarize conflicts_mean if e(sample)
    estadd scalar ymean = r(mean)
    estadd scalar ysd   = r(sd)

    estimates store `animal'
}

* Updated esttab command with mgroups to add animal names as column headers
* and eqlabels(none) to consolidate rows with the same coefficient name
esttab cape_buffalo rhino leopard lion elephant ///
    using "others\Tables\animal_species_comparison.tex", ///
    keep(big4 big4_dist2_dum_* dist2_dum_*) ///
    order(big4 big4_dist2_dum_* dist2_dum_*) ///
    stats(N N_clust, labels("Observations" "Clusters") fmt(0 0)) ///
    label style(tex) fragment booktabs nogaps replace noobs ///
    star(* 0.10 ** 0.05 *** 0.01) b(3) se(3) ///
    mgroups("Cape Buffalo" "Rhino" "Leopard" "Lion" "Elephant", ///
        pattern(1 1 1 1 1) prefix(\multicolumn{1}{c}{) suffix(}) ///
        span erepeat(\cmidrule(lr){@span})) ///
    nomtitles ///
    coeflabels(big4 "Big 4" ///
               big4_dist2_dum_cape_buffalo "Big 4 $\times$ Close to airport" ///
               big4_dist2_dum_rhino        "Big 4 $\times$ Close to airport" ///
               big4_dist2_dum_leopard      "Big 4 $\times$ Close to airport" ///
               big4_dist2_dum_lion         "Big 4 $\times$ Close to airport" ///
               big4_dist2_dum_elephant     "Big 4 $\times$ Close to airport" ///
               dist2_dum_cape_buffalo      "Close to airport" ///
               dist2_dum_rhino             "Close to airport" ///
               dist2_dum_leopard           "Close to airport" ///
               dist2_dum_lion              "Close to airport" ///
               dist2_dum_elephant          "Close to airport")

******************************************************
* End
******************************************************
