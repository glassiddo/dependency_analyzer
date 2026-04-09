use "C:\Users\o.vandeneynde\Nextcloud\conflict and protected areas\data\intermediate\event_for_stata.dta", clear
egen country_id=group( iso_a3 )
egen country_cls_id=group(cls iso_a3 )
egen country_ethnic_id=group(ethnic iso_a3 )
egen ethnic_id=group(ethnic )

egen country_gid_5deg_id=group(gid_5deg iso_a3 )
egen country_gid_2deg_id=group(gid_2deg iso_a3 )

ihstrans total_fatalities
gen over100=share_area_protected==1
gen over1=share_area_protected>=0.01
bys gid: egen max_over_50=max(over50)
bys gid: egen max_over10_50=max(over10_50)
bys gid: egen max_over5_50=max(over5_50)
bys gid: egen max_over1_50=max(over1_50)
bys gid: egen max_over20_50=max(over20_50)
bys gid: egen max_over_20=max(over20)
gen over10_50=over10==1 & over50==0
gen over5_50=over5==1 & over50==0
gen over1_50=over1==1 & over50==0
gen over20_50=over20==1 & over50==0


bys gid: egen pop_base=max(population*(year==1997))
bys gid: egen any_bird_watchers=max(avg_distinct_birds_month)
gen bird_watchers_dum=(avg_distinct_birds_month>0) &avg_distinct_birds_month!=.

egen cls_id=group(cls)
gen high_pop=pop_base>50000

bys gid_1deg year: egen PA_gid_1deg_10=max(over10)
bys gid_2deg year: egen PA_gid_2deg_10=max(over10)

gen over50_big_game=over50*big_game
gen over50_no_big_game=over50*(big_game==0)

gen central=country_group=="Central Africa"
gen east=country_group=="East Africa"
gen south=country_group=="Southern Africa"
gen north=country_group=="North Africa"
gen west=country_group=="West Africa"

egen country_group_id=group(country_group)


gen forested=forest_cover>20 &forest_cover!=.

did_multiplegt_dyn conflicts_dum gid year over50 if north==0, controls(precipitation) trends_nonparam(country_ethnic_id) placebo(6) effects(13) cluster(gid_10deg)


xtset gid year

reghdfe conflicts_dum over1  precipitation if north==0 & max_over_20, a(gid country_ethnic_id#year) vce(cluster country_id gid_10deg)

reghdfe conflicts_dum over50##high_pop precipitation if north==0, a(gid country_ethnic_id#year c.pop_base#year) vce(cluster country_id)

STOP
