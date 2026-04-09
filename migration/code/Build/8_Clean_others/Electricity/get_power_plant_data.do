import delimited "$dropbox\Migration Africa\data\Raw\Africa\Electricity\global_power_plant_database_v_1_3\global_power_plant_database.csv", clear

local l=1

cd "$dropbox\Migration Africa\data\Raw\Africa\Shapefiles\IPUMS\level `l'\"
shp2dta using "AFRADM`l'_reprojected.shp" , data("IPUMS`l'_data.dta") coord("IPUMS`l'_coords.dta") replace

geoinpoly latitude longitude using "IPUMS`l'_coords.dta"

drop if _ID==.
br