import os
import arcpy
arcpy.CheckOutExtension("spatial")
from arcpy import env
from arcpy.sa import *

os.chdir(os.path.dirname(sys.argv[0]))

#basepath="D:/Dropbox/Liam/Migration Africa"
basepath="C:/Users/user-admin/Dropbox/Liam/Migration Africa"

arcpy.env.overwriteOutput=True

for ipums in [1,2]:
    ipums_data= basepath+"/data/Raw/Africa/Shapefiles/IPUMS/level " + str(ipums) + "/AFRADM" + str(ipums) + "_renamed.shp"

    print("2017")
    elec_2017 = "C:/Users/user-admin/Dropbox/Liam/Migration Africa/data/Raw/Africa/Electricity/africagrid20170906existing.geojson"
    out_features = basepath + "/data/Build/Africa/Electricity/electricity_2017.shp"
    arcpy.conversion.JSONToFeatures(elec_2017, out_features, "POLYLINE")
    out_feature_class = basepath + "/data/Build/Africa/Electricity/electricity_2017_int"+ str(ipums)+ ".shp"
    arcpy.analysis.Intersect([out_features,ipums_data], out_feature_class)
    
    
    print("2007")
    out_features = basepath + "/data/Build/Africa/Electricity/electricity_2007.shp"
    elec_2007 = "C:/Users/user-admin/Dropbox/Liam/Migration Africa/data/Raw/Africa/Electricity/aicdall-countries-electricity-transmission-network.geojson"
    arcpy.conversion.JSONToFeatures(elec_2007, out_features, "POLYLINE")
    out_feature_class = basepath + "/data/Build/Africa/Electricity/electricity_2007_int"+ str(ipums)+ ".shp"
    arcpy.analysis.Intersect([out_features,ipums_data], out_feature_class)
    
