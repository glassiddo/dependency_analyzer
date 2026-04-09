import os
import arcpy
arcpy.CheckOutExtension("spatial")
from arcpy import env
from arcpy.sa import *

os.chdir(os.path.dirname(sys.argv[0]))

basepath="D:/Dropbox/Liam/Migration Africa"

arcpy.env.overwriteOutput=True

for ipums in [1,2]:
    ipums_data= basepath+"/data/Raw/Africa/Shapefiles/IPUMS/level " + str(ipums) + "/AFRADM" + str(ipums) + "_renamed.shp"
    #arcpy.CalculateGeometryAttributes_management(ipums_data, [["tot_area", "AREA_GEODESIC"]], area_unit="HECTARES")

    for m in [0,1,2]:
        print (m)
        wdpa_data = "D:/Data/WDPA/Map " + str(m) + "/WDPA_May2023_Public_shp-polygons.shp"
        out_feature_class = basepath + "/data/Build/Africa/WDPA/map" + str(m) + "_ipums" + str(ipums)
        arcpy.analysis.Intersect([wdpa_data,ipums_data], out_feature_class)
        #arcpy.CalculateGeometryAttributes_management(out_feature_class, [["int_area", "AREA_GEODESIC"]], area_unit="HECTARES")
