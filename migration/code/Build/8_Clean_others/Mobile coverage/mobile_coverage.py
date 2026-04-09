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
    

    for y in [2007, 2008, 2009, 2011, 2012]:
        print (y)
        coverage_data = "D:/Dropbox/Liam/India Irrigation/0_mobile_explorer_data/Input/Mobile Coverage/Global_2G_" + str(y) + "/Global_GSM_" + str(y) + ".shp"
        out_feature_class = basepath + "/data/Build/Africa/GSM/coverage_" + str(y) + "_ipums" + str(ipums)
        arcpy.analysis.Intersect([coverage_data,ipums_data], out_feature_class)
        #arcpy.CalculateGeometryAttributes_management(out_feature_class, [["int_area", "AREA_GEODESIC"]], area_unit="HECTARES")
