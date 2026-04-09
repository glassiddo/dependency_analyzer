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
    

    chinese_data = "D:/Data/Chinese Finance/chinese.shp"
    out_feature_class = basepath + "/data/Build/Africa/Chinese/chinese_ipums" + str(ipums)
    arcpy.analysis.Intersect([chinese_data,ipums_data], out_feature_class)
    
