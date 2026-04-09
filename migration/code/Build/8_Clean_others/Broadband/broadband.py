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
    

    broadband_data = "D:/Dropbox/Liam/Migration Africa/data/Raw/Africa/Broadband/terrestrial.shp"
    out_feature_class = basepath + "/data/Build/Africa/Broadband/broadband_ipums" + str(ipums)
    arcpy.analysis.Intersect([broadband_data,ipums_data], out_feature_class)
    
