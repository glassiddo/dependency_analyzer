# -*- coding: utf-8 -*-
"""
##########################################################################################################
# Name:         AGREGATING DATA ON CROP PRODUCTION VALUE ACROSS ADMIN UNITS OF INTEREST   
# Author:       Kenneth Houngbedji
# Date:         25/02/2024
# Copyright:   (c) IRD 2024
# Licence:     GNU Public Licence (GPL)
##########################################################################################################
"""

"""
############
# PREAMBLE #
############
"""

import glob
import fnmatch
import sys
import processing
import subprocess
import getpass
import gdal  # raster management
import ogr  # vector managment
import numpy as np  # gridded data
import os  # file managment
import csv  # for saving results in csv format

# modules QGIS 

from PyQt5 import *
from qgis.core import *
from qgis.core import QgsVectorLayer, QgsFeature, QgsGeometry, QgsProject,  QgsVectorFileWriter, QgsVectorDataProvider, QgsField
from qgis.PyQt.QtCore import QVariant
from PyQt5.QtCore import QFileInfo
from qgis.analysis import QgsZonalStatistics, QgsRasterCalculator, QgsRasterCalculatorEntry, QgsNativeAlgorithms
from processing.core.Processing import Processing

Processing.initialize()
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

"""
#############################
# SETTING WORKING DIRECTORY #
#############################
"""

# getting username

username = getpass.getuser().lower()

# Checking current working directory

print("Current Working Directory " , os.getcwd())

# Setting appropriate working directory depending on user

if username == 'houngbedji':
    subprocess.call(["subst","X:","/d"])
    subprocess.call(["subst","X:",r"C:/Users/houngbedji/Dropbox (IRD)"])
    wdpath = 'X:/PROJECTS/MIGRATION/WORK'

# Changing working directory and dealing with errors

try:
    # Change the current working Directory    
    os.chdir(wdpath)
    print("Directory changed")

except OSError:
    print("Can't change the Current Working Directory")        
    
"""
#####################################################################
# FUNCTION TO LIST ALL THE FILES WITH A GIVEN EXTENSION IN A FOLDER #
#####################################################################
"""

def findFiles (path, filter):
    for root, dirs, files in os.walk(path, filter):
        for file in fnmatch.filter(files, filter):
            yield os.path.join (root, file)


"""
###############################################################
# FUNCTION TO SELECT A LIMITED NUMBER OF FEATURES FOR TESTING #
###############################################################
"""

def selectLimitedByCount(layer, count):
    request = QgsFeatureRequest().setFlags(QgsFeatureRequest.NoGeometry)
    request.setNoAttributes()  # We only go for feature ids
    request.setLimit(count)
    layer.select([f.id() for f in layer.getFeatures(request)])

"""
############################################
# SHAPEFILE OFADMIN UNITS ACROSS COUNTRIES #
############################################
"""

# Loading vector layer with the polygons 

QgsProject.instance().removeAllMapLayers()

vfile  = 'C:/USERS/HOUNGBEDJI/DROPBOX (COMPTE PERSONNEL)/MIGRATION AFRICA/OUTPUT/MAPS/ADMROI.shp'
vinfo = QtCore.QFileInfo(vfile)
vpath = vinfo.filePath()
vname = vinfo.baseName()
vlayer = QgsVectorLayer(vfile, vname, "ogr") 
vlayer.isValid()
QgsProject.instance().addMapLayer(vlayer)

nbfts = vlayer.featureCount()
print('Number of features :', nbfts)

"""
#######################################################
# CREATING A COPY OF THE SHAPEFILE THAT WE CAN MODIFY #
#######################################################
"""

selectLimitedByCount(iface.activeLayer(), nbfts)
#selectLimitedByCount(iface.activeLayer(), 100)

save_options = QgsVectorFileWriter.SaveVectorOptions()
save_options.driverName = "ESRI Shapefile"
save_options.fileEncoding = "UTF-8"
transform_context = QgsProject.instance().transformContext()
wnvfile = QgsVectorFileWriter.writeAsVectorFormat(vlayer,
                                                  'X:/PROJECTS/MIGRATION/WORK/IPUMSAFRADM.shp',
                                                  "UTF-8", 
                                                  vlayer.crs(), 
                                                  "ESRI Shapefile",
                                                  onlySelected=True)
if wnvfile[0] == QgsVectorFileWriter.NoError:
    print("Layer copied!")
else:
  print(wnvfile)
  
# remove the original file and load the copy

QgsProject.instance().removeMapLayer(vlayer)

nvfile  = 'X:/PROJECTS/MIGRATION/WORK/IPUMSAFRADM.shp'
nvinfo = QtCore.QFileInfo(nvfile)
nvpath = nvinfo.filePath()
nvname = nvinfo.baseName()
nvlayer = QgsVectorLayer(nvfile, nvname, "ogr") 
nvlayer.isValid()
QgsProject.instance().addMapLayer(nvlayer)

"""
#########################################
# CROP DATA GROM GAEZ IN 2000  AND 2010 #
#########################################
"""

# Loading raster layers and computing the sum across admin units

rfolder= 'X:/PROJECTS/MIGRATION/INPUT/GAEZ/RES05/'

# Inside the loop that iterates through raster files
for rfile in findFiles (rfolder, '*.tif'):
    rinfo = QtCore.QFileInfo(rfile)
    rpath = rinfo.filePath()
    rname = rinfo.baseName()
    prfx = rname[0:3] + rname[4:8]
    # Load raster
    rlayer = QgsRasterLayer(rfile, 'raster')
    rlayer.isValid()
    
    # COMPUTING ZONAL STATISTICS
    # Calculate both the sum and count of pixels within each administrative unit
    zoneStat = QgsZonalStatistics(nvlayer, rlayer, prfx, 1, QgsZonalStatistics.Count | QgsZonalStatistics.Sum)
    statResult = zoneStat.calculateStatistics(None)
    
    # After calculating, proceed to check for the new fields and rename them
    fields = nvlayer.fields()
    countFieldIdx = fields.lookupField(f"{prfx}cou")
    sumFieldIdx = fields.lookupField(f"{prfx}sum")
    
    # Proceed with field renaming if the fields exist
    if countFieldIdx != -1 and sumFieldIdx != -1:
        nvlayer.startEditing()
        nvlayer.dataProvider().renameAttributes({countFieldIdx: f"{prfx}obs", sumFieldIdx: f"{prfx}val"})
        nvlayer.commitChanges()
    else:
        print(f"Failed to calculate zonal statistics or rename fields for {rname}.")


# Exporting the outcomes as a CSV file

ofile = 'C:/USERS/HOUNGBEDJI/DROPBOX (COMPTE PERSONNEL)/MIGRATION AFRICA/DATA/RAW/AFRICA/GAEZ DATA/' + 'GAEZADMVAL' + '.csv'
wofile = QgsVectorFileWriter.writeAsVectorFormat(nvlayer, ofile, "UTF-8", nvlayer.crs(), "CSV",onlySelected=False)
if wofile[0] == QgsVectorFileWriter.NoError:
    print("CSV created!")
else:
  print(wofile)
# END
