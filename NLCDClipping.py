import arcpy
import os
filePath = #Set to path to folder containing input NLCD Data

    
gdb_path = #Set to path to output geodatabase
arcpy.env.workspace = gdb_path
arcpy.env.overwriteOutput = True

#This is written to assume you have study area tracts saved in the gdb as these names
#This line may need to be modified depending on your setup
phoenixStudyArea = "Phoenix_tract_2010_Albers.shp"
houstonStudyArea = "Houston_tract_2010_Albers.shp"
fileList =  os.listdir(filePath)
albers = arcpy.SpatialReference(5070)

#Main loop that iterates through input file
for i in range(len(fileList)): 
    fullPath = os.path.join(filePath, fileList[i])
    desc = arcpy.Describe(fullPath)
    fileName = desc.baseName
    fileType = desc.extension
    fullPath = os.path.join(filePath, fileList[i])
    #Skips files that are not .tif rasters
    if fileType != "tif":
        continue
    #Pulls year and data type (land cover or impervious surface) from file name
    outYear = fileName[-12:-8]
    outType = fileName[12:18]
    outName = str(outType) + str(outYear)
    in_raster = fileList[i]
    print("Starting Clipping On " + fileName)
    #Sets output path
    phoenix_out = os.path.join(gdb_path, outName + "P")
    #Performs the actual clip
    arcpy.management.Clip(
            in_raster=fullPath,
            out_raster=phoenix_out,
            in_template_dataset=phoenixStudyArea,
            clipping_geometry="ClippingGeometry",
            nodata_value="250"
        )
    print("Clipped Phoenix", fileName)
    
        # Houston clip
    houston_out = os.path.join(gdb_path, outName + "H")
    arcpy.management.Clip(
            in_raster=fullPath,
            out_raster=houston_out,
            in_template_dataset=houstonStudyArea,
            clipping_geometry="ClippingGeometry",
            nodata_value="250"
        )
    print("Clipped Houston", fileName)