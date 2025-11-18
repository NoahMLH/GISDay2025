import arcpy
import arcpy.sa
import os
print("Imported arcpy")

#Function that calculates zonal statistics and joins the results back to the input tracts
def zonalStatsJoin (inputFile, outputName):
    desc = arcpy.Describe(inputFile)
    fileName = desc.baseName
    if fileName[-1] == "P":
        #This input name is the study area shapefile in the geodatabase
        #Might need to be changed if your gdb is set up differently
        inputZone = "Phoenix_tract_2010_Albers"
    elif fileName[-1] == "H":
        inputZone = "Houston_tract_2010_Albers"
    else:
        return
    #Runs on temporary files from earlier code if needed. These files should be deleted once
    #real copies are saved in earlier steps but this allows them to be used if needed
    if fileName.startswith("temp_"):
        fileName = fileName.replace("_temp", "")
    
    #Calculates zonal statistics
    #Set to ALL Stats but could be modified as needed
    outZonalStats = arcpy.sa.ZonalStatisticsAsTable(
        in_zone_data= inputZone,
        zone_field="GISJOIN",
        in_value_raster=inputFile,
        out_table= "zonalTable" + str(fileName),
        statistics_type= "ALL"
    )
    print("Zonal statistics found for " + str(fileName))
    #Creates a copy of the tracts
    newTable = arcpy.management.CopyFeatures(inputZone, "Zones_" + outputName)
    #Joins zonal stats results to copy of tracts
    joinedData = arcpy.management.JoinField(
    in_data = newTable,
    in_field = "GISJOIN",
    join_table = outZonalStats,
    join_field = "GISJOIN")
    output_shp = outputName + ".shp"
    arcpy.management.CopyFeatures(joinedData, output_shp)
    print(str(fileName) + " joined sucessfully")

#Function to be used only on land cover data to process each urban class seperately

def urbanPercents(inputFile):
    desc = arcpy.Describe(inputFile)
    fileName = desc.baseName
    
    #Reclassifies NLCD land cover to create the following layers:
            #All urban land cover (Classes 21, 22, 23, 24)
            #Class 21 urban
            #Class 22 urban
            #Class 23 urban
            #Class 24 urban
    allUrbanMap = arcpy.sa.RemapRange([[11,12,0], [21,24,1], [31,95,0]])
    class21Map = arcpy.sa.RemapRange([[11,12,0], [21,21,1], [22,95,0]])
    class22Map = arcpy.sa.RemapRange([[11,21,0], [22,22,1], [23,95, 0]])
    class23Map = arcpy.sa.RemapRange([[11,22,0], [23,23,1], [24,95,0]])
    class24Map = arcpy.sa.RemapRange([[11,23,0], [24,24,1], [25,95,0]])
    outReclassAllUrban = arcpy.sa.Reclassify(inputFile, "Value", allUrbanMap)
    outReclass21Urban =  arcpy.sa.Reclassify(inputFile, "Value", class21Map)
    outReclass22Urban =  arcpy.sa.Reclassify(inputFile, "Value", class22Map)
    outReclass23Urban =  arcpy.sa.Reclassify(inputFile, "Value", class23Map)
    outReclass24Urban =  arcpy.sa.Reclassify(inputFile, "Value", class24Map)
    
    #Saves the output so there are new rasters for each of these 
    city = fileName[-1]
    year = fileName[6:10]
    allUrbanOutput = "AllUrb" + year + city
    class21output = "Class21Urb" + year + city
    class22output = "Class22Urb" + year + city 
    class23output = "Class23Urb" + year + city 
    class24output = "Class24Urb" + year + city 

    outReclassAllUrban.save(allUrbanOutput)
    outReclass21Urban.save(class21output)
    outReclass22Urban.save(class22output)
    outReclass23Urban.save(class23output)
    outReclass24Urban.save(class24output)
    
    #Runs the new rasters through the zonalStatsJoin function that will run zonal statistics and rejoin the results to the tract shapefile
    splitUrban = [outReclassAllUrban, outReclass21Urban, outReclass22Urban, outReclass23Urban, outReclass24Urban]
    splitOutNames = [allUrbanOutput, class21output, class22output, class23output, class24output]
    
    for k in range(len(splitUrban)):
        zonalStatsJoin(splitUrban[k], splitOutNames[k])
    


   


    

    
gdb_path = #Set to previously set geodatabase containing processed air pollution and NLCD data
arcpy.env.workspace = gdb_path
arcpy.env.overwriteOutput = True
rasters = arcpy.ListRasters()
print(rasters)
for raster in rasters:

    if not (raster.endswith("P") or raster.endswith("H")):
        continue  

    if "LndCov" in raster:
        urbanPercents(raster)
    else:
        outFileName = raster + "ZonalStats"
        zonalStatsJoin(raster, outFileName)