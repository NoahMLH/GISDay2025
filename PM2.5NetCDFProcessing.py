import arcpy
import os

#Code created by Noah Holderbaum - noahmlholderbaum@gmail.com @NoahMLH
#This is designed to handle .nc files from https://sites.wustl.edu/acag/surface-pm2-5/
#Input are raw NetCDF files downloaded from site above
#Code is designed to handle data for dust, black carbon, NH4, NO3, organic matter, SO4, Sea Salt, and 2.5 diameter particulate matter
#for the years 2000, 2010, and 2020. And for the study areas of Phoenix, Arizona and Houston, Texas
#Code could be easily adapted to fit other years and cities 

filePath = #Set this to location of unprocessed air pollution NetCDF files

phoenixStudyArea = #Set this to location of phoenix shapefile, preferably projected to albers equal area
houstonStudyArea = #Set this to location of houston shapefile, preferably projected to albers equal area

#The templates came from copying python code from the make multidimensional raster tool in ArcGIS Pro
#If using the same study areas I used, posted on github, these should not need to be changed
phoenix_template = '-113.711879688006 32.1177127547459 -109.752359460592 34.6779907189008 GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]'
houston_template = '-96.6285637465374 28.8141226584743 -94.3267085217256 30.9092401063962 GEOGCS["GCS_North_American_1983",DATUM["D_North_American_1983",SPHEROID["GRS_1980",6378137.0,298.257222101]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]]'
#Sets spatial reference number to project end result rasters to Albers Equal Area
albers = arcpy.SpatialReference(5070)

fileList =  os.listdir(filePath)
    
gdb_path = #Set to the location of the geodatabse you would like the results to be stored in. Maybe the gdb to be used for this whole project?
arcpy.env.workspace = gdb_path
#Sets overwrite to be true so this can be ran multiple times if needed
arcpy.env.overwriteOutput = True

#Removes previous GWR rasters
#Only uncomment the next two lines if you are trying to run the code again and getting errors
# for r in arcpy.ListRasters("*GWR*"):
#     arcpy.management.Delete(r)


#Main loop to perform processing on each input NetCDF file
for i in range(len(fileList)):
    arcpy.env.overwriteOutput = True
    fullPath = os.path.join(filePath, fileList[i])
    desc = arcpy.Describe(fullPath)
    fileName = desc.baseName
    variable = None
    year = None
    fileType = desc.extension
    #Skips anything that is not a NetCDF
    if fileType != "nc":
        continue
   #Sets variable to be used in processing based on the file name
    if "DUST-DUST" in fileName:
        variable = "GWRDUST"
    if "BC-BC" in fileName:
        variable = "GWRBC"
    if "NH4-NH4" in fileName:
        variable = "GWRNH4"
    if "NO3-NO3" in fileName:
        variable = "GWRNO3"
    if "OM-OM" in fileName:
        variable = "GWROM"
    if "SO4-SO4" in fileName:
        variable = "GWRSO4"
    if "SS-SS" in fileName:
        variable = "GWRSS"
    if "PM25" in fileName:
        variable = "GWRPM25"
    if "2000001-2000364" in fileName:
        year = "2000"
    if "2010001-2010364" in fileName:
        year = "2010"
    if "2020001-2020364" in fileName:
        year = "2020"

    #Dynamically sets output name using variable and year
    #Raster file names are limited to 13 characters
    #All NetCDF files processed using this code will follow this naming convention
    #GWR (input variable name) (year) (P/H)
    #Example: GWRPM252000H being 2.5 diameter particulate matter for the year 2000 in Houston
    #temp_ layer will be removed and is just used as a temporary intermediate layer
    phoenix_layer = f"temp_{variable}{year}P"
    phoenix_final = os.path.join(gdb_path, f"{variable}{year}P")

   #Performs the actual NetCDF to raster conversion
   #All inputs have been previously set by variables in this code
    phoenixRaster = arcpy.md.MakeMultidimensionalRasterLayer(
       in_multidimensional_raster=fullPath,
       out_multidimensional_raster_layer=phoenix_layer,
       variables=variable,
       dimension_def="ALL",
       template=phoenix_template
   )
    #Deletes the output file if it already exsists
    #Mainly to prevent overwrite errors if running the code multiple times
    if arcpy.Exists(phoenix_final):
        arcpy.management.Delete(phoenix_final)
    #Projects the new raster. It is saved in the process
    arcpy.management.ProjectRaster(phoenix_layer, phoenix_final, albers)
    print(phoenix_final, "created")
    
    #Deletes temporary layer
    arcpy.management.Delete(phoenix_layer)

    #Repeats above code but for Houston
    #This is done seperately for both cities to ensure rasters for both cities are created for each input NetCDF
    houston_layer = f"temp_{variable}{year}H"

    houston_final = os.path.join(gdb_path, f"{variable}{year}H")

    houstonRaster = arcpy.md.MakeMultidimensionalRasterLayer(
       in_multidimensional_raster=fullPath,
       out_multidimensional_raster_layer=houston_layer,
       variables=variable,
       dimension_def="ALL",
       template=houston_template
   )
    if arcpy.Exists(houston_final):
        arcpy.management.Delete(houston_final)

    arcpy.management.ProjectRaster(houston_layer, houston_final, albers)


    arcpy.management.ProjectRaster(houston_layer, houston_final, albers)
    print(houston_final, "created")
    arcpy.management.Delete(houston_layer)
    