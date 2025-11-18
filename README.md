# GISDay 2025
Code and data for poster presentation at the University of Oklahoma's 2025 GIS Day hosted by the Center for Spatial Analysis. Extension of a Provost’s Summer UReCA Fellowship research project covering air pollution, demographics, and urbanization in Houston, Texas and Phoenix, Arizona.

## GIS Day Data Processing and Analysis Steps
This document aims to explain how to set up and run code to replicate results
1. Start by downloading data. This can come from the original sites or this link https://mega.nz/folder/aIMUwLoR#QmUha17suBDv65KZMODE_A. Needed data is as follows:
    1. Air pollution in NetCDF format from https://sites.wustl.edu/acag/surface-pm2-5/ 
    For this study, scroll down to the North American Regional Estimates with Composition (V5.NA.05 / V5.NA.05.02) model
    and click the NetCDF button under "Annual, monthly, and biweekly mean total PM2.5 [ug/m3] at 0.01° × 0.01°" and 
    "Annual, monthly, and biweekly mean component PM2.5 [ug/m3] at 0.01° × 0.01°". Download annual data for each variable for the year 2000, 2010, and 2020.

    2. NLCD Data from https://www.mrlc.gov/data. Under products select Annual NLCD to reduce the overall number of options. Select "Land Cover (CONUS)" for the years 2000, 2010, and 2020. Make sure to download land cover and not land cover change or land cover confidence. Additionally, download "Fractional Impervious Surface (CONUS)" for 2000, 2010, and 2020. Make sure to download fractional impervious surface and not Impervious Descriptor. 

    3. Download desired census variables from https://www.nhgis.org/. This will require an account. Code in this project is designed for block level input data. 

3. Create a project in ArcGIS Pro to use to view results. You could also create a geodatabase separately to use to hold results if wanted.

4. Run PM2.5NetCDFProcessing.py. Modify code to set input folder containing NetCDF files. Output gdb should be the gdb created as part of the ArcGIS Pro project created in step 2. 

5. Run NLCDClipping.py. Modify code to set input folder

6. Run ZonalStatsJoin.py. Modify code to set geodatabase

7. Check (probably easiest in ArcGIS Pro) that output zonal statistics tables have the same number of rows as input tables. If using study area shapefiles from Github repository this should be 1007 for Phoenix and 1073 for Houston. If these numbers do not match up, go through the steps at the bottom of this page under "Zonal Statistics Workarounds". 


8. Now we switch to R for more tabular processing. Export zonal statistics tables from geodatabase and store them in one folder. The best way I have found to do this is the Batch Export Table tool in ArcGIS Pro. Set the file path outside of the gdb and add .csv at the end. Something like this 
\Documents\ZonalStatsTables\%Name%.csv

9. Install and load needed libraries in R. This can be done as they are encountered but here are all of the ones that will be used.
    stringr
    dplyr
    sf
    GWmodel
    spdep
    ipumsr
    tibble
    janitor
    factoextra
    cluster
    devtools
    GISTools
    Rtools

10. Set working directory by session > set working directory > Choose Directory... > Select folder containing NHGIS data

11. Run CodebooksAndCrosswalks.R. Calls at the bottom may need to be changed if not using data from Github repository. By default, the processed data will stay in this folder and will need to be moved later

Some info on how codebooks and crosswalks work:
    Codebooks are .txt files that come with census data when downloaded through NHGIS that explain what each column is as well as information about survey forms and what level the data was collected on. When you first open the actual data in the .csv file, the column names are codes that are not understandable. The code in this step automatically reads and sets the column names based off of the codebook. Some of the column names may be worded a little odd or be very long names but they are more readable. 
    Crosswalks are tables that allow us to convert census data to the same boundaries. Census boundaries are changed for each decennial census. This causes a minor issue when working with multiple years of data. For this project we go from block data to tract data so the explanation will be targeted toward this. However, it is important to note that crosswalks exist for almost any combo of geographies.
    
    Crosswalks contain a list of each input block, a weight, and the tract that they are in using 2010 tract boundaries.  
    This allows us to take block level observations, weight them to represent what portion of the block ends up in the 2010 tract,  
    and then sum these weighted values to determine the count for that tract. Here is an example using made up data:
    Let's say we have the following information in our cross walk:
        Block Name: A   Weight: 1.00    Count: 27   Tract Name: 1
        Block Name: B   Weight: 0.75    Count: 45   Tract Name: 1
        Block Name: C   Weight: 0.49    Count: 18   Tract Name: 1
        Block Name: D   Weight: 0.33    Count: 70   Tract Name: 2

    All of block A ends up in tract 1 but only 49% of block c ends up in tract 1. To calculate the count in tract 1,  
    we take each weight and multiply it by its respective count. This gives us something like this: 
        Block Name: A   Weight: 1.00    Count: 27   Tract Name: 1   WeightedCount: 27
        Block Name: B   Weight: 0.75    Count: 45   Tract Name: 1   WeightedCount: 33.75
        Block Name: C   Weight: 0.49    Count: 18   Tract Name: 1   WeightedCount: 8.82
        Block Name: D   Weight: 0.33    Count: 70   Tract Name: 2   WeightedCount: 23.1

    To find the total count that we expect in the 2010 tract boundary, we sum by tract name.  
    So tract 1 will contain 27 + 33.75 + 8.82 of our desired variable.  
    This is an estimate but we are able to predict that the count in that tract will be 69.57.  
    Yes, this does mean we have half a person but this is acceptable for our needs and is the   
    best we can do given we want all of our data using the same census tracts. 

11. Set working directory to folder containing zonal statistic tables as exported in step 7. Run ZonalRenameAndJoin.R

12. Create or modify a folder to contain all of the input files created so far. This folder needs to contain the tables created in steps 10 and 11. Set this folder as your working directory.

13. Run ForwardRegression.R

14. Run TestingRegressionModelsOnOtherCities.R if desired. Code is set up to take the highest performing regression model (houston mean organic matter with 2020 air pollution data included as predictor variables) and test it using phoenix data. This is not currently set up as a function to be repeated with other models or data

15. Run ClusteringAndPCA.R. PCA ended up not being used for GIS Day but could create good regression input. Clustering was used for mapping neighborhoods but could also be used as regression input. 




### Zonal Statistics Workarounds
The zonal statistics as table tool in ArcGIS Pro has some known flaws that unfortunately affect this project. This mainly seems to impact the air pollution data instead of the NLCD data. 

If there are missing rows representing tracts that lost data, follow these steps in ArcGIS Pro:
1. Resample the raster. The output needs to be the raster following these naming conventions (to match zonal statistics output and work with later processing code) Examples:
    - AllUrb2000H
    - Class21Urb2000H
    - Class22Urb2000H
    - Class23Urb2000H
    - Class24Urb2000H
    - FctImp2000H
    - GWRBC2000H
    - GWRDUST2000H
    - GWRNH42000H
    - GWRNO32000H
    - GWROM2000H
    - GWRPM252000H
    - GWRSO42000H
    - GWRSS2000H

The output cell size should automatically set to the current cell size. This should be changed to about 1/4 the current cell size. If there continues to be missing rows, this may need to be adjusted to create an even smaller spatial resolution.
2. Rerun zonal statistics as table. This can be done either using the .py code or in ArcGIS Pro

3. If there are still missing rows: verify that the raster and the zone data (tracts) have the same projected coordinate system and geographic coordinate system. This will need to be corrected if they do not. The layers may line up in ArcGIS Pro due to on the fly projection but are not actually lining up correctly. Also check (visually) that the raster and zone data are as expected. Do they cover the same study area? Does the raster cover all of the zones? Are there large areas of missing data?

4. If you continue to have missing rows: try converting the shapefile of boundaries to a raster and using it in zonal statistics. Cell size needs to be small (start with at least 1/10 the size of the resampled raster).

5. I have always been able to get the zonal statistics to run as expected after these steps. It may take some trial and error. If it does not work at this point, try redownloading the data or maybe change the goals of the study (exclude tracts or variables)

