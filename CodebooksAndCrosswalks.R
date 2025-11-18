library(ipumsr)
library(dplyr)
library(tibble)
library(janitor)

#Helpful source for creating Regex expressions for grep and text selection
#https://www.datacamp.com/tutorial/regex-r-regular-expressions-guide

#Function to create a codebook for 2010 blocks to 2010 tracts
create2010crosswalk <- function(blockData){
  #Reads input block table
  blockData <- read.csv(blockData)
  if (!"GISJOIN" %in% names(blockData) || !"TRACTA" %in% names(blockData)) {
    stop("Table does not contain both GISJOIN and TRACT columns. Please check data and try again")
  }
  #Creates a crosswalk table
  #Sets blk2020gj and tr2010gj values based off table input
  crosswalk <- blockData %>%
    dplyr::mutate(
      blk2010gj = GISJOIN,
      tr2010gj = substr(GISJOIN, 1, nchar(GISJOIN) - 4)
    ) %>%
    #Only keeps unique rows
    dplyr::distinct(blk2010gj, tr2010gj)
  #Sets all crosswalk weights to 1 (Later functions require a weight field but 2010 tracts are just a sum of 2010 blocks)
  crosswalk$weight <- 1
  return(crosswalk)
  
  
}
#Cleans and renames NHGIS data using a codebook
#Names come directly from codebook and may be a little long but are more readable than the default
renameColumns <- function(table, codebook){
  #Reads data and codebook
  table <- read.csv(table)
  codebook <- read_nhgis_codebook(codebook)
  #Selects the codes from the codebook
  codes <- codebook$var_info
  codes <- as.data.frame(codes)
  #Uses make_clean_names to convert name descriptions into strings that will be allowed as column names
  codes$var_label <- make_clean_names(codes$var_label)
  subsetCodes <- codes[2:1]
  renamedTable <- table |> rename(!!!deframe(subsetCodes))
  renamedTable <- renamedTable %>%
    rename(GISJOIN = gis_join_match_code)
  return(renamedTable)
}
#Filters data to only Phoenix and Houston
#This will need to be modified to extract other cities
exportCities <- function(dataTable, crosswalkTable, city){
  phoenixCounties <- c("Maricopa", "Pinal", "Gila")
  houstonCounties <- c("Austin", "Brazoria", "Chambers", "Fort Bend", "Galveston",
                       "Harris", "Liberty", "Montgomery", "San Jacinto", "Waller")
  if (city == "Phoenix"){
    counties <- phoenixCounties
  }
  else if (city == "Houston"){
    counties <-  houstonCounties
  }
  countyCol <- grep("county", names(dataTable), ignore.case = TRUE, value = TRUE)[1]
  dataTable[[countyCol]] <- gsub(" County$", "", dataTable[[countyCol]], ignore.case = TRUE)
  dataTable[[countyCol]] <- trimws(dataTable[[countyCol]])
  dataTable[[countyCol]] <- tools::toTitleCase(tolower(dataTable[[countyCol]]))
  filteredTable <- dataTable[dataTable[[countyCol]] %in% counties, ]
  regexStatement <- "blk20(00|10|20)gj" #Looks for blk2000gj, blk2010gj, or blk2020gj
  blockField <- grep(regexStatement, names(dataTable), value = TRUE)
  blockNames <- filteredTable[["GISJOIN"]]
  blockFieldCrosswalk <- grep(regexStatement, names(crosswalkTable), value = TRUE)
  filteredCrosswalk <- crosswalkTable[crosswalkTable[[blockFieldCrosswalk]] %in% blockNames, ]

  
  
  return(list(filteredTable = filteredTable,
              filteredCrosswalk = filteredCrosswalk))
  
}
#Converts block based data to tracts using a crosswalk file
blocksToTracts <- function(table, crosswalks){
  regexStatement <- "blk20(00|10|20)gj" #Looks for blk2000gj, blk2010gj, or blk2020gj
  joinField <- grep(regexStatement, names(crosswalks), value = TRUE)[1]
  table$GISJOIN <- as.character(table$GISJOIN)
  crosswalks[[joinField]] <- as.character(crosswalks[[joinField]]) #converts to character to match common GISJOIN formatting
  #Joins the data table and crosswalk
  joinedData <- left_join(
    table,
    crosswalks,
    by = c("GISJOIN" = joinField)
  )
  #Finds the tract ID column
  tractID <- grep("tr20(00|10|20)gj", names(crosswalks), value = TRUE)[1]
  weightField <- grep("weight", names(crosswalks), value = TRUE)[1]
  
  #Long long list of ID columns that will be excluded from weighting and crosswalk processing
  idCols <- c(
    "GISJOIN", "data_file_year", "file_identification", "region", "division",
    "state_name", "state_code", "county_name", "county_code",
    "county_subdivision_code", "fips_county_subdivion_class_code",
    "place_code", "fips_place_class_code", "place_description_code",
    "census_tract_code", "block_group_code", "block_code",
    "consolidated_city_fips", "american_indian_area_alaska_native_area_hawaiian_home_land_code",
    "fips_american_indian_area_alaska_native_area_hawaiian_home_land_class_code",
    "american_indian_tribal_subdivision_census", "metropolitan_statistical_area_consolidated_metropolitan_statistical_area",
    "consolidated_metropolitan_statistical_area", "metropolitan_area_central_city_indicator",
    "primary_metropolitan_statistical_area", "new_england_county_metropolitan_area",
    "new_england_county_metropolitan_area_central_city_indicator", "congressional_district_106th",
    "state_legislative_district_upper_chamber", "state_legislative_district_lower_chamber",
    "voting_district", "voting_district_indicator", "zip_code_tabulation_area_3_digit",
    "zip_code_tabulation_area_5_digit", "area_name_legal_statistical_area_description_lsad_term_part_indicator",
    "functional_status_code", "internal_point_latitude", "internal_point_longitude",
    "legal_statistical_area_description_code", "school_district_elementary",
    "school_district_secondary", "school_district_unified", "traffic_analysis_zone",
    "metropolitan_area_central_city", tractID, weightField, "parea", "tr2010ge", "blk2000ge", "area_land", "area_water" ,"",
    "geographic_code_identifier","region_code","division_code","state_fips_code","county_fips_code","county_subdivision_fips_code","subminor_civil_division_fips_code",
    "consolidated_city_fips_code","place_fips_code","american_indian_area_alaska_native_area_hawaiian_home_land_census_code",
    "american_indian_area_alaska_native_area_reservation_or_statistical_entity_only_code","american_indian_area_off_reservation_trust_land_only_hawaiian_home_land_code",
    "american_indian_tribal_subdivision_census_code","alaska_native_regional_corporation_fips_code","metropolitan_statistical_area_micropolitan_statistical_area_code",
    "metropolitan_micropolitan_indicator","combined_statistical_area_code","metropolitan_division_code","new_england_city_and_town_area_code","necta_metropolitan_micropolitan_indicator",
    "combined_new_england_city_and_town_area_code","new_england_city_and_town_area_division_code","urban_area_code","congressional_district_116th_code","state_legislative_district_upper_chamber_2018_code",
    "state_legislative_district_lower_chamber_2018_code","zip_code_tabulation_area_5_digit_code","school_district_elementary_code","school_district_secondary_code","school_district_unified_code",
    "public_use_microdata_area","area_base_name","urban_growth_area","blk2020ge", "region_code","division_code","consolidated_city_code","american_indian_area_alaska_native_area_reservation_or_statistical_entity_only_code",
    "american_indian_area_off_reservation_trust_land_only_hawaiian_home_land_code","tribal_subdivision_remainder_code","american_indian_tribal_subdivision_fips","alaska_native_regional_corporation_code",
    "metropolitan_statistical_area_micropolitan_statistical_area_code","metropolitan_division_code","combined_statistical_area_code","new_england_city_and_town_area_code","new_england_city_and_town_area_division_code",
    "combined_new_england_city_and_town_area_code","urban_area_code","congressional_district_111th_congress_code","state_legislative_district_upper_chamber_code","state_legislative_district_lower_chamber_code",
    "x5_digit_zip_code_tabulation_area_code","subminor_civil_division_code","school_district_elementary_remainder_code","school_district_secondary_remainder_code","school_district_unified_remainder_code",
    "metropolitan_micropolitan_indicator","necta_metropolitan_micropolitan_indicator","public_use_microdata_area"

    
  )
  #Selects the actual data columns (Data minus id columns)
  dataCols <- names(joinedData)[sapply(joinedData, is.numeric) & !(names(joinedData) %in% idCols)]

  #Performs weighting and summing calculations
  weightedData <- joinedData %>%
    group_by(.data[[tractID]]) %>%
    summarize(
      across(all_of(dataCols),
             ~ sum(.x * .data[[weightField]], na.rm = TRUE)),
    ) %>%
    ungroup()
  weightedData %>%   select(- any_of (idCols))
  names(weightedData)[names(weightedData) == tractID] <- "GISJOIN"
  
  return(weightedData)
}

#Function that does the full processing using the functions created above
nhgisDataProcessing <- function(tableData, codebook, crosswalk, city, outputFileName, outputDir = getwd()){
  #Renames columns and adds them to a new variable
  renamedTable <- renameColumns(tableData, codebook)
  if (is.character(crosswalk)) {
    crosswalk <- read.csv(crosswalk)
  }
  #Clips the data to just the counties being used in our study
  filteredData <- exportCities(renamedTable, crosswalk, city)
  #Converts the data to tract level
  tractData <- blocksToTracts(filteredData$filteredTable, filteredData$filteredCrosswalk)
  
  outputPath <- file.path(outputDir, paste0("tract_data_", outputFileName, ".csv"))
  #Creates a new .csv with the processed data
  write.csv(tractData, outputPath)
  return(tractData)
}

#Runs the function for each dataset. This will need to be modified if using different data. 
nhgisDataProcessing("nhgis0021_ds147_2000_block.csv", 
                    "nhgis0021_ds147_2000_block_codebook.txt",
                    "nhgis_blk2000_tr2010_04.csv",
                    "Phoenix",
                    "phoenix2000")
nhgisDataProcessing("nhgis0021_ds147_2000_block.csv", 
                    "nhgis0021_ds147_2000_block_codebook.txt",
                    "nhgis_blk2000_tr2010_48.csv",
                    "Houston",
                    "houston2000")
nhgisDataProcessing("nhgis0021_ds258_2020_block.csv",
                    "nhgis0021_ds258_2020_block_codebook.txt",
                    "nhgis_blk2020_tr2010_04.csv",
                    "Phoenix",
                    "phoenix2020")
nhgisDataProcessing("nhgis0021_ds258_2020_block.csv",
                    "nhgis0021_ds258_2020_block_codebook.txt",
                    "nhgis_blk2020_tr2010_48.csv",
                    "Houston",
                    "houston2020")
nhgisDataProcessing("nhgis0021_ds172_2010_block.csv",
                    "nhgis0021_ds172_2010_block_codebook.txt",
                    create2010crosswalk("nhgis0021_ds172_2010_block.csv"),
                                        "Phoenix", "Phoenix2010")
nhgisDataProcessing("nhgis0021_ds172_2010_block.csv",
                    "nhgis0021_ds172_2010_block_codebook.txt",
                    create2010crosswalk("nhgis0021_ds172_2010_block.csv"),
                    "Houston", "Houston2010")
