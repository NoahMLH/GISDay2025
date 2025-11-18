library(stringr)
library(dplyr)
library(GWmodel)
library(sf)
joinTables <- function(tableList, outputName, outputDir = getwd()) {
  #Function to add city and year to end of each column
  addSuffixFromFilename <- function(data, filename) {
    #Extracts year and city name using RegEX
    year <- str_extract(filename, "20[0-9]{2}")
    city <- str_extract(filename, "(?i)(Houston|Phoenix)")
    
    if (is.na(year)) year <- "NA"
    if (is.na(city)) city <- "Unknown"
    #Creates the suffix that will be added to each column name
    suffix <- paste0("_", year, "_", tolower(city))
    

    new_names <- names(data)
    #For each column name, adds the suffix if it is not the GISJOIN
    for (i in seq_along(new_names)) {
      name <- new_names[i]
      if (name != "GISJOIN" && 
          !grepl(year, name) && 
          !grepl(city, name, ignore.case = TRUE)) {
        new_names[i] <- paste0(name, suffix)
      }
    }
    names(data) <- new_names
    return(data)
  }
  
  #Checks to make sure data contains a GISJOIN and adds the city and year to the end of each column name
  renamedList <- lapply(tableList, function(file) {
    data <- read.csv(file)
    
    if (!"GISJOIN" %in% names(data)) {
      stop(paste("File", file, "is missing a GISJOIN column."))
    }
    
    data <- addSuffixFromFilename(data, file)
    return(data)
  })
  #Merges all input tables
  combinedTable <- Reduce(function(x, y)
    merge(x, y, by = "GISJOIN", all = TRUE), renamedList)
  
  #Exports merged table 
  outputPath <- file.path(outputDir, paste0("combined", outputName, ".csv"))
  write.csv(combinedTable, outputPath, row.names = FALSE)
  return(combinedTable)
}
#Function to create a regression call to be used to create a call with all columns from a table
createRegressionCall <- function(response, predictors){
  as.formula(
    paste(response, "~", paste(names(predictors), collapse = " + "))
  )
}

#Function to automate regression process to run the same steps on all data
#This function is heavily reliant on using either phoenix or houston but could be modified to be more flexible
regression <- function(city, responseVar, withoutAP){
  houstonAll <- c("combined2000HoustonAP.csv", "combined2000HoustonUrb.csv", "combined2010HoustonAP.csv", "combined2010HoustonUrb.csv",
                  "combined2020HoustonUrb.csv", "Demographics2000Houston.csv", "Demographics2010Houston.csv","Demographics2020Houston.csv")
  phoenixAll <- c("combined2000PhoenixAP.csv", "combined2000PhoenixUrb.csv","combined2010PhoenixAP.csv", "combined2010PhoenixUrb.csv",
                  "combined2020PhoenixUrb.csv", "Demographics2000Phoenix.csv", "Demographics2010Phoenix.csv","Demographics2020Phoenix.csv")
  houstonWithoutAP <- c("combined2000HoustonUrb.csv", "combined2010HoustonUrb.csv", "combined2020HoustonUrb.csv", "Demographics2000Houston.csv", "Demographics2010Houston.csv","Demographics2020Houston.csv")
  phoenixWithoutAP <- c("combined2000PhoenixUrb.csv", "combined2010PhoenixUrb.csv", "combined2020PhoenixUrb.csv", "Demographics2000Phoenix.csv", "Demographics2010Phoenix.csv","Demographics2020Phoenix.csv")
  
  #If statements create the data that will go into the regression formula depending on the city, response variable (air pollution variables from 2020), and inclusion of
    #2020 air pollution data as predictors
  
  
  if (city == "Houston" && withoutAP == FALSE){
    combinedData <- joinTables(houstonAll, "houstonAll")
    responseAP <-  read.csv("combined2020HoustonAP.csv")
    tracts <- st_read("Houston_tract_2010_Albers.shp")
    outName <- paste0("HoustonWITH2020AirPollutionPredictors", responseVar, "RegressionOutput.txt")
  }
  else if (city == "Houston" && withoutAP == TRUE){
    combinedData <- joinTables(houstonWithoutAP, "houstonWithoutAP")
    responseAP <-  read.csv("combined2020HoustonAP.csv")
    tracts <- st_read("Houston_tract_2010_Albers.shp")
    outName <- paste0("HoustonNo2020AirPollutionPredictors", responseVar, "RegressionOutput.txt")
    
  }
  else if (city == "Phoenix" && withoutAP == FALSE){
    combinedData <- joinTables(phoenixAll, "phoenixAll")
    responseAP <-  read.csv("combined2020PhoenixAP.csv")
    tracts <- st_read("Phoenix_tract_2010_Albers.shp")
    outName <- paste0("PhoenixWith2020AirPollutionPredictors", responseVar, "RegressionOutput.txt")
    
  }
  else if (city == "Phoenix" && withoutAP == TRUE){
    combinedData <- joinTables(phoenixWithoutAP, "phoenixWithoutAP")
    responseAP <-  read.csv("combined2020PhoenixAP.csv")
    tracts <- st_read("Phoenix_tract_2010_Albers.shp")
    outName <- paste0("PhoenixNo2020AirPollutionPredictors", responseVar, "RegressionOutput.txt")
    
  }
  #Keeps only the complete rows
  combinedData <- combinedData[complete.cases(combinedData), ]
  #Pulls out only the response variable and GISJOIN from the 2020 air pollution table
  responseAP <- responseAP %>% select(GISJOIN, responseVar)
  #Adds response variable to all other data to have a complete dataset for the regression call
  modelData <- merge(combinedData, responseAP, by = "GISJOIN", all = FALSE)
  #Creates a dataset to be used to create the regression call to ensure that the response variable does not end up as a predictor
  predictors <- modelData %>% select(-GISJOIN, -responseVar)
  #Create a null model with only the response variable
  nullModel <- lm(as.formula(paste(responseVar, "~ 1")), data = modelData)
  #Creates a full model using the createRegressionCall function to contain all variables as predictors
  fullModel <- lm(
    createRegressionCall(responseVar, predictors),
    data = modelData
  )
  #Runs the forward model
  forwardModel <- step(
    nullModel,
    scope = list(
      lower = nullModel,
      upper = fullModel
    ),
    direction = "forward"
  )
  
  #Starts sink to print regression info into a .txt file
  sink(outName)
  
  #Filters the tracts so they match the data if any rows were removed
  filteredTracts <- tracts %>%
    filter(tracts$GISJOIN %in% modelData$GISJOIN)
  #Creates object of neighboring lists and connections
  tracts.nb <- poly2nb(filteredTracts)
  #Converts nb object to list of weights representing spatial relationships
  tracts.lw <- nb2listw(tracts.nb)
  #Runs Moran's I test to test for spatial autocorrelation of forward model residuals
  #Determines if GWR can be support (it will be ran regardless but Moran's I info needed to prove statistical significance)
  moran.test(forwardModel$residuals, tracts.lw)
  #Pulls the predictors used by forward regression in ending formula
  selected_predictors <- names(coef(forwardModel))[-1]
  #Creates right side of the equation by combining each predictor with a + 
  rightSide <- paste(selected_predictors, collapse = " + ")
  #Completes formula by adding in the response variable and tilde
  gwr_formula <- as.formula(paste(responseVar, "~", rightSide))
  #Filtering and conversion to ensure GWR data is in the correct format and only contains tracts being processed
  gwrData <- merge(filteredTracts, modelData, by = "GISJOIN")
  gwrData_sp <- as(gwrData, "Spatial")
  #Determines best bandwidth to use in GWR
  bw <- bw.gwr(gwr_formula, data=gwrData_sp, kernel="gaussian")
  #Runs GWR
  gwr_model <- gwr.basic(gwr_formula, data=gwrData_sp, bw=bw, kernel="gaussian")
  
  gwr_model
  summary(gwr_model)
  
  #Printing to save info to sink
  print(summary(forwardModel))
  print(moran.test(forwardModel$residuals, tracts.lw))
  print(gwr_model)
  print(summary(gwr_model))
  
  #Turns off sink
  sink()
  print("Completed regression and file created")
}


#Everything below this is just calls for each possible regression equation
#Separate equations for the two cities split by air pollution in input and response variable

#Houston minimum black carbon
regression("Houston", "MINGWRBC2020", TRUE)
regression("Houston", "MINGWRBC2020", FALSE)

#Houston maximum black carbon
regression("Houston", "MAXGWRBC2020", TRUE)
regression("Houston", "MAXGWRBC2020", FALSE)

#Houston black carbon range
regression("Houston", "RANGEGWRBC2020", TRUE)
regression("Houston", "RANGEGWRBC2020", FALSE)

#Houston mean black carbon
regression("Houston", "MEANGWRBC2020", TRUE)
regression("Houston", "MEANGWRBC2020", FALSE)

#Houston black carbon STD
regression("Houston", "STDGWRBC2020", TRUE)
regression("Houston", "STDGWRBC2020", FALSE)

#Houston median black carbon
regression("Houston", "MEDIANGWRBC2020", TRUE)
regression("Houston", "MEDIANGWRBC2020", FALSE)

#Houston minimum dust
regression("Houston", "MINGWRDUST2020", TRUE)
regression("Houston", "MINGWRDUST2020", FALSE)

#Houston maximum dust
regression("Houston", "MAXGWRDUST2020", TRUE)
regression("Houston", "MAXGWRDUST2020", FALSE)

#Houston dust range
regression("Houston", "RANGEGWRDUST2020", TRUE)
regression("Houston", "RANGEGWRDUST2020", FALSE)

#Houston mean dust
regression("Houston", "MEANGWRDUST2020", TRUE)
regression("Houston", "MEANGWRDUST2020", FALSE)

#Houston dust STD
regression("Houston", "STDGWRDUST2020", TRUE)
regression("Houston", "STDGWRDUST2020", FALSE)

#Houston median dust
regression("Houston", "MEDIANGWRDUST2020", TRUE)
regression("Houston", "MEDIANGWRDUST2020", FALSE)

#Houston minimum NH4
regression("Houston", "MINGWRNH42020", TRUE)
regression("Houston", "MINGWRNH42020", FALSE)

#Houston maximum NH4
regression("Houston", "MAXGWRNH42020", TRUE)
regression("Houston", "MAXGWRNH42020", FALSE)

#Houston NH4 range
regression("Houston", "RANGEGWRNH42020", TRUE)
regression("Houston", "RANGEGWRNH42020", FALSE)

#Houston mean NH4
regression("Houston", "MEANGWRNH42020", TRUE)
regression("Houston", "MEANGWRNH42020", FALSE)

#Houston NH4 STD
regression("Houston", "STDGWRNH42020", TRUE)
regression("Houston", "STDGWRNH42020", FALSE)

#Houston median NH4
regression("Houston", "MEDIANGWRNH42020", TRUE)
regression("Houston", "MEDIANGWRNH42020", FALSE)

#Houston minimum NO3
regression("Houston", "MINGWRNO32020", TRUE)
regression("Houston", "MINGWRNO32020", FALSE)

#Houston maximum NO3
regression("Houston", "MAXGWRNO32020", TRUE)
regression("Houston", "MAXGWRNO32020", FALSE)

#Houston NO3 range
regression("Houston", "RANGEGWRNO32020", TRUE)
regression("Houston", "RANGEGWRNO32020", FALSE)

#Houston mean NO3
regression("Houston", "MEANGWRNO32020", TRUE)
regression("Houston", "MEANGWRNO32020", FALSE)

#Houston NO3 STD
regression("Houston", "STDGWRNO32020", TRUE)
regression("Houston", "STDGWRNO32020", FALSE)

#Houston median NO3
regression("Houston", "MEDIANGWRNO32020", TRUE)
regression("Houston", "MEDIANGWRNO32020", FALSE)

#Houston minimum Organic Matter
regression("Houston", "MINGWROM2020", TRUE)
regression("Houston", "MINGWROM2020", FALSE)

#Houston maximum Organic Matter
regression("Houston", "MAXGWROM2020", TRUE)
regression("Houston", "MAXGWROM2020", FALSE)

#Houston Organic Matter range
regression("Houston", "RANGEGWROM2020", TRUE)
regression("Houston", "RANGEGWROM2020", FALSE)

#Houston mean Organic Matter
regression("Houston", "MEANGWROM2020", TRUE)
regression("Houston", "MEANGWROM2020", FALSE)

#Houston Organic Matter STD
regression("Houston", "STDGWROM2020", TRUE)
regression("Houston", "STDGWROM2020", FALSE)

#Houston median Organic Matter
regression("Houston", "MEDIANGWROM2020", TRUE)
regression("Houston", "MEDIANGWROM2020", FALSE)

#Houston minimum particulate matter
regression("Houston", "MINGWRPM252020", TRUE)
regression("Houston", "MINGWRPM252020", FALSE)

#Houston maximum particulate matter
regression("Houston", "MAXGWRPM252020", TRUE)
regression("Houston", "MAXGWRPM252020", FALSE)

#Houston particulate matter range
regression("Houston", "RANGEGWRPM252020", TRUE)
regression("Houston", "RANGEGWRPM252020", FALSE)

#Houston mean particulate matter
regression("Houston", "MEANGWRPM252020", TRUE)
regression("Houston", "MEANGWRPM252020", FALSE)

#Houston particulate matter STD
regression("Houston", "STDGWRPM252020", TRUE)
regression("Houston", "STDGWRPM252020", FALSE)

#Houston median particulate matter
regression("Houston", "MEDIANGWRPM252020", TRUE)
regression("Houston", "MEDIANGWRPM252020", FALSE)

#Houston minimum SO4
regression("Houston", "MINGWRSO42020", TRUE)
regression("Houston", "MINGWRSO42020", FALSE)

#Houston maximum SO4
regression("Houston", "MAXGWRSO42020", TRUE)
regression("Houston", "MAXGWRSO42020", FALSE)

#Houston SO4 range
regression("Houston", "RANGEGWRSO42020", TRUE)
regression("Houston", "RANGEGWRSO42020", FALSE)

#Houston mean SO4
regression("Houston", "MEANGWRSO42020", TRUE)
regression("Houston", "MEANGWRSO42020", FALSE)

#Houston SO4 STD
regression("Houston", "STDGWRSO42020", TRUE)
regression("Houston", "STDGWRSO42020", FALSE)

#Houston median SO4
regression("Houston", "MEDIANGWRSO42020", TRUE)
regression("Houston", "MEDIANGWRSO42020", FALSE)

#Houston minimum Sea Salt
regression("Houston", "MINGWRSS2020", TRUE)
regression("Houston", "MINGWRSS2020", FALSE)

#Houston maximum Sea Salt
regression("Houston", "MAXGWRSS2020", TRUE)
regression("Houston", "MAXGWRSS2020", FALSE)

#Houston Sea Salt range
regression("Houston", "RANGEGWRSS2020", TRUE)
regression("Houston", "RANGEGWRSS2020", FALSE)

#Houston mean Sea Salt
regression("Houston", "MEANGWRSS2020", TRUE)
regression("Houston", "MEANGWRSS2020", FALSE)

#Houston Sea Salt STD
regression("Houston", "STDGWRSS2020", TRUE)
regression("Houston", "STDGWRSS2020", FALSE)

#Houston median Sea Salt
regression("Houston", "MEDIANGWRSS2020", TRUE)
regression("Houston", "MEDIANGWRSS2020", FALSE)

#Phoenix minimum black carbon
regression("Phoenix", "MINGWRBC2020", TRUE)
regression("Phoenix", "MINGWRBC2020", FALSE)

#Phoenix maximum black carbon
regression("Phoenix", "MAXGWRBC2020", TRUE)
regression("Phoenix", "MAXGWRBC2020", FALSE)

#Phoenix black carbon range
regression("Phoenix", "RANGEGWRBC2020", TRUE)
regression("Phoenix", "RANGEGWRBC2020", FALSE)

#Phoenix mean black carbon
regression("Phoenix", "MEANGWRBC2020", TRUE)
regression("Phoenix", "MEANGWRBC2020", FALSE)

#Phoenix black carbon STD
regression("Phoenix", "STDGWRBC2020", TRUE)
regression("Phoenix", "STDGWRBC2020", FALSE)

#Phoenix median black carbon
regression("Phoenix", "MEDIANGWRBC2020", TRUE)
regression("Phoenix", "MEDIANGWRBC2020", FALSE)

#Phoenix minimum dust
regression("Phoenix", "MINGWRDUST2020", TRUE)
regression("Phoenix", "MINGWRDUST2020", FALSE)

#Phoenix maxium dust
regression("Phoenix", "MAXGWRDUST2020", TRUE)
regression("Phoenix", "MAXGWRDUST2020", FALSE)

#Phoenix dust range
regression("Phoenix", "RANGEGWRDUST2020", TRUE)
regression("Phoenix", "RANGEGWRDUST2020", FALSE)

#Phoenix mean dust
regression("Phoenix", "MEANGWRDUST2020", TRUE)
regression("Phoenix", "MEANGWRDUST2020", FALSE)

#Phoenix dust STD
regression("Phoenix", "STDGWRDUST2020", TRUE)
regression("Phoenix", "STDGWRDUST2020", FALSE)

#Phoenix median dust
regression("Phoenix", "MEDIANGWRDUST2020", TRUE)
regression("Phoenix", "MEDIANGWRDUST2020", FALSE)

#Phoenix minimum NH4
regression("Phoenix", "MINGWRNH42020", TRUE)
regression("Phoenix", "MINGWRNH42020", FALSE)

#Phoenix maximum NH4
regression("Phoenix", "MAXGWRNH42020", TRUE)
regression("Phoenix", "MAXGWRNH42020", FALSE)

#Phoenix NH4 range
regression("Phoenix", "RANGEGWRNH42020", TRUE)
regression("Phoenix", "RANGEGWRNH42020", FALSE)

#Phoenix mean NH4
regression("Phoenix", "MEANGWRNH42020", TRUE)
regression("Phoenix", "MEANGWRNH42020", FALSE)

#Phoenix NH4 STD
regression("Phoenix", "STDGWRNH42020", TRUE)
regression("Phoenix", "STDGWRNH42020", FALSE)

#Phoenix median NH4
regression("Phoenix", "MEDIANGWRNH42020", TRUE)
regression("Phoenix", "MEDIANGWRNH42020", FALSE)

#Phoenix minimum NO3
regression("Phoenix", "MINGWRNO32020", TRUE)
regression("Phoenix", "MINGWRNO32020", FALSE)

#Phoenix maximum NO3
regression("Phoenix", "MAXGWRNO32020", TRUE)
regression("Phoenix", "MAXGWRNO32020", FALSE)

#Phoenix NO3 range
regression("Phoenix", "RANGEGWRNO32020", TRUE)
regression("Phoenix", "RANGEGWRNO32020", FALSE)

#Phoenix mean NO3
regression("Phoenix", "MEANGWRNO32020", TRUE)
regression("Phoenix", "MEANGWRNO32020", FALSE)

#Phoenix NO3 STD
regression("Phoenix", "STDGWRNO32020", TRUE)
regression("Phoenix", "STDGWRNO32020", FALSE)

#Phoenix median NO3
regression("Phoenix", "MEDIANGWRNO32020", TRUE)
regression("Phoenix", "MEDIANGWRNO32020", FALSE)

#Phoenix minimum Organic Matter
regression("Phoenix", "MINGWROM2020", TRUE)
regression("Phoenix", "MINGWROM2020", FALSE)

#Phoenix maximum Organic Matter
regression("Phoenix", "MAXGWROM2020", TRUE)
regression("Phoenix", "MAXGWROM2020", FALSE)

#Phoenix Organic Matter range
regression("Phoenix", "RANGEGWROM2020", TRUE)
regression("Phoenix", "RANGEGWROM2020", FALSE)

#Phoenix mean Organic Matter
regression("Phoenix", "MEANGWROM2020", TRUE)
regression("Phoenix", "MEANGWROM2020", FALSE)

#Phoenix Organic Matter STD
regression("Phoenix", "STDGWROM2020", TRUE)
regression("Phoenix", "STDGWROM2020", FALSE)

#Phoenix median Organic Matter
regression("Phoenix", "MEDIANGWROM2020", TRUE)
regression("Phoenix", "MEDIANGWROM2020", FALSE)

#Phoenix minimum particulate matter
regression("Phoenix", "MINGWRPM252020", TRUE)
regression("Phoenix", "MINGWRPM252020", FALSE)

#Phoenix maximum particulate matter
regression("Phoenix", "MAXGWRPM252020", TRUE)
regression("Phoenix", "MAXGWRPM252020", FALSE)

#Phoenix particulate matter range
regression("Phoenix", "RANGEGWRPM252020", TRUE)
regression("Phoenix", "RANGEGWRPM252020", FALSE)

#Phoenix mean particulate matter
regression("Phoenix", "MEANGWRPM252020", TRUE)
regression("Phoenix", "MEANGWRPM252020", FALSE)

#Phoenix particulate matter STD
regression("Phoenix", "STDGWRPM252020", TRUE)
regression("Phoenix", "STDGWRPM252020", FALSE)

#Phoenix median particulate matter
regression("Phoenix", "MEDIANGWRPM252020", TRUE)
regression("Phoenix", "MEDIANGWRPM252020", FALSE)

#Phoenix minimum SO4
regression("Phoenix", "MINGWRSO42020", TRUE)
regression("Phoenix", "MINGWRSO42020", FALSE)

#Phoenix maximum SO4
regression("Phoenix", "MAXGWRSO42020", TRUE)
regression("Phoenix", "MAXGWRSO42020", FALSE)

#Phoenix SO4 range
regression("Phoenix", "RANGEGWRSO42020", TRUE)
regression("Phoenix", "RANGEGWRSO42020", FALSE)

#Phoenix mean SO4
regression("Phoenix", "MEANGWRSO42020", TRUE)
regression("Phoenix", "MEANGWRSO42020", FALSE)

#Phoenix SO4 STD
regression("Phoenix", "STDGWRSO42020", TRUE)
regression("Phoenix", "STDGWRSO42020", FALSE)

#Phoenix median SO4
regression("Phoenix", "MEDIANGWRSO42020", TRUE)
regression("Phoenix", "MEDIANGWRSO42020", FALSE)

#Phoenix minimum Sea Salt
regression("Phoenix", "MINGWRSS2020", TRUE)
regression("Phoenix", "MINGWRSS2020", FALSE)

#Phoenix maximum Sea Salt
regression("Phoenix", "MAXGWRSS2020", TRUE)
regression("Phoenix", "MAXGWRSS2020", FALSE)

#Phoenix Sea Salt range
regression("Phoenix", "RANGEGWRSS2020", TRUE)
regression("Phoenix", "RANGEGWRSS2020", FALSE)

#Phoenix mean Sea Salt
regression("Phoenix", "MEANGWRSS2020", TRUE)
regression("Phoenix", "MEANGWRSS2020", FALSE)

#Phoenix Sea Salt STD
regression("Phoenix", "STDGWRSS2020", TRUE)
regression("Phoenix", "STDGWRSS2020", FALSE)

#Phoenix median Sea Salt
regression("Phoenix", "MEDIANGWRSS2020", TRUE)
regression("Phoenix", "MEDIANGWRSS2020", FALSE)

