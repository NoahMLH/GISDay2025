library(dplyr)
#Function to process zonal statistic tables with air pollution and NLCD data
renameZonal <- function(path) {
  table <- read.csv(path)
  #Cuts the year and type out of the file name
  #Consistent formatting from earlier steps allows this to work
  year <- substr(path, nchar(path) - 8, nchar(path) - 5)
  type <- substr(path, 1, nchar(path) - 9)
  type <- substr(type, 11, nchar(type))
  
  #For percent impervious surface data,
    #Keep the median, standard deviation, mean, and range values
    #Add the type, in this case "FctImp" and year to the end of each column name
    #This makes it easier to keep track of data once it is combined with other results
  if (type == "FctImp") {
    table <- table %>%
      select(GISJOIN, MEDIAN, STD, MEAN, RANGE) %>%
      rename_with(
        .fn = ~ paste0(., type, year),
        .cols = c("MEDIAN", "STD", "MEAN", "RANGE")
      )
  }
  #For urban classes,
    #Determine if the amount of land covered by that class is the majority (1) or the minority (0)
    #Keep the correct percent based off of this so that we have the percent of land covered by that class
    #In the case that neither minority or majority are 1, set the percent of land covered by that class to 0.0001
    #This very small number is used to prevent these rows from being thrown out for having NA values while using a value that will not impact future results
  if (type == "Class21Urb") {
    table <- table %>% mutate(class21PercentUrban = ifelse(
      MINORITY == 1,
      MINORITY_PERCENT,
      ifelse(MAJORITY == 1, MAJORITY_PERCENT, 0.0001)
    )) %>%
      select(GISJOIN, class21PercentUrban) %>%
      rename_with(.fn = ~ paste0(., year),
                  .cols = c("class21PercentUrban"))
  }
  if (type == "Class22Urb") {
    table <- table %>% mutate(class22PercentUrban = ifelse(
      MINORITY == 1,
      MINORITY_PERCENT,
      ifelse(MAJORITY == 1, MAJORITY_PERCENT, 0.0001)
    )) %>%
      select(GISJOIN, class22PercentUrban) %>%
      rename_with(.fn = ~ paste0(., year),
                  .cols = c("class22PercentUrban"))
  }
  if (type == "Class23Urb") {
    table <- table %>% mutate(class23PercentUrban = ifelse(
      MINORITY == 1,
      MINORITY_PERCENT,
      ifelse(MAJORITY == 1, MAJORITY_PERCENT, 0.0001)
    )) %>%
      select(GISJOIN, class23PercentUrban) %>%
      rename_with(.fn = ~ paste0(., year),
                  .cols = c("class23PercentUrban"))
  }
  if (type == "Class24Urb") {
    table <- table %>% mutate(class24PercentUrban = ifelse(
      MINORITY == 1,
      MINORITY_PERCENT,
      ifelse(MAJORITY == 1, MAJORITY_PERCENT, 0.0001)
    )) %>%
      select(GISJOIN, class24PercentUrban) %>%
      rename_with(.fn = ~ paste0(., year),
                  .cols = c("class24PercentUrban"))
  }
  if (type == "AllUrb") {
    table <- table %>% mutate(overallPercentUrban = ifelse(
      MINORITY == 1,
      MINORITY_PERCENT,
      ifelse(MAJORITY == 1, MAJORITY_PERCENT, 0.0001)
    )) %>%
      select(GISJOIN, overallPercentUrban) %>%
      rename_with(.fn = ~ paste0(., year),
                  .cols = c("overallPercentUrban"))
  }
  #For air pollution variables,
    #keep minimum, maximum, range, mean, standard deviation, and median
  if (type %in% c("GWRBC",
                  "GWRDUST",
                  "GWRNH4",
                  "GWRNO3",
                  "GWROM",
                  "GWRSO4",
                  "GWRSS",
                  "GWRPM25")) {
    table <- table %>%
      select(GISJOIN, MIN, MAX, RANGE, MEAN, STD, MEDIAN) %>%
      rename_with(
        .fn = ~ paste0(., type, year),
        .cols = c("MIN", "MAX", "RANGE", "MEAN", "STD", "MEDIAN")
      )
    
  }
  return(table)
}
#Joins all input lists together by GISJOIN
joinTables <- function(tableList, outputName, outputDir = getwd()) {
  #Run the rename function across all input tables
  renamedList <- lapply(tableList, renameZonal)
  #Stops the function run if any table is missing GISJOIN
  stopifnot(all(sapply(renamedList, function(df)
    "GISJOIN" %in% names(df))))
  
  #Merges the data frames (not technically a join since they have different column names)
  combinedTable <- Reduce(function(x, y)
    merge(x, y, by = "GISJOIN", all = TRUE), renamedList)
  
  outputPath <- file.path(outputDir, paste0("combined", outputName, ".csv"))
  write.csv(combinedTable, outputPath, row.names = FALSE)
  
  return(combinedTable)
}

#Everything below here is calls to create combined data. 
houston2000Urb <- c(
  "zonalTableAllUrb2000H.csv",
  "zonalTableClass21Urb2000H.csv",
  "zonalTableClass22Urb2000H.csv",
  "zonalTableClass23Urb2000H.csv",
  "zonalTableClass24Urb2000H.csv",
  "zonalTableFctImp2000H.csv"
)
houston2010Urb <- c(
  "zonalTableAllUrb2010H.csv",
  "zonalTableClass21Urb2010H.csv",
  "zonalTableClass22Urb2010H.csv",
  "zonalTableClass23Urb2010H.csv",
  "zonalTableClass24Urb2010H.csv",
  "zonalTableFctImp2010H.csv"
)
houston2020Urb <- c(
  "zonalTableAllUrb2020H.csv",
  "zonalTableClass21Urb2020H.csv",
  "zonalTableClass22Urb2020H.csv",
  "zonalTableClass23Urb2020H.csv",
  "zonalTableClass24Urb2020H.csv",
  "zonalTableFctImp2020H.csv"
)

phoenix2000Urb <- c(
  "zonalTableAllUrb2000P.csv",
  "zonalTableClass21Urb2000P.csv",
  "zonalTableClass22Urb2000P.csv",
  "zonalTableClass23Urb2000P.csv",
  "zonalTableClass24Urb2000P.csv",
  "zonalTableFctImp2000P.csv"
)
phoenix2010Urb <- c(
  "zonalTableAllUrb2010P.csv",
  "zonalTableClass21Urb2010P.csv",
  "zonalTableClass22Urb2010P.csv",
  "zonalTableClass23Urb2010P.csv",
  "zonalTableClass24Urb2010P.csv",
  "zonalTableFctImp2010P.csv"
)
phoenix2020Urb <- c(
  "zonalTableAllUrb2020P.csv",
  "zonalTableClass21Urb2020P.csv",
  "zonalTableClass22Urb2020P.csv",
  "zonalTableClass23Urb2020P.csv",
  "zonalTableClass24Urb2020P.csv",
  "zonalTableFctImp2020P.csv"
)
houston2000AP <- c(
  "zonalTableGWRBC2000H.csv",
  "zonalTableGWRDUST2000H.csv",
  "zonalTableGWRNH42000H.csv",
  "zonalTableGWRNO32000H.csv",
  "zonalTableGWROM2000H.csv",
  "zonalTableGWRPM252000H.csv",
  "zonalTableGWRSO42000H.csv",
  "zonalTableGWRSS2000H.csv"
)
houston2010AP <- c(
  "zonalTableGWRBC2010H.csv",
  "zonalTableGWRDUST2010H.csv",
  "zonalTableGWRNH42010H.csv",
  "zonalTableGWRNO32010H.csv",
  "zonalTableGWROM2010H.csv",
  "zonalTableGWRPM252010H.csv",
  "zonalTableGWRSO42010H.csv",
  "zonalTableGWRSS2010H.csv"
)
houston2020AP <- c(
  "zonalTableGWRBC2020H.csv",
  "zonalTableGWRDUST2020H.csv",
  "zonalTableGWRNH42020H.csv",
  "zonalTableGWRNO32020H.csv",
  "zonalTableGWROM2020H.csv",
  "zonalTableGWRPM252020H.csv",
  "zonalTableGWRSO42020H.csv",
  "zonalTableGWRSS2020H.csv"
)

phoenix2000AP <- c(
  "zonalTableGWRBC2000P.csv",
  "zonalTableGWRDUST2000P.csv",
  "zonalTableGWRNH42000P.csv",
  "zonalTableGWRNO32000P.csv",
  "zonalTableGWROM2000P.csv",
  "zonalTableGWRPM252000P.csv",
  "zonalTableGWRSO42000P.csv",
  "zonalTableGWRSS2000P.csv"
)
phoenix2010AP <- c(
  "zonalTableGWRBC2010P.csv",
  "zonalTableGWRDUST2010P.csv",
  "zonalTableGWRNH42010P.csv",
  "zonalTableGWRNO32010P.csv",
  "zonalTableGWROM2010P.csv",
  "zonalTableGWRPM252010P.csv",
  "zonalTableGWRSO42010P.csv",
  "zonalTableGWRSS2010P.csv"
)
phoenix2020AP <- c(
  "zonalTableGWRBC2020P.csv",
  "zonalTableGWRDUST2020P.csv",
  "zonalTableGWRNH42020P.csv",
  "zonalTableGWRNO32020P.csv",
  "zonalTableGWROM2020P.csv",
  "zonalTableGWRPM252020P.csv",
  "zonalTableGWRSO42020P.csv",
  "zonalTableGWRSS2020P.csv"
)

joinTables(houston2000Urb, "2000HoustonUrb")
joinTables(houston2010Urb, "2010HoustonUrb")
joinTables(houston2020Urb, "2020HoustonUrb")
joinTables(phoenix2000Urb, "2000PhoenixUrb")
joinTables(phoenix2010Urb, "2010PhoenixUrb")
joinTables(phoenix2020Urb, "2020PhoenixUrb")

joinTables(houston2000AP, "2000HoustonAP")
joinTables(houston2010AP, "2010HoustonAP")
joinTables(houston2020AP, "2020HoustonAP")
joinTables(phoenix2000AP, "2000PhoenixAP")
joinTables(phoenix2010AP, "2010PhoenixAP")
joinTables(phoenix2020AP, "2020PhoenixAP")