library(factoextra)
library(cluster)
library(devtools)
library(spdep)
library(GISTools)
library(dplyr)

phoenixTracts <- st_read("Phoenix_tract_2010_Albers.shp")
houstonTracts <- st_read("Houston_tract_2010_Albers.shp")
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
houstonAll <- c("combined2000HoustonAP.csv", "combined2000HoustonUrb.csv", "combined2010HoustonAP.csv", "combined2010HoustonUrb.csv",
                "combined2020HoustonUrb.csv", "combined2020HoustonAP.csv", "Demographics2000Houston.csv", "Demographics2010Houston.csv","Demographics2020Houston.csv")
phoenixAll <- c("combined2000PhoenixAP.csv", "combined2000PhoenixUrb.csv","combined2010PhoenixAP.csv", "combined2010PhoenixUrb.csv",
                "combined2020PhoenixUrb.csv", "combined2020PhoenixAP.csv", "Demographics2000Phoenix.csv", "Demographics2010Phoenix.csv","Demographics2020Phoenix.csv")

joinTables(houstonAll, "combinedHoustonForClustering")
joinTables(phoenixAll, "combinedPhoenixForClustering")

#Clips demographics to just the tracts held in the air pollution and urbanization tables
demographicsClip <- function(demographicsTablePath, goodTablePath, city, year, outputDir = getwd()){
  demographicsTable <- read.csv(demographicsTablePath)
  goodTable <- read.csv(goodTablePath)
  demographicsTable <- demographicsTable %>%
    filter(GISJOIN %in% goodTable$GISJOIN)
  outputName = paste0("Demographics", year, city)
  outputPath <- file.path(outputDir, paste0(outputName, ".csv"))
  write.csv(demographicsTable, outputPath, row.names = FALSE)
}
demographicsClip("tract_data_houston2000.csv", "combined2000HoustonAP.csv", "Houston", "2000")
demographicsClip("tract_data_houston2010.csv", "combined2000HoustonAP.csv", "Houston", "2010")
demographicsClip("tract_data_houston2020.csv", "combined2000HoustonAP.csv", "Houston", "2020")
demographicsClip("tract_data_phoenix2000.csv", "combined2000PhoenixAP.csv", "Phoenix", "2000")
demographicsClip("tract_data_phoenix2010.csv", "combined2000PhoenixAP.csv", "Phoenix", "2010")
demographicsClip("tract_data_phoenix2020.csv", "combined2000PhoenixAP.csv", "Phoenix", "2020")

#Code to automatically perform k-means and SKATER with k=6
clustering <- function(table, tracts, outName, outputDir = getwd()){
  
  #Opens tables, keeps only complete cases, filters tracts to just locations with data
  table <- read.csv(table)
  table <- table[complete.cases(table),]
  validGISJOINs <- table$GISJOIN
  tracts <- tracts[tracts$GISJOIN %in% validGISJOINs,]
  table <- table[order(table$GISJOIN),]
  tracts <- tracts[order(tracts$GISJOIN), ]
  data <- table %>%
    select(-GISJOIN)
  dataScaled <- scale(data)
  tractsClean <- tracts[tracts$GISJOIN %in% table$GISJOIN, ]
  tractsClean <- tractsClean[order(tractsClean$GISJOIN), ]
  table <- table[order(table$GISJOIN), ]
  #Performs k-means clustering with k=6 on the cleaned data
  kmeans <- kmeans(dataScaled, centers=6)
  print(kmeans$size)
  print(kmeans$centers)
  #Adds the cluster number to the data
  table$kmeansCluster <- kmeans$cluster
  #Creates nb object of nearest neighbor connections
  tracts.nb <- poly2nb(tractsClean, queen = TRUE)
  #Stores the number of tracts that do not have neighbors
  no_neighbors <- which(card(tracts.nb) == 0)
  #If there are any tracts that do not have neighbors, remove these tracts and create a new nb object
  if (length(no_neighbors) > 0) {
    tractsClean <- tractsClean[-no_neighbors, ]
    table <- table[!table$GISJOIN %in% tractsClean$GISJOIN[no_neighbors], ]
    dataScaled <- dataScaled[!rownames(dataScaled) %in% as.character(no_neighbors), ]
    tracts.nb <- poly2nb(tractsClean, queen = TRUE)
  }
  #Checks that the study area is fully connected and there are no odd separate areas
  #Removes outlying areas and creates a new .nb object if there are any odd areas
  comp <- n.comp.nb(tracts.nb)
  if (comp$nc > 1) {
    mainComp <- which(comp$comp.id == 1)
    keepGISJOINs <- tractsClean$GISJOIN[mainComp]
    tractsClean <- tractsClean[mainComp, ]
    table <- table[table$GISJOIN %in% keepGISJOINs, ]
    dataScaled <- dataScaled[table$GISJOIN %in% keepGISJOINs, ]
    tracts.nb <- poly2nb(tractsClean, queen = TRUE)
  }
  stopifnot(all.equal(tractsClean$GISJOIN, table$GISJOIN))
  
  #Finds the costs of each edge
  lcosts <- nbcosts(tracts.nb, dataScaled)
  #Creates a list of spatial weights for neighbors using the cost of each edge
  tracts.w <- nb2listw(tracts.nb, lcosts, style="B")
  #Creates a minimum spanning tree
  tracts.mst <- mstree(tracts.w)
  #Cuts the minimum spanning tree until there are 6 clusters
  clust <- skater(tracts.mst[,1:2], dataScaled, 5)
  #Adds the SKATER cluster number to the data table
  table$skaterCluster <- clust$groups
  #Exports the data table with the clusterin information
  outputPath <- file.path(outputDir, paste0("clustered", outName, ".csv"))
  write.csv(table, outputPath, row.names = FALSE)
  return(table)
}

#Function to run principal component analysis and export results
#Ended up not being used for GIS Day
#Cluster means are values for each principal component at the tract level
#Rotations are amount of each original variable in each principal component
PCA <- function(path, outName, outputDir = getwd()){
  table <- read.csv(path)
  table <- table[complete.cases(table),]
  GISJOINs <- table%>% select("GISJOIN")
  table <- table %>% select(-"GISJOIN")
  spc <- prcomp(table,
                center = TRUE,  
                scale. = TRUE)
  print(summary(spc))
  print(spc)
  pcaLoadings <- predict(spc)
  print(pcaLoadings)
  clusterMeans <- as.data.frame(predict(spc))
  clusterMeans$GISJOIN <- GISJOINs$GISJOIN
  rotations <- spc$rotation
  rownames(rotations) <- colnames(table)
  outputPath <- file.path(outputDir, paste0("rotations", outName, ".csv"))
  write.csv(rotations, outputPath, row.names = TRUE)
  outputPath <- file.path(outputDir, paste0("ClusterMeans", outName, ".csv"))
  write.csv(clusterMeans, outputPath, row.names = FALSE)
}


clustering("combinedcombinedHoustonForClustering.csv", houstonTracts, "combinedHoustonForClustering")
clustering("combinedcombinedPhoenixForClustering.csv", phoenixTracts, "combinedPhoenixForClustering")