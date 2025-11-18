#Highest R^2 value for forward regression models is 0.9904 from model looking at
#Houston mean organic matter with 2000 and 2010 air pollution data included

#To test this model, the end equation from forward regression is brought in 
#from regression output file

# lm(formula = MEANGWROM2020 ~ MEANGWROM2000 + MEANGWRSO42010 + 
#     total_area_female_15_to_17_years_2020_houston + STDGWRPM252010 + 
#     STDGWRBC2000 + hispanic_or_latino_2000_houston + MAXGWRNH42000 + 
#     male_75_to_79_years_2000_houston + total_area_two_or_more_races_2020_houston + 
#     total_area_female_5_to_9_years_2020_houston + STDGWROM2000 + 
#     class23PercentUrban2020 + class23PercentUrban2000 + total_area_female_under_5_years_2010_houston + 
#     male_15_to_17_years_2000_houston + MEDIANGWRBC2010 + MINGWRSS2010 + 
#     MEANGWRNO32000 + total_area_male_75_to_79_years_2010_houston + 
#     MEDIANGWRSS2000 + MEDIANGWRNO32010 + MAXGWRBC2000 + MAXGWRSS2010 + 
#     male_40_to_44_years_2000_houston + MINGWROM2000 + MAXGWRDUST2000 + 
#     MEDIANGWRDUST2010 + MINGWRNO32000 + MEANGWRBC2010 + total_area_two_or_more_races_2010_houston + 
#     asian_alone_2000_houston + total_area_male_40_to_44_years_2010_houston + 
#     total_area_some_other_race_alone_2010_houston + total_area_american_indian_and_alaska_native_alone_2020_houston + 
#     MEDIANGWRSS2010 + MEANGWRSS2010 + total_area_native_hawaiian_and_other_pacific_islander_alone_2020_houston + 
#     MEANGWRNH42000 + STDGWRNH42000 + female_60_and_61_years_2000_houston + 
#     male_62_to_64_years_2000_houston + total_area_male_85_years_and_over_2020_houston + 
#     MEDIANGWRNO32000 + two_or_more_races_2000_houston + STDGWRNH42010 + 
#     MEDIANGWRNH42000 + MINGWRBC2010 + STDGWROM2010 + total_area_american_indian_and_alaska_native_alone_2010_houston + 
#     total_area_male_15_to_17_years_2020_houston + some_other_race_alone_2000_houston + 
#     RANGEGWRPM252000 + RANGEGWRSO42000 + total_area_male_65_and_66_years_2010_houston + 
#     total_area_male_70_to_74_years_2020_houston + total_area_female_65_and_66_years_2020_houston + 
#     total_area_female_2020_houston + total_area_male_22_to_24_years_2020_houston + 
#     total_area_female_20_years_2010_houston + total_area_male_62_to_64_years_2020_houston + 
#     RANGEGWRSS2000 + STDGWRDUST2000 + total_area_male_10_to_14_years_2020_houston + 
#     total_area_male_5_to_9_years_2020_houston + total_area_female_35_to_39_years_2010_houston + 
#     total_area_male_40_to_44_years_2020_houston + total_area_female_75_to_79_years_2010_houston + 
#     X_2000_houston + total_area_female_21_years_2020_houston + 
#     female_80_to_84_years_2000_houston + male_85_years_and_over_2000_houston + 
#     total_area_hispanic_or_latino_2010_houston + total_area_female_21_years_2010_houston, 
#     data = modelData)



library(stringr)
library(dplyr)
library(sf)
library(GWmodel)
library(spdep)

#Rerun forward regression for houston data in case first R^2 value was a false result
#And to make it easier to rerun the model with phoenix data

#Start processing data for use in regression model

phoenixRegressionData <- read.csv("combinedphoenixAll.csv")
houstonRegressionData <- read.csv("combinedhoustonAll.csv")

#Inclusion of Houston on the end of variable names will cause a slight problem
#Remove city name when present at end of column names in both sets of data
colnames(phoenixRegressionData) <- str_remove(colnames(phoenixRegressionData), ("phoenix"))
colnames(houstonRegressionData) <- str_remove(colnames(houstonRegressionData), ("houston"))

#Remove any incomplete cases from both sets of data
phoenixRegressionData <- phoenixRegressionData[complete.cases(phoenixRegressionData), ]
houstonRegressionData <- houstonRegressionData[complete.cases(houstonRegressionData), ]

#Import all 2020 air pollution data. Join just the response variable to the datasets that will be used in regression
#This ensures that the columns are lined up correctly.
responsePhoenix <- read.csv("combined2020PhoenixAP.csv")
responseHouston <- read.csv("combined2020HoustonAP.csv")
responsePhoenix <- responsePhoenix %>% select(GISJOIN, MEANGWROM2020)
responseHouston <- responseHouston %>% select(GISJOIN, MEANGWROM2020)
modelPhoenix <- merge(phoenixRegressionData, responsePhoenix, by = "GISJOIN", all = FALSE)
modelHouston <- merge(houstonRegressionData, responseHouston, by = "GISJOIN", all = FALSE)
predictorsHouston <- modelHouston  %>% select(-GISJOIN, -MEANGWROM2020)
predictorsPhoenix <- modelPhoenix  %>% select(-GISJOIN, -MEANGWROM2020)

#Create null model for houston data to use in forward selection
houstonNullModel <- lm(MEANGWROM2020 ~1, data = modelHouston)
houstonNullModel

#Create regression call function used in earlier regression code to automate creation of long input equation
createRegressionCall <- function(response, predictors){
  as.formula(
    paste(response, "~", paste(names(predictors), collapse = " + "))
  )
}

#Create houston full model to use in forward selection

houstonFullModel <- lm(
  createRegressionCall("MEANGWROM2020", predictorsHouston),
  data = modelHouston
)
houstonFullModel

#Run forward selection for houston
houstonForwardModel <- step(
  houstonNullModel,
  scope = list(
    lower = houstonNullModel,
    upper = houstonFullModel
  ),
  direction = "forward"
)
summary(houstonForwardModel)
#Houston forward model still has an R^2 of 0.9904

#Try a backwards model just to see
houstonBackwardModel <- step(houstonFullModel, direction = "backward") #Warning this takes a very long time. Approximately 30 minutes or more
summary(houstonBackwardModel) #Houston backwards model has an R^2 of 0.9907

selectedPredictors <- names(coef(houstonForwardModel))[-1]
rightSide <- paste(selectedPredictors, collapse = " + ")
resultFormula <- as.formula(paste("MEANGWROM2020", "~", rightSide))
phoenixModel <- lm(resultFormula, data = modelPhoenix)
summary(phoenixModel) #R^2 for Phoenix still 0.9747
predictions <- predict(phoenixModel, newdata = modelPhoenix)
summary(predictions)
#Look for spatial autocorrelation of residuals
phoenixTracts <- st_read("Phoenix_tract_2010_Albers.shp")

filteredPhoenixTracts <- phoenixTracts %>%
  filter(phoenixTracts$GISJOIN %in% modelPhoenix$GISJOIN)
phoenix.nb <- poly2nb(filteredPhoenixTracts)
phoenix.lw <- nb2listw(phoenix.nb)
moran.test(phoenixModel$residuals, phoenix.lw) #No spatial autocorrelation of residuals. Very low Moran's I (0.017) and high p-value (0.1892)
filteredPhoenixTracts$residual <- phoenixModel$residuals
plot(filteredPhoenixTracts["residual"])
par(mar = c(5, 10, 4, 2) + 0.1)
plot(modelPhoenix$MEANGWROM2020, predictions,
     main = "Cross-City Mean Organic Matter Predictions", 
     xlab = "Actual 2020 Mean Phoenix Organic Matter Value in µg/m3",    
     ylab = "2020 Mean Phoenix Organic Matter Value \n in µg/m3  Predicted by Houston Linear \n Regression Model",    
     col = "black",             
     pch = 21, 
     cex =1
  )
crossCity <- lm(modelPhoenix$MEANGWROM2020 ~ predictions)
summary(crossCity)
abline(crossCity, col="red", lwd=2)