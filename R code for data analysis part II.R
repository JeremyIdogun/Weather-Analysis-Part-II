library(tidyverse)
library(ggplot2)
library(coefplot)

# to import the modified dataset from Part I
attach (Weather_Data_DATA_SET_44_Modified)

#investigating correlation between continuous variables in the sample data

#correlation between Actual Temperature and Apparent Temperature 
cor.test(`Actual Temperature (C)`,`Apparent Temperature (C)`)

#correlation between Actual Temperature and Humidity
cor.test(`Actual Temperature (C)`,Humidity, method = "spearman")

#correlation between Actual Temperature and Wind Speed
cor.test(`Actual Temperature (C)`, `Wind Speed (km/h)`, method = "spearman")

#correlation between Actual Temperature and Wind Bearing
cor.test(`Actual Temperature (C)`, `Wind Bearing (degrees)`, method = "spearman")

#correlation between Actual Temperature and Visibility
cor.test(`Actual Temperature (C)`, `Visibility (km)`, method = "spearman")

#correlation between Actual Temperature and Pressure
cor.test(`Actual Temperature (C)`, `Pressure (millibars)`)



#correlation between Apparent Temperature and Humidity
cor.test(`Apparent Temperature (C)`,Humidity, method = "spearman")

#correlation between Apparent Temperature and Wind Speed
cor.test(`Apparent Temperature (C)`, `Wind Speed (km/h)`, method = "spearman")

#correlation between Apparent Temperature and Wind Bearing
cor.test(`Apparent Temperature (C)`, `Wind Bearing (degrees)`, method = "spearman")

#correlation between Apparent Temperature and Visibility
cor.test(`Apparent Temperature (C)`, `Visibility (km)`, method = "spearman")

#correlation between Apparent Temperature and Pressure
cor.test(`Apparent Temperature (C)`, `Pressure (millibars)`)


# Regression model for Actual Temperature
#the full regression model given the independents variable in the dataset
actualTemp.full.model = lm(`Actual Temperature (C)`~Humidity + `Visibility (km)`
                       + `Pressure (millibars)` + `Wind Speed (km/h)`
                       + `Wind Bearing (degrees)` + `Modified Summary`
                       + `Precipitation Type`)

summary (actualTemp.full.model)

#defining the null model with no independent variables
actualTemp.null.model = lm(`Actual Temperature (C)` ~ 1)


#Forward model selection 
forward.selection = step(actualTemp.null.model, scope = 
                            list(upper = actualTemp.full.model, 
                            lower = actualTemp.null.model),
                            direction = "forward", trace = FALSE)

summary (forward.selection)

#Backward Elimination model selection
backward.selection = step(actualTemp.full.model, scope = 
                            list(upper = actualTemp.full.model, 
                            lower = actualTemp.null.model), 
                            direction = "backward", trace = FALSE)

summary (backward.selection)

#Stepwise model selection
stepwise.selection = step(actualTemp.null.model, scope = 
                            list(upper = actualTemp.full.model, 
                            lower = actualTemp.null.model), direction = "both",
                            trace = FALSE)

summary (stepwise.selection)

#Final regression model for actual temperature
actualTemp.final.model = lm (`Actual Temperature (C)` ~ `Precipitation Type` +
                               Humidity + `Wind Speed (km/h)` + `Pressure (millibars)`
                             + `Visibility (km)`)
summary (actualTemp.final.model)
plot(actualTemp.final.model)


# Regression model for Apparent Temperature
#the full regression model given the independents variable in the dataset
appTemp.full.model = lm(`Apparent Temperature (C)`~Humidity + `Visibility (km)`
                           + `Pressure (millibars)` + `Wind Speed (km/h)`
                           + `Wind Bearing (degrees)` + `Modified Summary`
                           + `Precipitation Type`)
summary (appTemp.full.model)

#defining the null model with no independent variables
appTemp.null.model = lm(`Apparent Temperature (C)` ~ 1)


#Forward model selection 
appTemp_forward.selection = step(appTemp.null.model, scope = 
                           list(upper = appTemp.full.model, 
                                lower = appTemp.null.model),
                         direction = "forward", trace = FALSE)

summary (appTemp_forward.selection)

#Backward Elimination model selection
appTemp_backward.selection = step(appTemp.full.model, scope = 
                            list(upper = appTemp.full.model, 
                                 lower = appTemp.null.model), 
                          direction = "backward", trace = FALSE)

summary (appTemp_backward.selection)

#Stepwise model selection
appTemp_stepwise.selection = step(appTemp.null.model, scope = 
                            list(upper = appTemp.full.model, 
                                lower = appTemp.null.model), direction = "both",
                                trace = FALSE)

summary (appTemp_stepwise.selection)

#final regression model for apparent temperature
appTemp.final.model = lm (`Apparent Temperature (C)` ~ `Precipitation Type` +
                               Humidity + `Wind Speed (km/h)` + `Pressure (millibars)`
                             + `Visibility (km)`)
summary(appTemp.final.model)
plot(appTemp.final.model)
