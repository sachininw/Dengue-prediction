---
title: "Data Mining Project"
author: "Sachini Weerasekara and Lai Peng"
date: "4/10/2020"
output: html_document
---

library(tidyverse)
library(dplyr)
library(FNN)
library(psych)

library(ggplot2)
library(corrplot)
library(magrittr)
library(zoo)
library(RColorBrewer)
library(scatterplot3d)
library(gridExtra)
library(MASS)

library(neuralnet)
library(forecast)
library(BBmisc)
library(caret)
library(rpart) 
library(rpart.plot) 
library(maptree)
library(randomForest)

library (modelr)
library(leaps)
library(gains)
library(pROC)


############################################################ STEP 00 - DATA PREPARATION #################################################

#Rename variables
names(Dengue_Train) <- 
  c("CITY","YEAR","WEEK_OF_YEAR","WEEK_START_DATE","NDVI_NE","NDVI_NW","NDVI_SE","NDVI_SW","PRECIP_AMT_MM", "REANALYSIS_AIR_TEMP_K","REANALYSIS_AVG_TEMP_K","REANALYSIS_DEWPOINT_TEMP_K","REANALYSIS_MAX_AIR_TEMP_K","REANALYSIS_MIN_AIR_TEMP_K","REANALYSIS_PRECIP_AMT_KGPERM2","REANALYSIS_RELATIVE_HUMID_PCNT","REANALYSIS_SAT_PRECIP_AMT_MM","REANALYSIS_SPECIFIC_HUMID_GPERKG","REANALYSIS_TDTR_K","STATION_AVG_TEMP_C","STATION_DIUR_TEMP_RANGE_C","STATION_MAX_TEMP_C","STATION_MIN_TEMP_C","STATION_PRECIP_MM")

#Removing 1st row (Label's row)
Dengue_Train <- 
  subset(Dengue_Train,CITY!="city")

#Insert response variable also to the same dataframe and rename
Dengue <- 
  merge(Dengue_Train,Dengue_Response,by.x=c("CITY","YEAR","WEEK_OF_YEAR"),by.y=c("city","year","weekofyear"),all=T)

colnames(Dengue)[colnames(Dengue) == "total_cases"] = "TOTAL_CASES"

#Removing similar columns
Dengue <- 
  within(Dengue,rm(REANALYSIS_AIR_TEMP_K))

##Check summary of data
summary(Dengue)
#Numeric variables are in character form. They are converted to numeric form as below.

#Turn char variables to numerical as appropriate
Dengue$WEEK_OF_YEAR = as.numeric(Dengue$WEEK_OF_YEAR)
Dengue$NDVI_NE = as.numeric(Dengue$NDVI_NE)
Dengue$NDVI_NW = as.numeric(Dengue$NDVI_NW)
Dengue$NDVI_SE = as.numeric(Dengue$NDVI_SE)
Dengue$NDVI_SW = as.numeric(Dengue$NDVI_SW)
Dengue$PRECIP_AMT_MM = as.numeric(Dengue$PRECIP_AMT_MM)
Dengue$REANALYSIS_AIR_TEMP_K = as.numeric(Dengue$REANALYSIS_AIR_TEMP_K)
Dengue$REANALYSIS_AVG_TEMP_K = as.numeric(Dengue$REANALYSIS_AVG_TEMP_K)
Dengue$REANALYSIS_DEWPOINT_TEMP_K = as.numeric(Dengue$REANALYSIS_DEWPOINT_TEMP_K)
Dengue$REANALYSIS_MAX_AIR_TEMP_K = as.numeric(Dengue$REANALYSIS_MAX_AIR_TEMP_K)
Dengue$REANALYSIS_MIN_AIR_TEMP_K = as.numeric(Dengue$REANALYSIS_MIN_AIR_TEMP_K)
Dengue$REANALYSIS_PRECIP_AMT_KGPERM2 = as.numeric(Dengue$REANALYSIS_PRECIP_AMT_KGPERM2)
Dengue$REANALYSIS_RELATIVE_HUMID_PCNT = as.numeric(Dengue$REANALYSIS_RELATIVE_HUMID_PCNT)
Dengue$REANALYSIS_SAT_PRECIP_AMT_MM = as.numeric(Dengue$REANALYSIS_SAT_PRECIP_AMT_MM)
Dengue$REANALYSIS_SPECIFIC_HUMID_GPERKG = as.numeric(Dengue$REANALYSIS_SPECIFIC_HUMID_GPERKG)
Dengue$REANALYSIS_TDTR_K = as.numeric(Dengue$REANALYSIS_TDTR_K)
Dengue$STATION_AVG_TEMP_C = as.numeric(Dengue$STATION_AVG_TEMP_C)
Dengue$STATION_DIUR_TEMP_RANGE_C = as.numeric(Dengue$STATION_DIUR_TEMP_RANGE_C)
Dengue$STATION_MAX_TEMP_C = as.numeric(Dengue$STATION_MAX_TEMP_C)
Dengue$STATION_MIN_TEMP_C = as.numeric(Dengue$STATION_MIN_TEMP_C)
Dengue$STATION_PRECIP_MM = as.numeric(Dengue$STATION_PRECIP_MM)
Dengue$STATION_PRECIP_MM = as.numeric(Dengue$STATION_PRECIP_MM)
Dengue$TOTAL_CASES = as.numeric(Dengue$TOTAL_CASES)

#Split data into two cities
SJ_Dengue <- 
  Dengue %>% filter(CITY == 'sj')

IQ_Dengue <-
  Dengue %>% filter(CITY == 'iq')

#################################################################### STEP 01 - DATA EXPLORATION AND VISUALIZATION ####################################

#Exploration of response variable
cat('\nSan Juan\n',
    '\t total cases mean: ',      SJ_Dengue$TOTAL_CASES %>% mean(), 
    '\t total cases variance: ' , SJ_Dengue$TOTAL_CASES %>% var() )

cat('\nIquitos\n',
    '\t total cases mean: ',      IQ_Dengue$TOTAL_CASES %>% mean(), 
    '\t total cases variance: ' , IQ_Dengue$TOTAL_CASES %>% var() )

#Total cases-Histograms
rbind(IQ_Dengue, SJ_Dengue) %>% 
  ggplot(aes(x = TOTAL_CASES,fill = ..count..)) + 
  geom_histogram(bins = 12, colour = 'black') + ggtitle('Total Cases of Dengue') +
  scale_y_continuous(breaks = seq(0,700,100)) + facet_wrap(~CITY)

#total cases vs week of year in San Juan in 2000
Dengue_SJ<-
  Dengue %>% filter(YEAR == "2000" & CITY=="sj")
ggplot(data = Dengue_SJ, mapping = aes(x = WEEK_OF_YEAR, y =TOTAL_CASES)) +
  geom_point()+ggtitle("total cases vs week of year of San Juan in 2000")
#total cases vs week of year in Iquitos in 2000
Dengue_IQ<-
  Dengue %>% filter(YEAR == "2000" & CITY=="iq")
ggplot(data = Dengue_IQ, mapping = aes(x = WEEK_OF_YEAR, y =TOTAL_CASES)) +
  geom_jitter()+ggtitle("total cases vs week of year of Iquitos in 2000")
#total cases vs pixel southeast of city centroid in San Juan in 2000
Dengue_SJ<-
  Dengue %>% filter(YEAR == "2000", CITY=="sj")
ggplot(data = Dengue_SJ, mapping = aes(x = NDVI_SE, y =TOTAL_CASES)) +
  geom_jitter()+ggtitle("total cases vs pixel southeast of city centroid of San Juan in 2000")
#total cases vs pixel southwest of city centroid in San Juan in 2000
Dengue_SJ<-
  Dengue %>% filter(YEAR == "2000", CITY=="sj")
ggplot(data = Dengue_SJ, mapping = aes(x = NDVI_SW, y =TOTAL_CASES)) +
  geom_point()+ggtitle("total cases vs pixel southwest of city centroid of San Juan in 2000")
#total cases vs pixel northeast of city centroid in San Juan in 2000
Dengue_SJ<-
  Dengue %>% filter(YEAR == "2000", CITY=="sj")
ggplot(data = Dengue_SJ, mapping = aes(x = NDVI_NE, y =TOTAL_CASES)) +
  geom_jitter()+ggtitle("total cases vs pixel northeast of city centroid of San Juan in 2000")
#total cases vs pixel northwest of city centroid in San Juan in 2000
Dengue_SJ<-
  Dengue %>% filter(YEAR == "2000", CITY=="sj")
ggplot(data = Dengue_SJ, mapping = aes(x = NDVI_NW, y =TOTAL_CASES)) +
  geom_point()+ggtitle("total cases vs pixel northwest of city centroid of San Juan in 2000")
#total cases vs pixel southeast of city centroid in Iquitos in 2000
Dengue_IQ<-
  Dengue %>% filter(YEAR == "2000", CITY=="iq")
ggplot(data = Dengue_IQ, mapping = aes(x = NDVI_SE, y =TOTAL_CASES)) +
  geom_jitter()+ggtitle("total cases vs pixel southeast of city centroid of Iquitos in 2000")
#total cases vs pixel southwest of city centroid in Iquitos in 2000
Dengue_IQ<-
  Dengue %>% filter(YEAR == "2000", CITY=="iq")
ggplot(data = Dengue_IQ, mapping = aes(x = NDVI_SW, y =TOTAL_CASES)) +
  geom_point()+ggtitle("total cases vs pixel southwest of city centroid of Iquitos in 2000")
#total cases vs pixel northeast of city centroid in Iquitos in 2000
Dengue_IQ<-
  Dengue %>% filter(YEAR == "2000", CITY=="iq")
ggplot(data = Dengue_IQ, mapping = aes(x = NDVI_NE, y =TOTAL_CASES)) +
  geom_jitter()+ggtitle("total cases vs pixel northeast of city centroid of Iquitos in 2000")
#total cases vs pixel northwest of city centroid in Iquitos in 2000
Dengue_IQ<-
  Dengue %>% filter(YEAR == "2000", CITY=="iq")
ggplot(data = Dengue_IQ, mapping = aes(x = NDVI_NW, y =TOTAL_CASES)) +
  geom_dotplot()+ggtitle("total cases vs pixel northwest of city centroid of Iquitos in 2000")
#total cases vs total precipitation of San Juan in 2000
Dengue_IQ<-
  Dengue %>% filter(YEAR == "2000", CITY=="sj")
ggplot(data = Dengue_IQ, mapping = aes(x = PRECIP_AMT_MM, y =TOTAL_CASES)) +
  geom_point()+ggtitle("total cases vs total precipitation of San Juan in 2000")
#total cases vs total precipitation of Iquitos in 2000
Dengue_IQ<-
  Dengue %>% filter(YEAR == "2000", CITY=="iq")
ggplot(data = Dengue_IQ, mapping = aes(x = PRECIP_AMT_MM, y =TOTAL_CASES)) +
  geom_point()+ggtitle("total cases vs total precipitation of Iquitos in 2000")
#total cases vs average air temperature vs mean relative humidity of San Juan in 2000
Dengue_SJ<-
  Dengue %>% filter(YEAR == "2000", CITY=="sj")

attach(Dengue_SJ)
scatterplot3d(REANALYSIS_AVG_TEMP_K, TOTAL_CASES, REANALYSIS_RELATIVE_HUMID_PCNT,
              main="total cases vs average air temperature vs mean relative humidity of San Juan in 2000")
#total cases vs average air temperature vs mean relative humidity of Iquitos in 2000
Dengue_IQ<-
  Dengue %>% filter(YEAR == "2000" & CITY=="iq")

attach(Dengue_IQ)
scatterplot3d(REANALYSIS_AVG_TEMP_K, TOTAL_CASES, REANALYSIS_RELATIVE_HUMID_PCNT,
              main="total cases vs average air temperature vs mean relative humidity of San Juan in 2000")


##################################################################### STEP 02 - DATA MINING TASKS #################################################

## Checking for noise
summary(Dengue)

##Getting total number of missing values
sum(is.na(Dengue))

#Get columnwise missing value counts
na_count <-
  sapply(Dengue, function(y) sum(length(which(is.na(y)))))
na_count <- 
  data.frame(na_count)
na_count

#Vegitation index missing value visualization
SJ_Dengue %>%
  mutate(index = as.numeric(row.names(.))) %>%
  ggplot(aes(index, NDVI_NE)) + 
  geom_line(colour = 'dodgerblue') +
  ggtitle("Vegetation Index over Time")

IQ_Dengue %>%
  mutate(index = as.numeric(row.names(.))) %>%
  ggplot(aes(index, NDVI_NE)) + 
  geom_line(colour = 'dodgerblue') +
  ggtitle("Vegetation Index over Time")

####################### IMPUTE MISSING VALUES WITH LASTEST VALUE

SJ_Dengue <-
  SJ_Dengue %>% na.locf(fromLast = TRUE)
IQ_Dengue <-
  IQ_Dengue %>% na.locf(fromLast = TRUE)


sum(is.na(SJ_Dengue))
sum(is.na(IQ_Dengue))

################################## DATA TRANSFORMATION

#Log transformation of response variable
SJ_Dengue$TOTAL_CASES_LOG <- 
  log(SJ_Dengue$TOTAL_CASES)

hist(SJ_Dengue$TOTAL_CASES_LOG)

IQ_Dengue$TOTAL_CASES_LOG <- 
  log(IQ_Dengue$TOTAL_CASES)

hist(IQ_Dengue$TOTAL_CASES_LOG)

####################################### FEATURE ENGINEERING

#Correlation plot to justify the features added

i <- sapply(SJ_Dengue, is.numeric)
y <- "TOTAL_CASES" 
x <- setdiff(names(SJ_Dengue)[i], y)
cor(SJ_Dengue[x], SJ_Dengue[y])

i <- sapply(IQ_Dengue, is.numeric)
y <- "TOTAL_CASES" 
x <- setdiff(names(IQ_Dengue)[i], y)
cor(IQ_Dengue[x], IQ_Dengue[y])


##### DRAW PLOT #####

### Adding number of cases of previous week as a feature
SJ_Dengue$PRE_WEEK_CASES <- 0
IQ_Dengue$PRE_WEEK_CASES <- 0

for(i in 1:length(SJ_Dengue$TOTAL_CASES)){
  lag_days <- 1
  index <- 1
  for (j in 1:length(SJ_Dengue$TOTAL_CASES)){
    lag_days <- strptime(SJ_Dengue[i,4], format = "%Y-%m-%d") - strptime(SJ_Dengue[j,4], format = "%Y-%m-%d")
    if(lag_days==7){
      index <- j
      break
    }
  }
  SJ_Dengue[i,25] <- SJ_Dengue[index,24]
}

for(i in 1:length(IQ_Dengue$TOTAL_CASES)){
  lag_days <- 1
  index <- 1
  for (j in 1:length(IQ_Dengue$TOTAL_CASES)){
    lag_days <- strptime(IQ_Dengue[i,4], format = "%Y-%m-%d") - strptime(IQ_Dengue[j,4], format = "%Y-%m-%d")
    if(lag_days==7){
      index <- j
      break
    }
  }
  IQ_Dengue[i,25] <- IQ_Dengue[index,24]
}


### Adding climate feature lags

#Humidity a month ago
SJ_Dengue$PRE_MONTH_HUMIDITY <- 0
IQ_Dengue$PRE_MONTH_HUMIDITY <- 0

for(i in 1:length(SJ_Dengue$REANALYSIS_SPECIFIC_HUMID_GPERKG)){
  lag_days <- 1
  index <- 1
  for (j in 1:length(SJ_Dengue$REANALYSIS_SPECIFIC_HUMID_GPERKG)){
    lag_days <- strptime(SJ_Dengue[i,4], format = "%Y-%m-%d")-strptime(SJ_Dengue[j,4], format = "%Y-%m-%d")
    if(lag_days==28){
      index <- j
      break
    }
  }
  SJ_Dengue[i,26] <- SJ_Dengue[index,17]
}

for(i in 1:length(IQ_Dengue$REANALYSIS_SPECIFIC_HUMID_GPERKG)){
  lag_days <- 1
  index <- 1
  for (j in 1:length(IQ_Dengue$REANALYSIS_SPECIFIC_HUMID_GPERKG)){
    lag_days <- strptime(IQ_Dengue[i,4], format = "%Y-%m-%d")-strptime(IQ_Dengue[j,4], format = "%Y-%m-%d")
    if(lag_days==28){
      index <- j
      break
    }
  }
  IQ_Dengue[i,26] <- IQ_Dengue[index,17]
}

#Dew point temperature a month ago
SJ_Dengue$PRE_MONTH_DEWPOINT_TEMP <- 0
IQ_Dengue$PRE_MONTH_DEWPOINT_TEMP <- 0

for(i in 1:length(SJ_Dengue$REANALYSIS_DEWPOINT_TEMP_K)){
  lag_days <- 1
  index <- 1
  for (j in 1:length(SJ_Dengue$REANALYSIS_DEWPOINT_TEMP_K)){
    lag_days <- strptime(SJ_Dengue[i,4], format = "%Y-%m-%d")-strptime(SJ_Dengue[j,4], format = "%Y-%m-%d")
    if(lag_days==28){
      index <- j
      break
    }
  }
  SJ_Dengue[i,27] <- SJ_Dengue[index,11]
}

for(i in 1:length(IQ_Dengue$REANALYSIS_DEWPOINT_TEMP_K)){
  lag_days <- 1
  index <- 1
  for (j in 1:length(IQ_Dengue$REANALYSIS_DEWPOINT_TEMP_K)){
    lag_days <- strptime(IQ_Dengue[i,4], format = "%Y-%m-%d")-strptime(IQ_Dengue[j,4], format = "%Y-%m-%d")
    if(lag_days==28){
      index <- j
      break
    }
  }
  IQ_Dengue[i,27] <- IQ_Dengue[index,11]
}

#Station average temperature a month ago
SJ_Dengue$PRE_MONTH_STATION_AVG_TEMP <- 0
IQ_Dengue$PRE_MONTH_STATION_AVG_TEMP <- 0

for(i in 1:length(SJ_Dengue$STATION_AVG_TEMP_C)){
  lag_days <- 1
  index <- 1
  for (j in 1:length(SJ_Dengue$STATION_AVG_TEMP_C)){
    lag_days <- strptime(SJ_Dengue[i,4], format = "%Y-%m-%d")-strptime(SJ_Dengue[j,4], format = "%Y-%m-%d")
    if(lag_days==28){
      index <- j
      break
    }
  }
  SJ_Dengue[i,28] <- SJ_Dengue[index,19]
}

for(i in 1:length(IQ_Dengue$STATION_AVG_TEMP_C)){
  lag_days <- 1
  index <- 1
  for (j in 1:length(IQ_Dengue$STATION_AVG_TEMP_C)){
    lag_days <- strptime(IQ_Dengue[i,4], format = "%Y-%m-%d")-strptime(IQ_Dengue[j,4], format = "%Y-%m-%d")
    if(lag_days==28){
      index <- j
      break
    }
  }
  IQ_Dengue[i,28] <- IQ_Dengue[index,19]
}

#Renalysis maximum air temperature 
SJ_Dengue$PRE_MONTH_MAX_AIR_TEMP <- 0
IQ_Dengue$PRE_MONTH_MAX_AIR_TEMP <- 0

for(i in 1:length(SJ_Dengue$PRE_MONTH_MAX_AIR_TEMP)){
  lag_days <- 1
  index <- 1
  for (j in 1:length(SJ_Dengue$PRE_MONTH_MAX_AIR_TEMP)){
    lag_days <- strptime(SJ_Dengue[i,4], format = "%Y-%m-%d")-strptime(SJ_Dengue[j,4], format = "%Y-%m-%d")
    if(lag_days==28){
      index <- j
      break
    }
  }
  SJ_Dengue[i,29] <- SJ_Dengue[index,12]
}

for(i in 1:length(IQ_Dengue$PRE_MONTH_MAX_AIR_TEMP)){
  lag_days <- 1
  index <- 1
  for (j in 1:length(IQ_Dengue$PRE_MONTH_MAX_AIR_TEMP)){
    lag_days <- strptime(IQ_Dengue[i,4], format = "%Y-%m-%d")-strptime(IQ_Dengue[j,4], format = "%Y-%m-%d")
    if(lag_days==28){
      index <- j
      break
    }
  }
  IQ_Dengue[i,29] <- IQ_Dengue[index,12]
}



#################################### DIMENSION REDUCTION - PRINCIPAL COMPONENT ANALYSIS

#Correlation matrix

correlation_matrix_sj <- 
  select_if(SJ_Dengue,is.numeric)
correlation_plot <- 
  corrplot(cor(correlation_matrix_sj),method = 'shade',type = 'upper', title = 'Correlation Matrix', rect.lwd = '4',tl.cex = 0.6)

correlation_matrix_iq <- 
  select_if(IQ_Dengue,is.numeric)
correlation_plot <- 
  corrplot(cor(correlation_matrix_iq),method = 'shade',type = 'upper', title = 'Correlation Matrix', rect.lwd = '4',tl.cex = 0.6)

# Scree plot

fa.parallel(SJ_Dengue[,-c(1:2,4,24)], fa = "PC", n.iter = 100,
            show.legend = FALSE, main = "Scree plot of parallel analysis")

fa.parallel(IQ_Dengue[,-c(1:2,4,24)], fa = "PC", n.iter = 100,
            show.legend = FALSE, main = "Scree plot of parallel analysis")

#Predictor normalizing
SJ_Dengue_Normalized <- scale(SJ_Dengue[,-c(1:2,4,24)],center = TRUE,scale = TRUE)
IQ_Dengue_Normalized <- scale(IQ_Dengue[,-c(1:2,4,24)],center = TRUE,scale = TRUE)

#Principal components for San Juan
principal_components_SJ <- principal(r= SJ_Dengue_Normalized, nfactors = 6, rotate = "none")

#Principal components for Iquitos
principal_components_IQ <- principal(r= IQ_Dengue_Normalized, nfactors = 6, rotate = "none")

#Rotated components for San Juan
rotated_components_SJ <- principal(r= SJ_Dengue_Normalized, nfactors = 6, rotate="varimax")

#Rotated components for Iquitos
rotated_components_IQ <- principal(r= IQ_Dengue_Normalized, nfactors = 6, rotate="varimax")

#Components scores
head(principal_components_SJ$scores)
head(rotated_components_SJ$scores)

head(principal_components_IQ$scores)
head(rotated_components_IQ$scores)

#Scaling principal components between 0 and 1
SJ_Dengue_Scaled_PC <- normalize(principal_components_SJ$scores, method = "range", range = c(0, 1), margin = 1L, on.constant = "quiet")
SJ_Dengue_Scaled_PC_Full <- cbind(SJ_Dengue_Scaled_PC, TOTAL_CASES = SJ_Dengue$TOTAL_CASES) 

IQ_Dengue_Scaled_PC <- normalize(principal_components_IQ$scores, method = "range", range = c(0, 1), margin = 1L, on.constant = "quiet")
IQ_Dengue_Scaled_PC_Full <- cbind(IQ_Dengue_Scaled_PC, TOTAL_CASES = IQ_Dengue$TOTAL_CASES) 


#Scaling all data including feature engineered data
SJ_Dengue_Scaled <- normalize(SJ_Dengue[,-c(24)], method = "range", range = c(0, 1), margin = 1L, on.constant = "quiet")
SJ_Dengue_Scaled <- cbind(SJ_Dengue_Scaled, TOTAL_CASES = SJ_Dengue$TOTAL_CASES) 

IQ_Dengue_Scaled <- normalize(IQ_Dengue[,-c(24)], method = "range", range = c(0, 1), margin = 1L, on.constant = "quiet")
IQ_Dengue_Scaled <- cbind(IQ_Dengue_Scaled, TOTAL_CASES = IQ_Dengue$TOTAL_CASES)

############################################## STEP 04 - MODEL DEPLOYMENT & PERFORMANCE ANALYSIS ###############################################################

############################################### CLASSIFYING THE CLASS OF TOTAL CASES AS LOW , MEDIUM , HIGH , VERY HIGH ##############################################

####### San Juan #######

Class <- 0
SJ_Dengue_Class <- as.data.frame(cbind(SJ_Dengue,Class))
#summary(SJ_Dengue_Class$TOTAL_CASES)
#Classification Rule:-
# 0 <= TOTAL CASES <= 7 - Low
# 8 <= TOTAL CASES <= 25 - Medium
# 26 <= TOTAL CASES <= 55 - High
# 55 >= Very High

for (i in 1:length(SJ_Dengue_Class$TOTAL_CASES)) {
  if(SJ_Dengue_Class[i,24] <= 7){
    SJ_Dengue_Class[i,30] <- 'Low'
  }else if(SJ_Dengue_Class[i,24] <= 25){
    SJ_Dengue_Class[i,30] <- 'Medium'
  }else if(SJ_Dengue_Class[i,24] <= 55){
    SJ_Dengue_Class[i,30] <- 'High'
  }else{
    SJ_Dengue_Class[i,30] <- 'Very High'
  }
}

counts <- SJ_Dengue_Class %>% group_by(SJ_Dengue_Class$Class) %>% summarise(count = n())
counts

SJ_Dengue_Normalized_Class <- cbind(SJ_Dengue_Normalized,SJ_Dengue_Class$Class)

names(SJ_Dengue_Normalized_Class)[26] <- "Class"

SJ_Dengue_Class = SJ_Dengue_Class[,-c(1,2,4,24)]

######### Iquitos ######

Class <- 0
IQ_Dengue_Class <- as.data.frame(cbind(IQ_Dengue,Class))
#summary(SJ_Dengue_Class$TOTAL_CASES)
#Classification Rule:-
# 0 <= TOTAL CASES <= 7 - Low
# 8 <= TOTAL CASES <= 25 - Medium
# 26 <= TOTAL CASES <= 55 - High
# 55 >= Very High

for (i in 1:length(IQ_Dengue_Class$TOTAL_CASES)) {
  if(IQ_Dengue_Class[i,24] <= 7){
    IQ_Dengue_Class[i,30] <- 'Low'
  }else if(IQ_Dengue_Class[i,24] <= 25){
    IQ_Dengue_Class[i,30] <- 'Medium'
  }else if(IQ_Dengue_Class[i,24] <= 55){
    IQ_Dengue_Class[i,30] <- 'High'
  }else{
    IQ_Dengue_Class[i,30] <- 'Very High'
  }
}

counts <- IQ_Dengue_Class %>% group_by(IQ_Dengue_Class$Class) %>% summarise(count = n())
counts

IQ_Dengue_Normalized_Class <- cbind(IQ_Dengue_Normalized,IQ_Dengue_Class$Class)

names(IQ_Dengue_Normalized_Class)[26] <- "Class"

IQ_Dengue_Class = IQ_Dengue_Class[,-c(1,2,4,24)]

################################################################### 8.1.1 RANDOM FOREST CLASSIFICATION #######################################################

##### San juan ####

# Partition Data
set.seed(1)  
train_index <- sample(c(1:935), 655)   
SJ_Dengue_Class_Train <- SJ_Dengue_Class[train_index,] 
SJ_Dengue_Class_Valid <- SJ_Dengue_Class[-train_index,] 

########## MODEL DEPLOYMENT ##########

SJ_Dengue_Class_rf <- randomForest(as.factor(Class) ~., data = SJ_Dengue_Class_Train, ntree = 600,
                   mtry = 7, nodesize = 5, importance = TRUE)


#Variable importance plot
varImpPlot(SJ_Dengue_Class_rf, type = 1)

#Prediction to validation dataset
SJ_Dengue_Class_rf_pred <- predict(SJ_Dengue_Class_rf, SJ_Dengue_Class_Valid)

SJ_Dengue_Class_Valid$Class <- factor(SJ_Dengue_Class_Valid$Class)

######### PERFORMANCE EVALUATION ##########

# Confusion Matrix

cm <- confusionMatrix(data = SJ_Dengue_Class_rf_pred, reference = SJ_Dengue_Class_Valid$Class)
cm


# ROC Curves for each Class

predictions <- as.data.frame(predict(SJ_Dengue_Class_rf, SJ_Dengue_Class_Valid, type = "prob"))
predictions$predict <- names(predictions)[1:4][apply(predictions[,1:4], 1, which.max)]
predictions$observed <- SJ_Dengue_Class_Valid$Class

roc_Low <- roc(ifelse(predictions$observed=="Low", "Low", "non-Low"), as.numeric(predictions$Low))
plot(roc_Low, col = "orange")
roc_Medium <- roc(ifelse(predictions$observed=="Medium", "Medium", "non-Medium"), as.numeric(predictions$Medium))
lines(roc_Medium, col = "blue")
roc_High<- roc(ifelse(predictions$observed=="High", "High", "non-High"), as.numeric(predictions$High))
lines(roc_High, col = "red")
names(predictions)<-str_replace_all(names(predictions), c(" " = "." , "," = "" ))
roc_VeryHigh <- roc(ifelse(predictions$observed=="Very High", "Very High", "non-Very High"), as.numeric(predictions$Very.High))
lines(roc_VeryHigh, col = "brown")

# Area Under the curve for each class

auc(roc_Low)
auc(roc_Medium)
auc(roc_High)
auc(roc_VeryHigh)

Class <- 0
IQ_Dengue_Class <- as.data.frame(cbind(IQ_Dengue,Class))
#summary(SJ_Dengue_Class$TOTAL_CASES)
#Classification Rule:-
# 0 <= TOTAL CASES <= 7 - Low
# 8 <= TOTAL CASES <= 25 - Medium
# 26 <= TOTAL CASES <= 55 - High
# 55 >= Very High

for (i in 1:length(IQ_Dengue_Class$TOTAL_CASES)) {
  if(IQ_Dengue_Class[i,24] <= 7){
    IQ_Dengue_Class[i,30] <- 'Low'
  }else if(IQ_Dengue_Class[i,24] <= 25){
    IQ_Dengue_Class[i,30] <- 'Medium'
  }else if(IQ_Dengue_Class[i,24] <= 55){
    IQ_Dengue_Class[i,30] <- 'High'
  }else{
    IQ_Dengue_Class[i,30] <- 'Very High'
  }
}

counts <- IQ_Dengue_Class %>% group_by(IQ_Dengue_Class$Class) %>% summarise(count = n())
counts

IQ_Dengue_Normalized_Class <- cbind(IQ_Dengue_Normalized,IQ_Dengue_Class$Class)

names(IQ_Dengue_Normalized_Class)[26] <- "Class"

IQ_Dengue_Class = IQ_Dengue_Class[,-c(1,2,4,24)]

###### Iquitos ######

# Partition Data
set.seed(1)  
train_index <- sample(c(1:520), 365)   
IQ_Dengue_Class_Train <- IQ_Dengue_Class[train_index,] 
IQ_Dengue_Class_Valid <- IQ_Dengue_Class[-train_index,] 

########## MODEL DEPLOYMENT ##########

IQ_Dengue_Class_rf <- randomForest(as.factor(Class) ~., data = IQ_Dengue_Class_Train, ntree = 600,
                                   mtry = 7, nodesize = 5, importance = TRUE)

#Variable importance plot
varImpPlot(IQ_Dengue_Class_rf, type = 1)

#Prediction to validation dataset
IQ_Dengue_Class_rf_pred <- predict(IQ_Dengue_Class_rf, IQ_Dengue_Class_Valid)

IQ_Dengue_Class_Valid$Class <- factor(IQ_Dengue_Class_Valid$Class)

######### PERFORMANCE EVALUATION ##########

# Confusion Matrix

cm <- confusionMatrix(data = IQ_Dengue_Class_rf_pred, reference = IQ_Dengue_Class_Valid$Class)
cm

####################################################################### 8.1.2 K NEAREST NEIGHBORS #####################################################################

#### San Juan###

# Data partitioning

set.seed(1)  
train_index <- sample(c(1:935), 655)   
SJ_Dengue_Norm_Class_Train <- as.data.frame(SJ_Dengue_Normalized_Class[train_index,])
SJ_Dengue_Norm_Class_Valid <- as.data.frame(SJ_Dengue_Normalized_Class[-train_index,])

## MODEL DEPLOYMENT ##

# Applying KNN for K from 1 to 14 in validation to get the most accurate K

accuracy <- 0
for(i in 1:14) {   
  
  knn.pred <- class::knn(SJ_Dengue_Norm_Class_Train[,-c(26)], SJ_Dengue_Norm_Class_Valid[,-c(26)], cl = SJ_Dengue_Norm_Class_Train[, 26], k = i)  
  
  accuracy[i] <- confusionMatrix(knn.pred,factor(SJ_Dengue_Norm_Class_Valid[,26]))$overall[1] 
} 
accuracy

# Most ideal K is observed to be 8, where accuracy os 0.55

#Model with K = 8

knn.pred <- class::knn(SJ_Dengue_Norm_Class_Train[,-c(26)], SJ_Dengue_Norm_Class_Valid[,-c(26)], cl = SJ_Dengue_Norm_Class_Train[, 26], k = 8, prob = TRUE)

## PERFORMANCE EVALUATION ##

# Confusion Matrix

confusionMatrix(knn.pred,factor(SJ_Dengue_Norm_Class_Valid[,26]))

# ROC Curves for each Class
knn.pred
names(knn.pred)
prob <- attr(knn.pred, "prob")
prob <- 2*ifelse(knn.pred == "-1", 1-prob, prob) - 1

observed <- SJ_Dengue_Norm_Class_Valid$Class

roc_Low <- roc(ifelse(observed=="Low", "Low", "non-Low"), as.numeric(prob))
plot(roc_Low, col = "orange")
roc_Medium <- roc(ifelse(observed=="Medium", "Medium", "non-Medium"), as.numeric(prob))
lines(roc_Medium, col = "blue")
roc_High<- roc(ifelse(observed=="High", "High", "non-High"), as.numeric(prob))
lines(roc_High, col = "red")
names(knn.pred)<-str_replace_all(names(knn.pred), c(" " = "." , "," = "" ))
roc_VeryHigh <- roc(ifelse(observed=="Very High", "Very High", "non-Very High"), as.numeric(prob))
lines(roc_VeryHigh, col = "brown")

# Area Under the curve for each class

auc(roc_Low)
auc(roc_Medium)
auc(roc_High)
auc(roc_VeryHigh)

################################################################### 8.1.3 NEURAL NETWORKS ################################################################

###### San Juan ######

# For principal components

Class <- 0

SJ_Dengue_Class <- as.data.frame(cbind(SJ_Dengue_Scaled_PC_Full,Class))

for (i in 1:length(SJ_Dengue_Class$TOTAL_CASES)) {
  if(SJ_Dengue_Class[i,7] <= 7){
    SJ_Dengue_Class[i,8] <- 'Low'
  }else if(SJ_Dengue_Class[i,7] <= 25){
    SJ_Dengue_Class[i,8] <- 'Medium'
  }else if(SJ_Dengue_Class[i,7] <= 55){
    SJ_Dengue_Class[i,8] <- 'High'
  }else{
    SJ_Dengue_Class[i,8] <- 'Very High'
  }
}


# Dummifying response variable

LOW <- 0
SJ_Dengue_Class <- as.data.frame(cbind(SJ_Dengue_Class,LOW))

MEDIUM <- 0
SJ_Dengue_Class <- as.data.frame(cbind(SJ_Dengue_Class,MEDIUM))

HIGH <- 0
SJ_Dengue_Class <- as.data.frame(cbind(SJ_Dengue_Class,HIGH))

VERY_HIGH <- 0
SJ_Dengue_Class <- as.data.frame(cbind(SJ_Dengue_Class,VERY_HIGH))

for (i in 1:length(SJ_Dengue_Class$TOTAL_CASES)) {
  if(SJ_Dengue_Class[i,8] == 'Low'){
    SJ_Dengue_Class[i,9] <- 1
  }else if(SJ_Dengue_Class[i,8] == 'Medium'){
    SJ_Dengue_Class[i,10] <- 1
  }else if(SJ_Dengue_Class[i,8] == 'High'){
    SJ_Dengue_Class[i,11] <- 1
  }else{
    SJ_Dengue_Class[i,12] <- 1
  }
}

SJ_Dengue_Class <- SJ_Dengue_Class[,-c(7)]

# Neural Network with 5 fold cross validation
accuracy_train <- 0
k=5
for(i in 1:k){
  # Partition Data
  set.seed(1)  
  train_index <- sample(c(1:935), 655)   
  kFoldTraining <- as.data.frame(SJ_Dengue_Class[train_index,])
  kFoldValidation <- as.data.frame(SJ_Dengue_Class[-train_index,])
  
  nn[i] <- neuralnet(LOW + MEDIUM + HIGH + VERY_HIGH ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = kFoldTraining, hidden = 6,stepmax=1000000, linear.output = FALSE, algorithm = "backprop", err.fct = "sse", act.fct = "logistic", threshold = 0.01, lifesign = "full",learningrate = 0.01 )

  training.prediction <- compute(nn[i],kFoldValidation[,-c(8:11)])
  training.class <-  apply(training.prediction$net.result,1,which.max)-1
  accuracy_train[i] <- confusionMatrix(training.class,factor(SJ_Dengue_Norm_Class_Valid[,c(8:11)]))$overall[1]
}

training.prediction <- neuralnet :: compute(nn[1],kFoldValidation[,-c(7:10)])
training.class <-  apply(training.prediction$net.result,1,which.max)-1
accuracy_train[i] <- confusionMatrix(training.class,factor(SJ_Dengue_Norm_Class_Valid[,c(8:11)]))$overall[1]


################################################### 8.2 PREDICTING TOTAL NUMBER OF CASES FOR EACH CITY #########################################################



########################################################## 8.2.1 MULTIPLE LINEAR REGRESSION -  #########################################################

###### San Juan ######

#Normalize predictors
SJ_Dengue_Normalized <- scale(SJ_Dengue[,-c(1:2,4,24)],center = TRUE,scale = TRUE)

SJ_Dengue_Normalized <- cbind(SJ_Dengue_Normalized, TOTAL_CASES = SJ_Dengue$TOTAL_CASES) 

set.seed(1)  
train_index <- sample(c(1:935), 655)   
SJ_Dengue_Normalized_Train <- SJ_Dengue_Normalized[train_index,] 
SJ_Dengue_Normalized_Valid <- SJ_Dengue_Normalized[-train_index,] 

SJ_Dengue_Normalized_Train <- data.frame(SJ_Dengue_Normalized_Train)
SJ_Dengue_Normalized_Valid <- data.frame(SJ_Dengue_Normalized_Valid)

######## MODEL DEPLOYMENT #########

## USING STEPWISE REGRESSION ##
SJ_Dengue_lm <- lm(TOTAL_CASES ~ ., data = SJ_Dengue_Normalized_Train) 

options(scipen = 999) 
#summary(fares.lm)
SJ_Dengue_lm_step <- step(SJ_Dengue_lm, direction = "backward") 
summary(SJ_Dengue_lm_step)  

# Model :- TOTAL_CASES = 33.5042 + 1.8107 * WEEK_OF_YEAR - 24.9751 * REANALYSIS_DEWPOINT_TEMP_K + 26.5415 * REANALYSIS_SPECIFIC_HUMID_GPERKG + 1.8695  * STATION_MAX_TEMP_C   + 45.0145 * PRE_WEEK_CASES - 2.4380 * PRE_MONTH_HUMIDITY

## USING EXHAUSTIVE SEARCH ##

SJ_Dengue_exh <- regsubsets(TOTAL_CASES ~ ., data = SJ_Dengue_Normalized_Train, nbest = 1, nvmax = dim(SJ_Dengue_Normalized_Train)[2], method = "exhaustive") 
SJ_Dengue_exh_summary <- summary(SJ_Dengue_exh) 

# models 
SJ_Dengue_exh_summary$which 
SJ_Dengue_exh_summary
# metrics 
SJ_Dengue_exh_summary$rsq 
SJ_Dengue_exh_summary$adjr2 
SJ_Dengue_exh_summary$cp 

# 8 features are selected which have been prominent throughout exhaustive search
SJ_Dengue_exh_1 <- subset(SJ_Dengue_Normalized_Train, select = c(TOTAL_CASES, WEEK_OF_YEAR,REANALYSIS_DEWPOINT_TEMP_K,REANALYSIS_SPECIFIC_HUMID_GPERKG, STATION_MAX_TEMP_C, PRE_WEEK_CASES, PRE_MONTH_HUMIDITY,PRE_MONTH_STATION_AVG_TEMP))

SJ_Dengue_lm_exh_1 <- lm(TOTAL_CASES ~ ., data = SJ_Dengue_exh_1) 

options(scipen = 999) 
summary(SJ_Dengue_lm_exh_1)

#Model:- 33.4947 + 1.7758 * WEEK_OF_YEAR - 25.4482 * REANALYSIS_DEWPOINT_TEMP_K + 26.7668 * REANALYSIS_SPECIFIC_HUMID_GPERKG + 1.6345 * STATION_MAX_TEMP_C  + 44.9644 * PRE_WEEK_CASES - 3.8788 * PRE_MONTH_HUMIDITY + 1.9315 * PRE_MONTH_STATION_AVG_TEMP 
              

##### PERFORMANCE EVALUATION ####

#Accurancy of stepwise model
SJ_Dengue_lm_step_pred <- predict(SJ_Dengue_lm_step, SJ_Dengue_Normalized_Valid) 
accuracy(SJ_Dengue_lm_step_pred, SJ_Dengue_Normalized_Valid$TOTAL_CASES)
#Accuracy of exhaustic search model
SJ_Dengue_lm_exh_1_pred <- predict(SJ_Dengue_lm_exh_1, SJ_Dengue_Normalized_Valid) 
accuracy(SJ_Dengue_lm_exh_1_pred, SJ_Dengue_Normalized_Valid$TOTAL_CASES) 

#Lift chart for stepwise model
gain.step <- gains(SJ_Dengue_Normalized_Valid$TOTAL_CASES, SJ_Dengue_lm_step_pred)
plot(c(0,gain.step$cume.pct.of.total*sum(SJ_Dengue_Normalized_Valid$TOTAL_CASES))~c(0,gain.step$cume.obs), xlab="# cases", ylab="Cumulative total cases", main="Lift Chart for Stepwise Regression", type="l") 
lines(c(0,sum(SJ_Dengue_Normalized_Valid$TOTAL_CASES))~c(0,dim(SJ_Dengue_Normalized_Valid)[1]), col="gray", lty=2) 

#Lift chart for exhaustive search model
gain.exhaust <- gains(SJ_Dengue_Normalized_Valid$TOTAL_CASES, SJ_Dengue_lm_exh_1_pred)
plot(c(0,gain.exhaust$cume.pct.of.total*sum(SJ_Dengue_Normalized_Valid$TOTAL_CASES))~c(0,gain.exhaust$cume.obs), xlab="# cases", ylab="Cumulative total cases", main="Lift Chart for Exhaustive Regression", type="l") 
lines(c(0,sum(SJ_Dengue_Normalized_Valid$TOTAL_CASES))~c(0,dim(SJ_Dengue_Normalized_Valid)[1]), col="gray", lty=2) 



####### Iquitos ########

#Normalize predictors
IQ_Dengue_Normalized <- scale(IQ_Dengue[,-c(1:2,4,24)],center = TRUE,scale = TRUE)

IQ_Dengue_Normalized <- cbind(IQ_Dengue_Normalized, TOTAL_CASES = IQ_Dengue$TOTAL_CASES) 

set.seed(1)  
train_index <- sample(c(1:520), 365)   
IQ_Dengue_Normalized_Train <- IQ_Dengue_Normalized[train_index,] 
IQ_Dengue_Normalized_Valid <- IQ_Dengue_Normalized[-train_index,] 

IQ_Dengue_Normalized_Train <- data.frame(IQ_Dengue_Normalized_Train)
IQ_Dengue_Normalized_Valid <- data.frame(IQ_Dengue_Normalized_Valid)

######## MODEL DEPLOYMENT #########

## USING STEPWISE REGRESSION ##
IQ_Dengue_lm <- lm(TOTAL_CASES ~ ., data = IQ_Dengue_Normalized_Train) 

options(scipen = 999) 
#summary(fares.lm)
IQ_Dengue_lm_step <- step(IQ_Dengue_lm, direction = "backward") 
summary(IQ_Dengue_lm_step)  

# Model :- TOTAL_CASES = 7.1571 - 0.8156 * WEEK_OF_YEAR + 1.0217 * NDVI_SW - 1.9507 * REANALYSIS_MAX_AIR_TEMP_K - 2.2997 * REANALYSIS_RELATIVE_HUMID_PCNT + 1.7450 * REANALYSIS_SPECIFIC_HUMID_GPERKG + 0.5936 * STATION_MAX_TEMP_C   + 5.7477 * PRE_WEEK_CASES - 8.1030 * PRE_MONTH_HUMIDITY + 7.6790 * PRE_MONTH_DEWPOINT_TEMP 

## USING EXHAUSTIVE SEARCH ##

IQ_Dengue_exh <- regsubsets(TOTAL_CASES ~ ., data = IQ_Dengue_Normalized_Train, nbest = 1, nvmax = dim(IQ_Dengue_Normalized_Train)[2], method = "exhaustive") 
IQ_Dengue_exh_summary <- summary(IQ_Dengue_exh) 

# models 
IQ_Dengue_exh_summary$which 
IQ_Dengue_exh_summary
# metrics 
IQ_Dengue_exh_summary$rsq 
IQ_Dengue_exh_summary$adjr2 
IQ_Dengue_exh_summary$cp 

# 11 features are selected which have been prominent throughout exhaustive search

IQ_Dengue_exh_1 <- subset(IQ_Dengue_Normalized_Train, select = c(TOTAL_CASES, WEEK_OF_YEAR,NDVI_SW,REANALYSIS_MAX_AIR_TEMP_K,REANALYSIS_RELATIVE_HUMID_PCNT,REANALYSIS_SPECIFIC_HUMID_GPERKG, REANALYSIS_TDTR_K,STATION_MAX_TEMP_C, PRE_WEEK_CASES, PRE_MONTH_HUMIDITY,PRE_MONTH_DEWPOINT_TEMP))

IQ_Dengue_lm_exh_1 <- lm(TOTAL_CASES ~ ., data = IQ_Dengue_exh_1) 

options(scipen = 999) 
summary(IQ_Dengue_lm_exh_1)

#Model:- TOTAL_CASES = 7.1621 - 0.7678 * WEEK_OF_YEAR + 0.9751 * NDVI_SW - 1.5752 * REANALYSIS_MAX_AIR_TEMP_K  - 2.9440 * REANALYSIS_RELATIVE_HUMID_PCNT + 1.6984 * REANALYSIS_SPECIFIC_HUMID_GPERKG - 1.0600 * REANALYSIS_TDTR_K + 0.5291 * STATION_MAX_TEMP_C + 5.7574 * PRE_WEEK_CASES - 8.0868 * PRE_MONTH_HUMIDITY + 7.5815 * PRE_MONTH_DEWPOINT_TEMP


##### PERFORMANCE EVALUATION ####

#Accurancy of stepwise model

IQ_Dengue_lm_step_pred <- predict(IQ_Dengue_lm_step, IQ_Dengue_Normalized_Valid) 
accuracy(IQ_Dengue_lm_step_pred, IQ_Dengue_Normalized_Valid$TOTAL_CASES)
#Accuracy of exhaustic search model
IQ_Dengue_lm_exh_1_pred <- predict(IQ_Dengue_lm_exh_1, IQ_Dengue_Normalized_Valid) 
accuracy(IQ_Dengue_lm_exh_1_pred, IQ_Dengue_Normalized_Valid$TOTAL_CASES) 

#Lift chart for stepwise model

gain.step <- gains(IQ_Dengue_Normalized_Valid$TOTAL_CASES, IQ_Dengue_lm_step_pred)
plot(c(0,gain.step$cume.pct.of.total*sum(IQ_Dengue_Normalized_Valid$TOTAL_CASES))~c(0,gain.step$cume.obs), xlab="# cases", ylab="Cumulative total cases", main="Lift Chart for Stepwise Regression", type="l") 
lines(c(0,sum(IQ_Dengue_Normalized_Valid$TOTAL_CASES))~c(0,dim(IQ_Dengue_Normalized_Valid)[1]), col="gray", lty=2) 

#Lift chart for exhaustive search model
gain.exhaust <- gains(IQ_Dengue_Normalized_Valid$TOTAL_CASES, IQ_Dengue_lm_exh_1_pred)
plot(c(0,gain.exhaust$cume.pct.of.total*sum(IQ_Dengue_Normalized_Valid$TOTAL_CASES))~c(0,gain.exhaust$cume.obs), xlab="# cases", ylab="Cumulative total cases", main="Lift Chart for Exhaustive Regression", type="l") 
lines(c(0,sum(IQ_Dengue_Normalized_Valid$TOTAL_CASES))~c(0,dim(IQ_Dengue_Normalized_Valid)[1]), col="gray", lty=2) 

######################################################## 8.2.2 K-NEAREST NEIGHBOURS ########################################################

SJ_Dengue_Normalized <- cbind(SJ_Dengue_Normalized, TOTAL_CASES = SJ_Dengue$TOTAL_CASES) 

######## San Juan ######

#### For the full dataset ####

set.seed(1)  
train_index <- sample(c(1:935), 655)   
SJ_Dengue_Normalized_Train <- as.data.frame(SJ_Dengue_Normalized[train_index,])
SJ_Dengue_Normalized_Valid <- as.data.frame(SJ_Dengue_Normalized[-train_index,])

### Accuracy check for from K=1 to K=14 (Within a two week period)

accuracy <- 0
for(i in 1:14) {   
  knn.pred <- class::knn(SJ_Dengue_Normalized_Train[, -26], SJ_Dengue_Normalized_Valid[, -26],cl = SJ_Dengue_Normalized_Train[,26], k = i)  
  accuracy[i]<-MAE(as.numeric(as.character(knn.pred)),SJ_Dengue_Normalized_Valid[,26])
  print(accuracy[i])
} 

#Ideal accuracies are arrived at K=6

#Accuracy on validation dataset
knn.pred <- class::knn(SJ_Dengue_Normalized_Train[, -26], SJ_Dengue_Normalized_Valid[, -26],cl = SJ_Dengue_Normalized_Train[, 26], k = 6)
accuracy(as.numeric(as.character(knn.pred)),SJ_Dengue_Normalized_Valid[,26])

#### For principal components ####

principal_component_scores_SJ <- principal_components_SJ$scores
principal_component_scores_SJ <- cbind(principal_component_scores_SJ, TOTAL_CASES = SJ_Dengue$TOTAL_CASES) 

set.seed(1)  
train_index <- sample(c(1:935), 655)   
principal_component_scores_SJ_Train <- as.data.frame(principal_component_scores_SJ[train_index,])
principal_component_scores_SJ_Valid <- as.data.frame(principal_component_scores_SJ[-train_index,])

### Accuracy check for from K=1 to K=14 (Within a two week period)

accuracy <- 0
for(i in 1:14) {   
  knn.pred <- class::knn(principal_component_scores_SJ_Train[, -7], principal_component_scores_SJ_Valid[, -7],cl = principal_component_scores_SJ_Train[, 7], k = i)  
  accuracy[i]<-MAE(as.numeric(as.character(knn.pred)),principal_component_scores_SJ_Valid[,7])
  print(accuracy[i])
} 

#Ideal accuracies are arrived at K=1 and K=2

#Choosing K=2 to avoid overfitting
knn.pred <- class::knn(principal_component_scores_SJ_Train[, -7], principal_component_scores_SJ_Valid[, -7],cl = principal_component_scores_SJ_Train[, 7], k = 2)

accuracy(as.numeric(as.character(knn.pred)),principal_component_scores_SJ_Valid[,7])

####### Iquitos #######

#### For the full dataset ####

set.seed(1)  
train_index <- sample(c(1:520), 365)   
IQ_Dengue_Normalized_Train <- as.data.frame(IQ_Dengue_Normalized[train_index,])
IQ_Dengue_Normalized_Valid <- as.data.frame(IQ_Dengue_Normalized[-train_index,])

### Accuracy check for from K=1 to K=14 (Within a two week period)

accuracy <- 0
for(i in 1:14) {   
  knn.pred <- class::knn(IQ_Dengue_Normalized_Train[, -26], IQ_Dengue_Normalized_Valid[, -26],cl = IQ_Dengue_Normalized_Train[, 26], k = i)  
  accuracy[i]<-MAE(as.numeric(as.character(knn.pred)),IQ_Dengue_Normalized_Valid[,26])
  print(accuracy[i])
} 

#Ideal accuracies are arrived at K=6

#Accuracy on validation dataset
knn.pred <- class::knn(IQ_Dengue_Normalized_Train[, -26], IQ_Dengue_Normalized_Valid[, -26],cl = IQ_Dengue_Normalized_Train[, 26], k = 6)
accuracy(as.numeric(as.character(knn.pred)),IQ_Dengue_Normalized_Valid[,26])

#### For principal components ####

principal_component_scores_IQ <- principal_components_IQ$scores
principal_component_scores_IQ <- cbind(principal_component_scores_IQ, TOTAL_CASES = IQ_Dengue$TOTAL_CASES) 

set.seed(1)  
train_index <- sample(c(1:520), 365)   
principal_component_scores_IQ_Train <- as.data.frame(principal_component_scores_IQ[train_index,])
principal_component_scores_IQ_Valid <- as.data.frame(principal_component_scores_IQ[-train_index,])

### Accuracy check for from K=1 to K=14 (Within a two week period)

accuracy <- 0
for(i in 1:14) {   
  knn.pred <- class::knn(principal_component_scores_IQ_Train[, -7], principal_component_scores_IQ_Valid[, -7],cl = principal_component_scores_IQ_Train[, 7], k = i)  
  accuracy[i]<-MAE(as.numeric(as.character(knn.pred)),principal_component_scores_IQ_Valid[,7])
  print(accuracy[i])
} 

#Ideal accuracies are arrived at K=3

#Choosing K=3
knn.pred <- class::knn(principal_component_scores_IQ_Train[, -7], principal_component_scores_IQ_Valid[, -7],cl = principal_component_scores_IQ_Train[, 7], k = 3)

accuracy(as.numeric(as.character(knn.pred)),principal_component_scores_IQ_Valid[,7])

####################################################### 8.2.3 REGRESSION TREES ##################################################

##### San Juan #####

SJ_Dengue_Heu <- as.data.frame(SJ_Dengue[,c(3,11,12,17,22,24,25,26,27,28,29)])

##### For heuristically selected subset of features
set.seed(1)  
train_index <- sample(c(1:935), 655)   
SJ_Dengue_Train <- as.data.frame(SJ_Dengue_Heu[train_index,])
SJ_Dengue_Valid <- as.data.frame(SJ_Dengue_Heu[-train_index,])

SJ_Dengue_rt <- rpart(TOTAL_CASES ~ ., method = "anova", data = SJ_Dengue_Train,cp = 0.001,minbucket = 1,maxdepth = 30)

#Full tree 
draw.tree(SJ_Dengue_rt,cex=0.6,size = 2)

pred_train <- predict(SJ_Dengue_rt, newdata = SJ_Dengue_Train)
pred_valid <- predict(SJ_Dengue_rt, newdata = SJ_Dengue_Valid) 

mae_train <- MAE(pred_train, SJ_Dengue_Train$TOTAL_CASES) 
mae_valid <- MAE(pred_valid, SJ_Dengue_Valid$TOTAL_CASES) 
mae_valid
mae_train
error.tr <- pred_train - SJ_Dengue_Train$TOTAL_CASES
error.va <- pred_valid - SJ_Dengue_Valid$TOTAL_CASES
boxplot(error.tr, error.va, main = "Prediction Error of Datasets",
        names = c("Training Set", "Validation Set"), ylab = "Error" )

#Prunning tree
Pruned_SJ_Dengue_rt <- prune(SJ_Dengue_rt,cp = SJ_Dengue_rt$cptable[which.min(SJ_Dengue_rt$cptable[,"xerror"]),"CP"])

draw.tree(Pruned_SJ_Dengue_rt,cex=0.6,size = 2)

######## PERFORMANCE #####
pred_train <- predict(Pruned_SJ_Dengue_rt, newdata = SJ_Dengue_Train)
pred_valid <- predict(Pruned_SJ_Dengue_rt, newdata = SJ_Dengue_Valid)
mae_train <- MAE(pred_train, SJ_Dengue_Train$TOTAL_CASES) 
mae_valid <- MAE(pred_valid, SJ_Dengue_Valid$TOTAL_CASES) 
mae_valid
mae_train

##### For Principal Component Scores
set.seed(1)  
train_index <- sample(c(1:935), 655)   
SJ_Dengue_Train <- as.data.frame(principal_component_scores_SJ[train_index,])
SJ_Dengue_Valid <- as.data.frame(principal_component_scores_SJ[-train_index,])

SJ_Dengue_rt <- rpart(TOTAL_CASES ~ ., method = "anova", data = SJ_Dengue_Train,cp = 0.001,minbucket = 1,maxdepth = 30)

draw.tree(SJ_Dengue_rt,cex=0.6,size = 2)
pred_train <- predict(SJ_Dengue_rt, newdata = SJ_Dengue_Train)
pred_valid <- predict(SJ_Dengue_rt, newdata = SJ_Dengue_Valid) 

mae_train <- MAE(pred_train, SJ_Dengue_Train$TOTAL_CASES) 
mae_valid <- MAE(pred_valid, SJ_Dengue_Valid$TOTAL_CASES) 
mae_valid
mae_train
error.tr <- pred_train - SJ_Dengue_Train$TOTAL_CASES
error.va <- pred_valid - SJ_Dengue_Valid$TOTAL_CASES
boxplot(error.tr, error.va, main = "Prediction Error of Datasets",
        names = c("Training Set", "Validation Set"), ylab = "Error" )

#Prunning tree
Pruned_SJ_Dengue_rt <- prune(SJ_Dengue_rt,cp = SJ_Dengue_rt$cptable[which.min(SJ_Dengue_rt$cptable[,"xerror"]),"CP"])

draw.tree(Pruned_SJ_Dengue_rt,cex=0.6,size = 2)

########### PERFORMANCE EVALUATION ############
pred_train <- predict(Pruned_SJ_Dengue_rt, newdata = SJ_Dengue_Train)
pred_valid <- predict(Pruned_SJ_Dengue_rt, newdata = SJ_Dengue_Valid)
mae_train <- MAE(pred_train, SJ_Dengue_Train$TOTAL_CASES) 
mae_valid <- MAE(pred_valid, SJ_Dengue_Valid$TOTAL_CASES) 
mae_valid
mae_train


######### IQUITOS ######

IQ_Dengue_Heu <- as.data.frame(IQ_Dengue[,c(3,11,13,15,17,19,22,24,25,26,27)])

##### For heuristically selected subset of features

set.seed(1)  
train_index <- sample(c(1:520), 365)   
IQ_Dengue_Train <- as.data.frame(IQ_Dengue_Heu[train_index,])
IQ_Dengue_Valid <- as.data.frame(IQ_Dengue_Heu[-train_index,])

IQ_Dengue_rt <- rpart(TOTAL_CASES ~ ., method = "anova", data = IQ_Dengue_Train,cp = 0.001,minbucket = 1,maxdepth = 30)

#Full tree 
draw.tree(IQ_Dengue_rt,cex=0.6,size = 1)

pred_train <- predict(IQ_Dengue_rt, newdata = IQ_Dengue_Train)
pred_valid <- predict(IQ_Dengue_rt, newdata = IQ_Dengue_Valid) 

mae_train <- MAE(pred_train, IQ_Dengue_Train$TOTAL_CASES) 
mae_valid <- MAE(pred_valid, IQ_Dengue_Valid$TOTAL_CASES) 
mae_valid
mae_train
error.tr <- pred_train - IQ_Dengue_Train$TOTAL_CASES
error.va <- pred_valid - IQ_Dengue_Valid$TOTAL_CASES
boxplot(error.tr, error.va, main = "Prediction Error of Datasets",
        names = c("Training Set", "Validation Set"), ylab = "Error" )

#Prunning tree
Pruned_IQ_Dengue_rt <- prune(IQ_Dengue_rt,cp = IQ_Dengue_rt$cptable[which.min(IQ_Dengue_rt$cptable[,"xerror"]),"CP"])

draw.tree(Pruned_IQ_Dengue_rt,cex=0.6,size = 2)

######## PERFORMANCE #####
pred_train <- predict(Pruned_IQ_Dengue_rt, newdata = IQ_Dengue_Train)
pred_valid <- predict(Pruned_IQ_Dengue_rt, newdata = IQ_Dengue_Valid)
mae_train <- MAE(pred_train, IQ_Dengue_Train$TOTAL_CASES) 
mae_valid <- MAE(pred_valid, IQ_Dengue_Valid$TOTAL_CASES) 
mae_valid
mae_train

##### For Principal Component Scores

set.seed(1)  
train_index <- sample(c(1:520), 365)   
IQ_Dengue_Train <- as.data.frame(principal_component_scores_IQ[train_index,])
IQ_Dengue_Valid <- as.data.frame(principal_component_scores_IQ[-train_index,])

IQ_Dengue_rt <- rpart(TOTAL_CASES ~ ., method = "anova", data = IQ_Dengue_Train,cp = 0.001,minbucket = 1,maxdepth = 30)

draw.tree(IQ_Dengue_rt,cex=0.6,size = 2)
pred_train <- predict(IQ_Dengue_rt, newdata = IQ_Dengue_Train)
pred_valid <- predict(IQ_Dengue_rt, newdata = IQ_Dengue_Valid) 

mae_train <- MAE(pred_train, IQ_Dengue_Train$TOTAL_CASES) 
mae_valid <- MAE(pred_valid, IQ_Dengue_Valid$TOTAL_CASES) 
mae_valid
mae_train
error.tr <- pred_train - IQ_Dengue_Train$TOTAL_CASES
error.va <- pred_valid - IQ_Dengue_Valid$TOTAL_CASES
boxplot(error.tr, error.va, main = "Prediction Error of Datasets",
        names = c("Training Set", "Validation Set"), ylab = "Error" )

#Prunning tree
Pruned_IQ_Dengue_rt <- prune(IQ_Dengue_rt,cp = IQ_Dengue_rt$cptable[which.min(SJ_Dengue_rt$cptable[,"xerror"]),"CP"])

draw.tree(Pruned_IQ_Dengue_rt,cex=0.6,size = 2)

########### PERFORMANCE EVALUATION ############
pred_train <- predict(Pruned_IQ_Dengue_rt, newdata = IQ_Dengue_Train)
pred_valid <- predict(Pruned_IQ_Dengue_rt, newdata = IQ_Dengue_Valid)
mae_train <- MAE(pred_train, IQ_Dengue_Train$TOTAL_CASES) 
mae_valid <- MAE(pred_valid, IQ_Dengue_Valid$TOTAL_CASES) 
mae_valid
mae_train

########################################################## 8.2.4 NEURAL NETWORKS #################################################

#### San Juan ####

#### Neural Network with one hidden layer

####### Neural Network with Principal components
set.seed(1)  
train_index <- sample(c(1:935), 655)   
SJ_Dengue_Scaled_PC_Train <- SJ_Dengue_Scaled_PC_Full[train_index,] 
SJ_Dengue_Scaled_PC_Valid <- SJ_Dengue_Scaled_PC_Full[-train_index,] 

SJ_Dengue_Scaled_PC_Train <- data.frame(SJ_Dengue_Scaled_PC_Train)
SJ_Dengue_Scaled_PC_Valid = as.matrix(SJ_Dengue_Scaled_PC_Valid)

####### MODEL DEPLYMENT ########

set.seed(1)

nn_pca <- neuralnet(TOTAL_CASES ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = SJ_Dengue_Scaled_PC_Train,stepmax=10000000, linear.output = TRUE, hidden =6, algorithm = "backprop", err.fct = "sse", act.fct = "logistic", threshold = 0.01, lifesign = "full",learningrate = 0.01)
prediction(nn_pca)
?neuralnet()
testset <- SJ_Dengue_Scaled_PC_Valid[,c(1:6)]
predict_testNN <- neuralnet::compute(nn_pca, testset)

predict_testNN$net.result

data.frame(SJ_Dengue_Scaled_PC_Train)

##### PERFORMANCE ANALYSIS #####

mae(nn_pca,SJ_Dengue_Scaled_PC_Train)

mae(nn_pca,SJ_Dengue_Scaled_PC_Valid)

###### Neural Network with Heuristically selected Subset of features

set.seed(1)  
train_index <- sample(c(1:935), 655)   
SJ_Dengue_Scaled_Heu_Train <- SJ_Dengue_Scaled[train_index,] 
SJ_Dengue_Scaled_Heu_Valid <- SJ_Dengue_Scaled[-train_index,] 

####### MODEL DEPLYMENT ########

set.seed(1)
nn_heu <- neuralnet(TOTAL_CASES ~ WEEK_OF_YEAR + REANALYSIS_DEWPOINT_TEMP_K + REANALYSIS_SPECIFIC_HUMID_GPERKG + REANALYSIS_MAX_AIR_TEMP_K + STATION_AVG_TEMP_C + PRE_WEEK_CASES+ PRE_MONTH_HUMIDITY + 
                      PRE_MONTH_MAX_AIR_TEMP + PRE_MONTH_DEWPOINT_TEMP + PRE_MONTH_STATION_AVG_TEMP, data = SJ_Dengue_Scaled_Heu_Train, linear.output = T, hidden =10)

nn_heu$result.matrix

##### PERFORMANCE ANALYSIS #####

mae(nn_heu,SJ_Dengue_Scaled_Heu_Valid)

mae(nn_heu,SJ_Dengue_Scaled_Heu_Train)

mae(nn2,SJ_Dengue)


#### Iquitos ####

#### Neural Network with one hidden layer

####### Neural Network for Principal components

set.seed(1)  
train_index <- sample(c(1:520), 365)   
IQ_Dengue_Scaled_PC_Train <- IQ_Dengue_Scaled_PC_Full[train_index,] 
IQ_Dengue_Scaled_PC_Valid <- IQ_Dengue_Scaled_PC_Full[-train_index,] 

IQ_Dengue_Scaled_PC_Train <- data.frame(IQ_Dengue_Scaled_PC_Train)
IQ_Dengue_Scaled_PC_Valid = as.matrix(IQ_Dengue_Scaled_PC_Valid)

####### MODEL DEPLYMENT ########

set.seed(1)

nn_pca <- neuralnet(TOTAL_CASES ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6, data = IQ_Dengue_Scaled_PC_Train,stepmax=10000000, linear.output = TRUE, hidden =6, algorithm = "backprop", err.fct = "sse", act.fct = "logistic", threshold = 0.01, lifesign = "full",learningrate = 0.01)

testset <- IQ_Dengue_Scaled_PC_Valid[,c(1:6)]
predict_testNN <- neuralnet::compute(nn_pca, testset)

predict_testNN$net.result

data.frame(IQ_Dengue_Scaled_PC_Train)

##### PERFORMANCE ANALYSIS #####

mae(nn_pca,IQ_Dengue_Scaled_PC_Train)

mae(nn_pca,IQ_Dengue_Scaled_PC_Valid)

###### Neural Network with Heuristically selected Subset of features

set.seed(1)  
train_index <- sample(c(1:520), 365)   
IQ_Dengue_Scaled_Heu_Train <- IQ_Dengue_Scaled[train_index,] 
IQ_Dengue_Scaled_Heu_Valid <- IQ_Dengue_Scaled[-train_index,] 

####### MODEL DEPLYMENT ########

set.seed(1)
nn_heu <- neuralnet(TOTAL_CASES ~ WEEK_OF_YEAR + REANALYSIS_DEWPOINT_TEMP_K + REANALYSIS_SPECIFIC_HUMID_GPERKG + REANALYSIS_MAX_AIR_TEMP_K + STATION_AVG_TEMP_C + PRE_WEEK_CASES+ PRE_MONTH_HUMIDITY + 
                      PRE_MONTH_MAX_AIR_TEMP + PRE_MONTH_DEWPOINT_TEMP + PRE_MONTH_STATION_AVG_TEMP, data = IQ_Dengue_Scaled_Heu_Train, linear.output = T, hidden =10)

nn_heu$result.matrix

##### PERFORMANCE ANALYSIS #####

mae(nn_heu,IQ_Dengue_Scaled_Heu_Valid)

mae(nn_heu,IQ_Dengue_Scaled_Heu_Train)


#### STEP 05 - FINAL MODEL SELECTION ####

#####





