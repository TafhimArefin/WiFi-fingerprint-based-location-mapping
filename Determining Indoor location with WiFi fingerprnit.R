#WiFi based Locationing
#Minhaz 
#V1


# Setting Directory -------------------------------------------------------

setwd("/Users/tafhim/Documents/Ubiqum/3.2 WiFi Locationing")


# Library  ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(caret)
library(ggplot2)


# Import Dataset ----------------------------------------------------------

Initial_Data_test <- read.csv("UJIndoorLoc/trainingData.csv")

Initial_Data_validation <- read.csv("UJIndoorLoc/validationData.csv")



# Check DataQuality -------------------------------------------------------



# Data Slicing by Building number -----------------------------------------

# Test

Building0_Data_test <- Initial_Data_test %>% filter(BUILDINGID == 0)

Building1_Data_test <- Initial_Data_test %>% filter(BUILDINGID == 1)

Building2_Data_test <- Initial_Data_test %>% filter(BUILDINGID == 2)

# VALIDATION

Building0_Data_validation <- Initial_Data_validation %>% filter(BUILDINGID == 0)

Building1_Data_validation <- Initial_Data_validation %>% filter(BUILDINGID == 1)

Building2_Data_validation <- Initial_Data_validation %>% filter(BUILDINGID == 2)


# Filter by Columns

# drop longitude test

Building0_Data_test_Latitude <- Building0_Data_test %>% 
  select(-LONGITUDE)

Building1_Data_test_Latitude <- Building1_Data_test %>% 
  select(-LONGITUDE)

Building2_Data_test_Latitude <- Building2_Data_test %>% 
  select(-LONGITUDE)

# drop longitude valid

Building0_Data_valid_Latitude <- Building0_Data_validation %>% 
  select(-LONGITUDE)

Building1_Data_valid_Latitude <- Building1_Data_validation %>% 
  select(-LONGITUDE)

Building2_Data_valid_Latitude <- Building2_Data_validation %>% 
  select(-LONGITUDE)

# Drop Latitude test

Building0_Data_test_Longitude <- Building0_Data_test %>% 
  select(-LATITUDE)

Building1_Data_test_Longitude <- Building1_Data_test %>% 
  select(-LATITUDE)

Building2_Data_test_Longitude <- Building2_Data_test %>% 
  select(-LATITUDE)


# Drop Latitude test

Building0_Data_valid_Longitude <- Building0_Data_validation %>% 
  select(-LATITUDE)

Building1_Data_valid_Longitude <- Building1_Data_validation %>% 
  select(-LATITUDE)

Building2_Data_valid_Longitude <- Building2_Data_validation %>% 
  select(-LATITUDE)

# Testing long data -------------------------------------------------------

# Test

Long_Building0_Data_test_Latitude <-
  Building0_Data_test_Latitude %>% 
  gather(WAP.ID, SIG.STRENGTH, WAP001:WAP520)

# Validation

Long_Building0_Data_valid_Latitude <-
  Building0_Data_valid_Latitude %>% 
  gather(WAP.ID, SIG.STRENGTH, WAP001:WAP520)



# CONVERT WAP.ID IN Numeric

#test

Long_Building0_Data_test_Latitude <- 
  Long_Building0_Data_test_Latitude %>%
  mutate(SIG.STRENGTH = as.numeric(SIG.STRENGTH))


#Validation

Long_Building0_Data_valid_Latitude <- 
  Long_Building0_Data_valid_Latitude %>%
  mutate(SIG.STRENGTH = as.numeric(SIG.STRENGTH))

# Remove Rows with Sig.Strength EQUAL 100

# test

Long_Building0_Data_test_Latitude_woSIG.Str <- 
  Long_Building0_Data_test_Latitude %>% 
  filter(SIG.STRENGTH < 100)

# validation

Long_Building0_Data_valid_Latitude_woSIG.Str <- 
  Long_Building0_Data_valid_Latitude %>% 
  filter(SIG.STRENGTH < 100)

# Create df for building 0 and floor 0

# Test

Lng_Build.0_FL.0_test_Lat._woSIG.Str <- 
  Long_Building0_Data_test_Latitude_woSIG.Str %>%
  filter(FLOOR == 0)

# Validation

Lng_Build.0_FL.0_valid_Lat._woSIG.Str <- 
  Long_Building0_Data_valid_Latitude_woSIG.Str %>%
  filter(FLOOR == 0)

# Linear Model for Building 0, Floor 0, Latitude

# Setting Seed

set.seed(999)

# Define Cross Validation

WiFi_fitControl<- 
  trainControl(method = "repeatedcv", number = 10, repeats = 10)

# Run an LM Model Building 0 and floor 0

B0_F0_LM_Latitude <- train(LATITUDE~SIG.STRENGTH,
                           data = Lng_Build.0_FL.0_test_Lat._woSIG.Str,
                           method = "lm", trControl = WiFi_fitControl)

B0_F0_LM_Latitude_Pred. <- predict(B0_F0_LM_Latitude, Lng_Build.0_FL.0_valid_Lat._woSIG.Str)

postResample(B0_F0_LM_Latitude_Pred., 
             Lng_Build.0_FL.0_valid_Lat._woSIG.Str$LATITUDE)

# Run an KNN Model Building 0 and Floor 0

B0_F0_KNN_Latitude <- train(LATITUDE~SIG.STRENGTH,
                           data = Lng_Build.0_FL.0_test_Lat._woSIG.Str,
                           method = "knn", trControl = WiFi_fitControl)

B0_F0_KNN_Latitude_Pred. <- predict(B0_F0_KNN_Latitude, Lng_Build.0_FL.0_valid_Lat._woSIG.Str)

postResample(B0_F0_KNN_Latitude_Pred., 
             Lng_Build.0_FL.0_valid_Lat._woSIG.Str$LATITUDE)
