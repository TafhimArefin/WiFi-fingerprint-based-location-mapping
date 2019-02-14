#WiFi based Locationing
#Minhaz 
#V1


# Setting Directory -------------------------------------------------------

setwd("/Users/tafhim/Documents/Ubiqum/3.2 WiFi Locationing")


# Library  ----------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)


# Import Dataset ----------------------------------------------------------

Initial_Data_test <- read.csv("UJIndoorLoc/trainingData.csv")

Initial_Data_validation <- read.csv("UJIndoorLoc/validationData.csv")



# Check DataQuality -------------------------------------------------------



# Data Slicing by Building number -----------------------------------------

Building0_Data_test <- Initial_Data_test %>% filter(BUILDINGID == 0)

Building1_Data_test <- Initial_Data_test %>% filter(BUILDINGID == 1)

Building2_Data_test <- Initial_Data_test %>% filter(BUILDINGID == 2)


# Filter by Columns

# drop Latitude

Building0_Data_test_Latitude <- Building0_Data_test %>% 
  select(-LONGITUDE)



