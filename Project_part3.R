rm(list=ls())
library(tidyverse)
library(readxl)
library(dplyr)
require(gdata)
library(MASS)
library(knitr)
library(ggplot2)
library(VGAM)
library(nnet)

###INITIAL EXPLORATION OF THE DATA
FAA1 = read.xls ("FAA1.xls", sheet = 1, header = TRUE)
FAA2 = read.xls ("FAA2.xls", sheet = 1, header = TRUE)
FAA <- bind_rows(FAA1,FAA2) 

# Removing Duplicates
FAA <- FAA[!duplicated(FAA[c('aircraft','no_pasg','speed_ground','speed_air','height',
                             'pitch','distance')]),]  # There were 100 elements 
# where duplicates were found and removed
###DATA CLEANING AND FURTHER EXPLORATION

FAA <- subset(FAA, (FAA$duration>=40 | is.na(FAA$duration))) # Five rows were removed
FAA <- subset(FAA,((FAA['speed_air']<=140 & FAA['speed_air']>=30)| 
                     is.na(FAA$speed_air))) # one observation removed
FAA <- subset(FAA,((FAA['speed_ground']<=140 & FAA['speed_ground']>=30)| 
                     is.na(FAA$speed_ground))) # two observation removed

FAA <- subset(FAA, (FAA['height']>=6 | is.na(FAA$height)))  #  10 observations removed
FAA <- subset(FAA, (FAA['distance']<6000 | is.na(FAA$distance)))  # 3 observations were removed
str(FAA) # There are 844 observations now.

#Boeing is coded as 0 and Airbus is 1

FAA$aircraft <- ifelse(FAA$aircraft =="boeing",0,1)


#Step 1

FAA$Y <- ifelse(FAA$distance < 1000,1,ifelse((FAA$distance>=1000 & FAA$distance<2500),2,3)) 
 

FAA <- subset( FAA, select = -distance )

FAA$aircraft = as.factor(FAA$aircraft)
FAA$Y = as.factor(FAA$Y)

#Step2


#Step3

# FAA$speed_ground <- (FAA$speed_ground-mean(FAA$speed_ground, na.rm = TRUE))/sd(FAA$speed_ground, na.rm = TRUE)
# FAA$speed_air <- (FAA$speed_air-mean(FAA$speed_air, na.rm = TRUE))/sd(FAA$speed_air, na.rm = TRUE)
# FAA$height <- (FAA$height-mean(FAA$height, na.rm = TRUE))/sd(FAA$height, na.rm = TRUE)
# FAA$pitch <- (FAA$pitch-mean(FAA$pitch, na.rm = TRUE))/sd(FAA$pitch, na.rm = TRUE)
# FAA$no_pasg <- (FAA$no_pasg-mean(FAA$no_pasg, na.rm = TRUE))/sd(FAA$no_pasg, na.rm = TRUE)
# FAA$duration <- (FAA$duration-mean(FAA$duration, na.rm = TRUE))/sd(FAA$duration, na.rm = TRUE)

head(FAA)

FAA <- na.omit(FAA)
modl <- multinom(Y~.,FAA)
summary(modl)

FAA_new <- subset(FAA, select = -speed_air)
modl2 <- multinom(Y~.,FAA_new)
summary(modl2)
modl_reduced <- step(modl2)

deviance(modl_reduced)-deviance(modl2)

modl3 <- multinom(Y ~ pitch+height+aircraft+speed_ground, FAA_new)
summary(modl3)


duration <- tapply(FAA_new$duration, FAA_new$Y, mean, na.rm=TRUE)
no_pasg <- tapply(FAA_new$no_pasg, FAA_new$Y, mean, na.rm=TRUE)
speed_ground <- tapply(FAA_new$speed_ground, FAA_new$Y, mean, na.rm=TRUE)
height <- tapply(FAA_new$height, FAA_new$Y, mean, na.rm=TRUE)
pitch <- tapply(FAA_new$pitch, FAA_new$Y, mean, na.rm=TRUE)

FAA_test <- subset(FAA_new, select = -Y)


xtabs(~predict(modl_reduced) + FAA_new$Y)

FAA_test$Y1 <- predict(modl_reduced, FAA_new)

ggplot(FAA_new, aes(x=aircraft, fill=Y)) + 
  geom_bar(position = 'dodge')
      

ggplot(FAA_test, aes(x=pitch, y=Y1)) + 
  geom_point()

FAA_new$Y <- ifelse(FAA_new$distance < 1000,1,ifelse((FAA_new$distance>=1000 & FAA_new$distance<2500),2,3)) 
FAA_new2 <- subset(FAA_new, select = -distance)
model_ordinal<-vglm(Y ~ .,
                       family=cumulative(parallel=TRUE), FAA_new2)
summary(model_ordinal)
