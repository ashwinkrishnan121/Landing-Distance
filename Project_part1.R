rm(list=ls())
library(tidyverse)
library(readxl)
library(dplyr)
require(gdata)
library(MASS)

###INITIAL EXPLORATION OF THE DATA
FAA1 = read.xls ("FAA1.xls", sheet = 1, header = TRUE)
FAA2 = read.xls ("FAA2.xls", sheet = 1, header = TRUE)
head(FAA1) # There are 8 variables: aircraft, duration, no_psg, speed_ground
           #speed_air, height, pitch, distance
str(FAA1) # 1. There are 800 observations
          #2. Two kinds of aircraft:airbus and boeing.Aircraft is factor variable
          #3. no_psg is integer variable and rest are numeric.
head(FAA2) # Same variables are FAA1 except duration which is absent in FAA2
str(FAA2)  # 1. 150 Observations.
           # 2. The data type of variable same as FAA1

#Merging the two data frames row-wise
FAA <- bind_rows(FAA1,FAA2) 
# Removing Duplicates
FAA <- FAA[!duplicated(FAA[c('aircraft','no_pasg','speed_ground','speed_air','height',
         'pitch','distance')]),]  # There were 100 elements 
                                  # where duplicates were found and removed
summary(FAA)
# 1. There are 450 airbus and 400 boeing flights
# 2. The duration column has 50 NA's. The minimum duration is 14.76 and maximum 
#is 305.62. The mean is 154.01 and median is 154.01
# 3. The no_pasg is minimum 29 and maximum 87. The mean and median are 60 and 60.1 
#  4. The speed_ground has minimum of 27.74, maximum of 141.22 and mean and 
#median of 79.64 and 79.45. 
# 5.The speed_air has minimum 90, maximum of 141.72. The mean and median values
#are 101.15 and 103.80. There is some difference between the mean and median value
#The number of NAs is 642, which is very high. Therefore the data sample is 
#less and hence the mean and median may not be accurate. Also the summary statistic
#between speed_air and speed_ground is differing a lot.
#6.The height variable has mimium of -3.546, which is not possible. The maximum is
#59.946. The mean and median are 30.144 and 30.093.
#7.The pitch variable has minimum 2.284, max value of 5.927. The mean and median are
#4.009 and 4.008.
#8.The distance variable has minimum of 34.08, maximum of 6533.05 and mean and 
#median of 1526.02 and 1258.09. The minimum value is too small. The mean and median
#are differing a lot showing the presence of outliers in the data set.

###DATA CLEANING AND FURTHER EXPLORATION

FAA <- subset(FAA, (FAA$duration>=40 | is.na(FAA$duration))) # Five rows were removed
FAA <- subset(FAA,((FAA['speed_air']<=140 & FAA['speed_air']>=30)| 
                     is.na(FAA$speed_air))) # one observation removed
FAA <- subset(FAA,((FAA['speed_ground']<=140 & FAA['speed_ground']>=30)| 
                     is.na(FAA$speed_ground))) # two observation removed

FAA <- subset(FAA, (FAA['height']>=6 | is.na(FAA$height)))  #  10 observations removed
FAA <- subset(FAA, (FAA['distance']<6000 | is.na(FAA$distance)))  # 3 observations were removed
str(FAA) # There are 844 observations now.
summary(FAA$aircraft)
summary(FAA$duration)
summary(FAA$no_pasg)
summary(FAA$speed_ground)
summary(FAA$speed_air)
summary(FAA$height)
summary(FAA$pitch)
summary(FAA$distance)

?histogram
hist(FAA$speed_ground, xaxt='n')
hist(FAA$speed_air)
hist(FAA$height)
hist(FAA$pitch)
hist(FAA$distance)

ggplot(data=FAA, aes(FAA$speed_ground)) + geom_histogram(binwidth = 5)
ggplot(data=FAA, aes(FAA$speed_air)) + geom_histogram(binwidth = 5)
ggplot(data=FAA, aes(FAA$height)) + geom_histogram(binwidth = 5)
ggplot(data=FAA, aes(FAA$pitch)) + geom_histogram(binwidth = 0.2)
ggplot(data=FAA, aes(FAA$distance)) + geom_histogram(binwidth = 200)

cor(FAA$distance, FAA$speed_air, use = "pairwise.complete.obs")
cor(FAA$distance, FAA$speed_ground, use = "pairwise.complete.obs")
cor(FAA$distance, FAA$height, use = "pairwise.complete.obs")
cor(FAA$distance, FAA$pitch, use = "pairwise.complete.obs")
cor(FAA$distance, FAA$aircraft, use = "pairwise.complete.obs")
cor(FAA$distance, FAA$no_pasg, use = "pairwise.complete.obs")
cor(FAA$distance, FAA$duration, use = "pairwise.complete.obs")

ggplot(data=FAA, aes(FAA$distance, FAA$speed_ground)) + geom_point()
ggplot(data=FAA, aes(FAA$distance,FAA$speed_air)) + geom_point()
ggplot(data=FAA, aes(FAA$distance,FAA$height)) + geom_point()
ggplot(data=FAA, aes(FAA$distance,FAA$pitch)) + geom_point()
ggplot(data=FAA, aes(FAA$distance, FAA$aircraft)) + geom_point()

linearMod <- lm(distance ~ speed_ground+speed_air+height+pitch+aircraft+no_pasg+duration, data = FAA) 
summary(linearMod)

FAA$speed_ground <- (FAA$speed_ground-mean(FAA$speed_ground, na.rm = TRUE))/sd(FAA$speed_ground, na.rm = True)
FAA$speed_air <- (FAA$speed_air-mean(FAA$speed_air, na.rm = TRUE))/sd(FAA$speed_air, na.rm = TRUE)
FAA$height <- (FAA$height-mean(FAA$height, na.rm = TRUE))/sd(FAA$height, na.rm = TRUE)
FAA$pitch <- (FAA$pitch-mean(FAA$pitch, na.rm = TRUE))/sd(FAA$pitch, na.rm = TRUE)
FAA$no_pasg <- (FAA$no_pasg-mean(FAA$no_pasg, na.rm = TRUE))/sd(FAA$no_pasg, na.rm = TRUE)
FAA$duration <- (FAA$duration-mean(FAA$duration, na.rm = TRUE))/sd(FAA$duration, na.rm=TRUE)

linearMod <- lm(distance ~ speed_ground+speed_air+height+pitch+aircraft+no_pasg+duration, data = FAA) 
summary(linearMod)

Model1 <- lm(distance ~ speed_ground, data=FAA)
Model2 <- lm(distance ~ speed_air, data=FAA)
Model3 <- lm(distance ~ speed_ground+speed_air, data=FAA)

summary(Model1)
summary(Model2)
summary(Model3)

cor(FAA$speed_air, FAA$speed_ground, use = "pairwise.complete.obs")

M1 <- lm(distance ~ speed_air, data=FAA)
M2 <- lm(distance ~ speed_air+height, data=FAA)
M3 <- lm(distance ~ speed_air+height+no_pasg, data=FAA)
M4 <- lm(distance ~ speed_air+height+no_pasg+pitch, data=FAA)
M5 <- lm(distance ~ speed_air+height+no_pasg+pitch+duration, data=FAA)
M6 <- lm(distance ~ speed_air+height+no_pasg+pitch+duration+speed_ground, data=FAA)

summary(M1)
summary(M2)
summary(M3)
summary(M4)
summary(M5)
summary(M6)

AIC(M1)
AIC(M2)
AIC(M3)
AIC(M4)
AIC(M5)
AIC(M6)



AIC <- stepAIC(M6, direction = 'forward')
summary(AIC)

M1 <- lm(distance ~ speed_ground, data=FAA)
M2 <- lm(distance ~ speed_ground+aircraft, data=FAA)
M3 <- lm(distance ~ speed_ground+aircraft+height, data=FAA)
M4 <- lm(distance ~ speed_ground+aircraft+height+no_pasg, data=FAA)
M5 <- lm(distance ~ speed_ground+aircraft+height+no_pasg+pitch, data=FAA)
M6 <- lm(distance ~ speed_ground+aircraft+height+no_pasg+pitch+duration, data=FAA)

