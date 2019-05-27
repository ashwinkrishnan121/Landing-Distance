rm(list=ls())
library(tidyverse)
library(readxl)
library(dplyr)
require(gdata)
library(MASS)
library(knitr)
library(ggplot2)
library(ROCR)

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
FAA$long.landing  <- ifelse(FAA$distance > 2500, 1, 0)

FAA$risky.landing  <- ifelse(FAA$distance > 3000, 1, 0)

FAA <- subset( FAA, select = -distance )

#Step2
ggplot(data=FAA, aes(FAA$long.landing)) + geom_histogram(bins = 3)

#Step3
lmod<-glm(long.landing ~ .,family=binomial,FAA)
summary(lmod)

FAA$speed_ground <- (FAA$speed_ground-mean(FAA$speed_ground, na.rm = TRUE))/sd(FAA$speed_ground, na.rm = TRUE)
FAA$speed_air <- (FAA$speed_air-mean(FAA$speed_air, na.rm = TRUE))/sd(FAA$speed_air, na.rm = TRUE)
FAA$height <- (FAA$height-mean(FAA$height, na.rm = TRUE))/sd(FAA$height, na.rm = TRUE)
FAA$pitch <- (FAA$pitch-mean(FAA$pitch, na.rm = TRUE))/sd(FAA$pitch, na.rm = TRUE)
FAA$no_pasg <- (FAA$no_pasg-mean(FAA$no_pasg, na.rm = TRUE))/sd(FAA$no_pasg, na.rm = TRUE)
FAA$duration <- (FAA$duration-mean(FAA$duration, na.rm = TRUE))/sd(FAA$duration, na.rm = TRUE)

FAA_mod <- FAA[1:8]
lmod<-glm(long.landing ~ .,family=binomial,FAA_mod)
summary(lmod)


table1 <- data.frame()

for(i in 1:7)
{
  
  model <- glm(long.landing~FAA[,i], FAA, family=binomial)
  magnitude_coeff <- abs(model$coefficients[2])
  odds_ratio <- exp(model$coefficients[2])
  direction_coeff <- ifelse(sign(model$coefficients[2])>0,"+","-")
  p_value <- summary(model)$coefficients[-1,c(1,4)][2]
}

table1 <-
  table1 %>% 
  mutate(variable = str_replace(variable, "aircraft", "aircraft_boeing")) %>% 
  arrange(p_value)

kable(table1)

#step 4

eda_Plot <- function(x)
{
  ggplot(FAA, aes(x=x, fill=long.landing)) +
    geom_histogram(position = 'dodge', binwidth = 1
}

eda_Plot(FAA$pitch)




full_mdl <- glm(long.landing ~ .,
                  data=FAA_mod,
                  family=binomial)

summary(full_mdl)

full_mdl2 <- glm(long.landing ~ speed_ground+speed_air+aircraft+pitch,
                 data=FAA_mod,
                 family=binomial)
summary(full_mdl2)



FAA_mod2 <- subset(FAA_mod, select = -speed_air)
                
FAA_mod2 <- na.omit(FAA_mod2)
full_mdl_landing <- glm(long.landing ~ .,
                  data=FAA_mod2,
                  family=binomial)

AIC_mdl <- step(full_mdl)

BIC_mdl_landing <- step(full_mdl_landing, k=log(nrow(FAA_mod2)))
summary(BIC_mdl)
AIC(BIC_mdl)

hist_Plot(FAA$height)


ggplot(data=FAA, aes(FAA$risky.landing)) + geom_histogram(bins = 3)



for(i in 1:7)
{
  
  model <- glm(risky.landing~FAA[,i], FAA, family=binomial)
  magnitude_coeff [i]<- abs(model$coefficients[2])
  odds_ratio[i] <- exp(model$coefficients[2])
  p_value[i] <- summary(model)$coefficients[-1,c(1,4)][2]
}

FAA_mod2 <- subset(FAA, select = c(-long.landing,-speed_air))
FAA_mod2 <- na.omit(FAA_mod2)
full_mdl_risky <- glm(risky.landing ~ .,
                data=FAA_mod2,
                family=binomial)
AIC_mdl <- step(full_mdl)
summary(AIC_mdl)
BIC_mdl_risky <- step(full_mdl_risky, k=log(nrow(FAA_mod2)))
AIC(BIC_mdl)
summary(BIC_mdl)


#STEP 12

par(mfrow=c(1,2))

FAA_long <- na.omit(subset(FAA, select = c(-speed_air, -risky.landing)))
FAA_risky <- na.omit(subset(FAA, select = c(-speed_air, -long.landing)))

pred1 <- prediction(predict(BIC_mdl_landing), FAA_long$long.landing)
perf <- performance(pred1, "tpr", "fpr")
plot(perf, colorize=TRUE)

pred2 <- prediction(predict(BIC_mdl_risky), FAA_risky$risky.landing)
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2, add = TRUE, colorize = TRUE)


auc_long <- unlist(slot(performance(pred1, "auc"), "y.values"))
auc_risky <- unlist(slot(performance(pred2, "auc"), "y.values"))

#Step 13
df <- data.frame(aircraft = 0, duration=200, no_pasg=80, speed_ground=115, speed_air=120,
height=40, pitch=4)

prob_long <- predict(BIC_mdl_landing, newdata=df, type="response", se=T)
CI_long <- c((prob_long$fit-(1.96*prob_long$se.fit)),(prob_long$fit+(1.96*prob_long$se.fit)))
prob_risky <- predict(BIC_mdl_risky, newdata=df, type="response", se=T)
CI_risky <- c((prob_risky$fit-(1.96*prob_risky$se.fit)),(prob_risky$fit+(1.96*prob_risky$se.fit)))


#STEP 14:

mdl_probit <- glm(risky.landing ~ aircraft+speed_ground, 
                    family=binomial (link = "probit"),
                    data=FAA_risky)

mdl_hazard <- glm(risky.landing ~ aircraft+speed_ground, 
                    family=binomial (link = "cloglog"),
                    data=FAA_risky)

summary(BIC_mdl_risky)
summary(mdl_probit)
summary(mdl_hazard)


#STEP 15:

pred2 <- prediction(predict(BIC_mdl_risky), FAA_risky$risky.landing)
perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2, colorize = TRUE)

pred3 <- prediction(predict(mdl_probit), FAA_risky$risky.landing)
perf3 <- performance(pred3, "tpr", "fpr")
plot(perf3, add = TRUE, colorize = TRUE)

pred4 <- prediction(predict(mdl_hazard), FAA_risky$risky.landing)
perf4 <- performance(pred4, "tpr", "fpr")
plot(perf4, add = TRUE, colorize = TRUE)


auc_risky_logit <- unlist(slot(performance(pred2, "auc"), "y.values"))
auc_risky_probit <- unlist(slot(performance(pred3, "auc"), "y.values"))
auc_risky_loglog <- unlist(slot(performance(pred4, "auc"), "y.values"))

prob_risky_logit <- predict(BIC_mdl_risky, type = "response")
prob_risky_probit <- predict(mdl_probit, type = "response")
prob_risky_hazard <- predict(mdl_hazard, type = "response")

head(sort(prob_risky_logit, decreasing = TRUE),5)


head(sort(prob_risky_probit, decreasing = TRUE),5)

head(sort(prob_risky_hazard, decreasing = TRUE),5)

#Step 17
prob_risky <- predict(BIC_mdl_risky, newdata=df, type="response", se=T)
CI_risky <- c((prob_risky$fit-(1.96*prob_risky$se.fit)),(prob_risky$fit+(1.96*prob_risky$se.fit)))

prob_risky_probit <- predict(mdl_probit, newdata=df, type="response", se=T)
CI_risky_probit <- c((prob_risky_probit$fit-(1.96*prob_risky_probit$se.fit)),(prob_risky_probit$fit+(1.96*prob_risky_probit$se.fit)))

prob_risky_hazard <- predict(mdl_hazard, newdata=df, type="response", se=T)
CI_risky_hazard <- c((prob_risky_hazard$fit-(1.96*prob_risky_hazard$se.fit)),(prob_risky_hazard$fit+(1.96*prob_risky_hazard$se.fit)))
