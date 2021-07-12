# Final Project:
# GLM Analysis:


# loading the needed packages
library(tidyr)
library(dplyr)
library(ggplot2)
library(vcd)
library(tidyverse)
library(scatterplot3d)
library(sjmisc)
library(hesim)
library(plot3D)
library(rgl)
library("car")
library(mgcv)
library(MASS)
library(stepPenal)
library(performance)
library(pscl)
library(Metrics)
library(xtable)
library("countreg") # to install it : install.packages("countreg", repos="http://R-Forge.R-project.org")

#------------------------------------------------------------------------------------
#please load your data  here
# loading the data: 
load("/Users/Mac/Downloads/new_data_MRM.RData")
#------------------------------------------------------------------------------------


#aggregating some variables:
data_train_DF <- data_train_DF %>% dplyr::rename(
  NSwind=clim1, WEwind=clim2, dew_temperature=clim3, temperature=clim4, 
  potential_evaporation=clim5, solar_radiation= clim6, 
  thermal_radiation=clim7, pressure=clim8, evaporation=clim9, 
  precipitation=clim10)
str(data_train_DF)

# setting our seed 
set.seed(42)
#sub_sample <- c(1995, 1997, 2013, 2005, 2015)
Known_data<-data_train_DF%>%filter(!is.na(CNT))
dim(Known_data)

#take only 10% of the data making it 48000 dates:
Known_data <-Known_data %>% sample_frac(.2)
dim(Known_data)
#Seperating the data in 80/20 training and test data: 

#We first mix the data to avoid biais:

rows<-sample(nrow(Known_data))
Known_data<-Known_data[rows,]
dim(Known_data)

#cut the data
head(Known_data)
split<-round(nrow(Known_data)*0.8)
split

Training_df<-Known_data[1:split,];
dim(Training_df)

Test_df<-Known_data[(split+1):nrow(Known_data),]
dim(Test_df)

Dimensions<-nrow(Test_df)+nrow(Training_df)==nrow(Known_data)
Dimensions

# to do a synthesis of all the models and collect the relevant informations:
results_models <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(results_models)<-t(c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values"))

#creating a new data set: same for training and test data
df <- Training_df %>% 
  dplyr::select(-BA) %>%
  #filter(year %in% sub_sample) %>%
  mutate(humidity=100*exp(17.625*(dew_temperature-273.15)/(dew_temperature-39.11))/exp(17.625*(temperature-273.15)/(temperature-39.11))) %>% # relative humidity
  mutate(Wspeed=(sqrt(NSwind^2+WEwind^2))) %>%
  dplyr::select(-NSwind, -WEwind)
dim(df)
head(df)

Test_df_filtered <- Test_df %>% 
  dplyr::select(-BA) %>%
  # filter(year %in% sub_sample) %>%
  mutate(humidity=100*exp(17.625*(dew_temperature-273.15)/(dew_temperature-39.11))/exp(17.625*(temperature-273.15)/(temperature-39.11))) %>% # relative humidity
  mutate(Wspeed=(sqrt(NSwind^2+WEwind^2))) %>%
  dplyr::select(-NSwind, -WEwind)
dim(Test_df_filtered)

colnames(df)


#aggregating variables: for both training and test data
df <- df %>% 
  mutate(lc_Cropland=lc1+lc2+lc3) %>%
  mutate(lc_Vegetation_Bare_areas=lc4+lc13+lc17) %>%
  mutate(lc_Tree_broadleaved_evergreen=lc5) %>%
  mutate(lc_Tree_broadleaved_deciduous=lc6) %>%
  mutate(lc_Tree_needleleave_evergreen=lc7) %>%
  mutate(lc_Tree_needleleave_deciduous=lc8) %>%
  mutate(lc_Tree_mixed=lc9) %>%
  mutate(lc_Grassland=lc12) %>%
  mutate(lc_Shrubs=lc10+lc11) %>%
  mutate(lc_Urban=lc16) %>%
  mutate(lc_Water_and_flooded_areas=lc14+lc15+lc18) %>%
  dplyr::select(-c(lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12, lc13, lc14, lc15, lc16, lc17, lc18))


test<- Test_df_filtered  %>% mutate(lc_Cropland=lc1+lc2+lc3) %>%
  mutate(lc_Vegetation_Bare_areas=lc4+lc13+lc17) %>%
  mutate(lc_Tree_broadleaved_evergreen=lc5) %>%
  mutate(lc_Tree_broadleaved_deciduous=lc6) %>%
  mutate(lc_Tree_needleleave_evergreen=lc7) %>%
  mutate(lc_Tree_needleleave_deciduous=lc8) %>%
  mutate(lc_Tree_mixed=lc9) %>%
  mutate(lc_Grassland=lc12) %>%
  mutate(lc_Shrubs=lc10+lc11) %>%
  mutate(lc_Urban=lc16) %>%
  mutate(lc_Water_and_flooded_areas=lc14+lc15+lc18) %>%
  dplyr::select(-c(lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12, lc13, lc14, lc15, lc16, lc17, lc18))

colnames(df)


# We also consider the following interaction formula 
formula_interaction<-as.formula(CNT ~lon+lat+area+year+month+
                                  altiMean+altiSD+dew_temperature+
                                  temperature+potential_evaporation+
                                  solar_radiation+thermal_radiation+
                                  pressure+evaporation+precipitation+
                                  humidity+Wspeed+lc_Cropland+
                                  lc_Vegetation_Bare_areas+
                                  lc_Tree_broadleaved_evergreen+
                                  lc_Tree_broadleaved_deciduous+
                                  lc_Tree_needleleave_deciduous+
                                  lc_Tree_mixed+lc_Grassland+
                                  lc_Shrubs+lc_Urban+
                                  lc_Water_and_flooded_areas+
                                  lc_Tree_needleleave_evergreen+
                                  temperature:precipitation+temperature:solar_radiation +
                                  temperature:thermal_radiation +temperature:Wspeed +
                                  evaporation:precipitation +  potential_evaporation:precipitation +
                                  lc_Tree_broadleaved_deciduous:month +lc_Tree_needleleave_deciduous:month +
                                  lc_Tree_broadleaved_evergreen:lc_Shrubs +lc_Tree_broadleaved_deciduous:lc_Shrubs+
                                  lc_Tree_needleleave_evergreen:lc_Shrubs +lc_Tree_needleleave_deciduous:lc_Shrubs +
                                  lc_Tree_mixed:lc_Shrubs +lc_Tree_broadleaved_evergreen:lc_Grassland +
                                  lc_Tree_broadleaved_deciduous:lc_Grassland + lc_Tree_needleleave_evergreen:lc_Grassland+
                                  lc_Tree_needleleave_deciduous:lc_Grassland + lc_Tree_mixed:lc_Grassland +
                                  lc_Tree_broadleaved_evergreen:lc_Urban +lc_Tree_broadleaved_deciduous:lc_Urban +
                                  lc_Tree_needleleave_evergreen:lc_Urban +lc_Tree_needleleave_deciduous:lc_Urban +
                                  lc_Tree_mixed:lc_Urban +lc_Shrubs:lc_Urban +lc_Grassland:lc_Urban + lc_Shrubs:solar_radiation +
                                  lc_Shrubs:thermal_radiation +lc_Shrubs:temperature +lc_Grassland:solar_radiation +  
                                  lc_Grassland:thermal_radiation +lc_Grassland:temperature)


#*********************************************************************************************************************************************************************
#GLM:
#*********************************************************************************************************************************************************************


# Poisson:**************************************************************************************************************

#basic fit:

fit_poisson = glm(CNT ~.-1, data = df, family = poisson(link = log))
summary(fit_poisson)
layout(matrix(1:4,ncol=2))
plot( fit_poisson, caption = list("Residuals vs Fitted", "Normal Q-Q",
                                  "Scale-Location", "Cook's distance",
                                  "Residuals vs Leverage",
                                  expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))
newdata <- test[,-1]
log_predictions <- fit_poisson %>% predict(newdata)
predictions<- exp(log_predictions )

zero_rows<-which(test$CNT == 0)
rmsevalue_zero<-rmse(test$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test$CNT != 0)
rmsevalue_non_zero<-rmse(test$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

AICvalue<-AIC(fit_poisson)
AICvalue

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("Poisson", "NO",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
newrow
dim(newrow)
dim(results_models)
results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models


# considering interaction
fit_poisson_interaction = glm(formula_interaction, data = df, family = poisson(link = log))
summary(fit_poisson_interaction)

layout(matrix(1:4,ncol=2))

plot( fit_poisson_interaction, caption = list("Residuals vs Fitted", "Normal Q-Q",
                                              "Scale-Location", "Cook's distance",
                                              "Residuals vs Leverage",
                                              expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))


newdata <- test[,-1]
log_predictions <- fit_poisson_interaction %>% predict(newdata)
predictions<- exp(log_predictions )

zero_rows<-which(test$CNT == 0)
rmsevalue_zero<-rmse(test$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test$CNT != 0)
rmsevalue_non_zero<-rmse(test$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

AICvalue<-AIC(fit_poisson_interaction)
AICvalue

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("Poisson", "YES",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
newrow

dim(newrow)
dim(results_models)
results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models



# 
# toselect.x <- summary(fit_poisson_interaction)$coeff[-1,4] < 0.05 # credit to kith
# # select sig. variables
# relevant.x <- names(toselect.x)[toselect.x == TRUE] 
# 
# # formula with only sig variables
# restricted_formula_poisson_interaction <- as.formula(paste("CNT ~",paste(relevant.x, collapse= "+")))
# restricted_formula_poisson_interaction
# restricted_fit_poisson_interaction <- glm(restricted_formula_poisson_interaction, data = df, family = poisson(link =log))
# 
# summary(restricted_fit_poisson_interaction)
# layout(matrix(1:4,ncol=2))
# plot( restricted_fit_poisson_interaction, caption = list("Residuals vs Fitted", "Normal Q-Q",
#                                                          "Scale-Location", "Cook's distance",
#                                                          "Residuals vs Leverage",
#                                                          expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))
# newdata <- test[,-1]
# log_predictions <- restricted_fit_poisson_interaction %>% predict(newdata)
# predictions<- exp(log_predictions )
# rmsevalue<-rmse(test$CNT,predictions)
# rmsevalue
# 
# zero_rows<-which(test$CNT == 0)
# rmsevalue_zero<-rmse(test$CNT[zero_rows],predictions[zero_rows])
# rmsevalue_zero
# 
# non_zero_rows<-which(test$CNT != 0)
# rmsevalue_non_zero<-rmse(test$CNT[non_zero_rows],predictions[non_zero_rows])
# rmsevalue_non_zero
# 
# AICvalue<-AIC(restricted_fit_poisson_interaction)
# AICvalue
# 
# #c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
# newrow<-data.frame("Poisson", "YES after step",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
# newrow
# 
# dim(newrow)
# dim(results_models)
# results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
# results_models
# forwards <- step(restricted_fit_poisson_interaction,
#                  scope=list(lower=formula(restricted_fit_poisson_interaction),upper=formula(fit_poisson_interaction)), direction="forward")
# 




final_Formula_Poisson<-as.formula(CNT ~ lon + lat + area + year + month + altiMean + altiSD + dew_temperature + 
                                    temperature + potential_evaporation + solar_radiation + thermal_radiation + 
                                    pressure + evaporation + precipitation + humidity + Wspeed + 
                                    lc_Cropland + lc_Vegetation_Bare_areas + lc_Tree_broadleaved_evergreen + 
                                    lc_Tree_broadleaved_deciduous + lc_Tree_needleleave_deciduous + 
                                    lc_Tree_mixed + lc_Grassland + lc_Shrubs + lc_Urban + lc_Water_and_flooded_areas + 
                                    temperature:precipitation + temperature:solar_radiation + 
                                    temperature:thermal_radiation + temperature:Wspeed + evaporation:precipitation + 
                                    potential_evaporation:precipitation + month:lc_Tree_broadleaved_deciduous + 
                                    month:lc_Tree_needleleave_deciduous + lc_Tree_broadleaved_evergreen:lc_Shrubs + 
                                    lc_Tree_broadleaved_deciduous:lc_Shrubs + lc_Shrubs:lc_Tree_needleleave_evergreen + 
                                    lc_Tree_needleleave_deciduous:lc_Shrubs + lc_Tree_mixed:lc_Shrubs + 
                                    lc_Tree_broadleaved_evergreen:lc_Grassland + lc_Tree_broadleaved_deciduous:lc_Grassland + 
                                    lc_Grassland:lc_Tree_needleleave_evergreen + lc_Tree_needleleave_deciduous:lc_Grassland + 
                                    lc_Tree_mixed:lc_Grassland + lc_Tree_broadleaved_evergreen:lc_Urban + 
                                    lc_Tree_broadleaved_deciduous:lc_Urban + lc_Urban:lc_Tree_needleleave_evergreen + 
                                    lc_Tree_needleleave_deciduous:lc_Urban + lc_Shrubs:lc_Urban + 
                                    lc_Grassland:lc_Urban + solar_radiation:lc_Shrubs + thermal_radiation:lc_Shrubs + 
                                    temperature:lc_Shrubs + solar_radiation:lc_Grassland + thermal_radiation:lc_Grassland + 
                                    temperature:lc_Grassland)


final_fit_poisson_interaction <- glm(final_Formula_Poisson, data = df, family = poisson(link =log))
summary(final_fit_poisson_interaction)

layout(matrix(1:4,ncol=2))

plot( final_fit_poisson_interaction, caption = list("Residuals vs Fitted", "Normal Q-Q",
                                              "Scale-Location", "Cook's distance",
                                              "Residuals vs Leverage",
                                              expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))



newdata <- test[,-1]
log_predictions <- final_fit_poisson_interaction %>% predict(newdata)
predictions<- exp(log_predictions )
rmsevalue<-rmse(test$CNT,predictions)
rmsevalue

zero_rows<-which(test$CNT == 0)
rmsevalue_zero<-rmse(test$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test$CNT != 0)
rmsevalue_non_zero<-rmse(test$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

AICvalue<-AIC(final_fit_poisson_interaction)
AICvalue

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("Poisson final", "Yes after step",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
newrow

dim(newrow)
dim(results_models)
results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models

#Quasi Poisson:*******************************************************************************************************

fit_quasipoisson = glm(CNT ~.-1, data = df, family =  quasipoisson)
summary(fit_quasipoisson)
layout(matrix(1:4,ncol=2))
plot( fit_quasipoisson, caption = list("Residuals vs Fitted", "Normal Q-Q",
                                       "Scale-Location", "Cook's distance",
                                       "Residuals vs Leverage",
                                       expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))



newdata <- test[,-1]
log_predictions <- fit_quasipoisson %>% predict(newdata)
predictions<- exp(log_predictions )

zero_rows<-which(test$CNT == 0)
rmsevalue_zero<-rmse(test$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test$CNT != 0)
rmsevalue_non_zero<-rmse(test$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

#AICvalue<-AIC(fit_quasipoisson)
#AICvalue

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("QuasiPoisson", "NO","", rmsevalue_zero,rmsevalue_non_zero)
newrow
dim(newrow)
dim(results_models)
results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models

fit_quasipoisson_interaction = glm(formula_interaction, data = df, family = quasipoisson)
summary(fit_quasipoisson_interaction)

layout(matrix(1:4,ncol=2))
plot( fit_quasipoisson_interaction, caption = list("Residuals vs Fitted", "Normal Q-Q",
                                                   "Scale-Location", "Cook's distance",
                                                   "Residuals vs Leverage",
                                                   expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))

newdata <- test[,-1]
log_predictions <- fit_quasipoisson_interaction %>% predict(newdata)
predictions<- exp(log_predictions )

zero_rows<-which(test$CNT == 0)
rmsevalue_zero<-rmse(test$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test$CNT != 0)
rmsevalue_non_zero<-rmse(test$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

# AICvalue<-AIC(fit_poisson_interaction)
# AICvalue

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("QuasiPoisson", "YES","", rmsevalue_zero,rmsevalue_non_zero)
newrow

dim(newrow)
dim(results_models)
results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models

# ##
# toselect.x <- summary(fit_quasipoisson_interaction)$coeff[-1,4] < 0.05 # credit to kith
# # select sig. variables
# relevant.x <- names(toselect.x)[toselect.x == TRUE] 
# # formula with only sig variables
# restricted_formula_quasipoisson_interaction <- as.formula(paste("CNT ~",paste(relevant.x, collapse= "+")))
# restricted_formula_quasipoisson_interaction
# restricted_fit_quasipoisson_interaction <- glm(restricted_formula_quasipoisson_interaction, data = df, family = quasipoisson)
# summary(restricted_fit_quasipoisson_interaction)
# layout(matrix(1:4,ncol=2))
# plot( restricted_fit_quasipoisson_interaction, caption = list("Residuals vs Fitted", "Normal Q-Q",
#                                                               "Scale-Location", "Cook's distance",
#                                                               "Residuals vs Leverage",
#                                                               expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))
# newdata <- test[,-1]
# log_predictions <- restricted_fit_quasipoisson_interaction %>% predict(newdata)
# predictions<- exp(log_predictions )
# rmsevalue<-rmse(test$CNT,predictions)
# rmsevalue
# 
# zero_rows<-which(test$CNT == 0)
# rmsevalue_zero<-rmse(test$CNT[zero_rows],predictions[zero_rows])
# rmsevalue_zero
# 
# non_zero_rows<-which(test$CNT != 0)
# rmsevalue_non_zero<-rmse(test$CNT[non_zero_rows],predictions[non_zero_rows])
# rmsevalue_non_zero
# 
# # AICvalue<-AIC(restricted_fit_poisson_interaction)
# # AICvalue
# 
# #c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
# newrow<-data.frame("QuasiPoisson", "YES before step","", rmsevalue_zero,rmsevalue_non_zero)
# newrow
# 
# dim(newrow)
# dim(results_models)
# results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
# results_models
# No step possible since the quasipoisson has no likelihood

#Binomial:********************************************************************************************************************************

#Writing the data  in a binary format: both training and test data

df_binom<-df
for (i in 1:nrow(df)) {
  if(df[i,1]==0){
    df_binom[i,1]<-0
  } else{
    df_binom[i,1]<-1
  }
}
df_binom[,1]

test_binom<-test
for (i in 1:nrow(test)) {
  if(test[i,1]==0){
    test_binom[i,1]<-0
  } else{
    test_binom[i,1]<-1
  }
}
test_binom[,1]

fit_binomial = glm(CNT ~1, data = df_binom, family = binomial())
summary(fit_binomial)

layout(matrix(1:4,ncol=2))
plot( fit_binomial, caption = list("Residuals vs Fitted", "Normal Q-Q",
                                   "Scale-Location", "Cook's distance",
                                   "Residuals vs Leverage",
                                   expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))


newdata <- test_binom[,-1]
num<-nrow(newdata)
log_predictions <- fit_binomial %>% predict(newdata)
probabilities<-inv.logit(log_predictions)
predictions<-rbinom( num, 1, probabilities)
zero_rows<-which(test_binom$CNT == 0)
rmsevalue_zero<-rmse(test_binom$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test_binom$CNT != 0)
rmsevalue_non_zero<-rmse(test_binom$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

AICvalue<-AIC(fit_binomial)

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("Binomial", "NO",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
newrow

results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models


fit_binomial_interaction = glm(formula_interaction, data = df_binom, family = binomial())
summary(fit_binomial_interaction)

layout(matrix(1:4,ncol=2))
plot( fit_binomial_interaction, caption = list("Residuals vs Fitted", "Normal Q-Q",
                                               "Scale-Location", "Cook's distance",
                                               "Residuals vs Leverage",
                                               expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))

newdata <- test_binom[,-1]
num<-nrow(newdata)
log_predictions <- fit_binomial_interaction %>% predict(newdata)
probabilities<-inv.logit(log_predictions)
#predictions<- log_predictions 
predictions<-rbinom( num, 1, probabilities)
zero_rows<-which(test_binom$CNT == 0)
rmsevalue_zero<-rmse(test_binom$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test_binom$CNT != 0)
rmsevalue_non_zero<-rmse(test_binom$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

AICvalue<-AIC(fit_binomial_interaction)

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("Binomial", "YES",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
newrow

results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models

# toselect.x <- summary(fit_binomial_interaction)$coeff[-1,4] < 0.05 # credit to kith
# # select sig. variables
# relevant.x <- names(toselect.x)[toselect.x == TRUE] 
# 
# # formula with only sig variables
# restricted_formula_binomial_interaction <- as.formula(paste("CNT ~",paste(relevant.x, collapse= "+")))
# restricted_formula_binomial_interaction
# 
# restricted_fit_binomial_interaction = glm(restricted_formula_binomial_interaction, data = df_binom, family = binomial())
# summary(restricted_fit_binomial_interaction)
# 
# forwards <- step(restricted_fit_binomial_interaction,
#                  scope=list(lower=formula(restricted_fit_binomial_interaction),upper=formula(fit_binomial_interaction)), direction="forward") 
# fit_binomial = glm(CNT ~1, data = df_binom, family = binomial())
# summary(fit_binomial)

final_formula_Binomial<-CNT ~ lon + area + year + month + altiSD + dew_temperature + 
  temperature + potential_evaporation + solar_radiation + thermal_radiation + 
  pressure + evaporation + precipitation + humidity + Wspeed + 
  lc_Cropland + lc_Vegetation_Bare_areas + lc_Tree_broadleaved_evergreen + 
  lc_Tree_broadleaved_deciduous + lc_Tree_needleleave_deciduous + 
  lc_Tree_mixed + lc_Grassland + lc_Shrubs + lc_Urban + lc_Water_and_flooded_areas + 
  lc_Tree_needleleave_evergreen + lat + temperature:precipitation + 
  temperature:solar_radiation + temperature:thermal_radiation + 
  temperature:Wspeed + evaporation:precipitation + potential_evaporation:precipitation + 
  month:lc_Tree_broadleaved_deciduous + month:lc_Tree_needleleave_deciduous + 
  lc_Tree_broadleaved_evergreen:lc_Shrubs + lc_Tree_broadleaved_deciduous:lc_Shrubs + 
  lc_Shrubs:lc_Tree_needleleave_evergreen + lc_Tree_needleleave_deciduous:lc_Shrubs + 
  lc_Tree_mixed:lc_Shrubs + lc_Tree_broadleaved_evergreen:lc_Grassland + 
  lc_Tree_broadleaved_deciduous:lc_Grassland + lc_Grassland:lc_Tree_needleleave_evergreen + 
  lc_Tree_needleleave_deciduous:lc_Grassland + lc_Tree_mixed:lc_Grassland + 
  lc_Tree_broadleaved_evergreen:lc_Urban + lc_Urban:lc_Tree_needleleave_evergreen + 
  lc_Tree_needleleave_deciduous:lc_Urban + lc_Tree_mixed:lc_Urban + 
  lc_Shrubs:lc_Urban + lc_Grassland:lc_Urban + temperature:lc_Shrubs + 
  solar_radiation:lc_Grassland + thermal_radiation:lc_Grassland + 
  temperature:lc_Grassland + solar_radiation:lc_Shrubs



final_fit_binomial_interaction = glm(final_formula_Binomial, data = df_binom, family = binomial())
summary(final_fit_binomial_interaction)
layout(matrix(1:4,ncol=2))
plot( final_fit_binomial_interaction, caption = list("Residuals vs Fitted", "Normal Q-Q",
                                                     "Scale-Location", "Cook's distance",
                                                     "Residuals vs Leverage",
                                                     expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))




newdata <- test_binom[,-1]
log_predictions <- final_fit_binomial_interaction %>% predict(newdata)
probabilities<-inv.logit(log_predictions)
predictions<-rbinom( num, 1, probabilities) 
zero_rows<-which(test_binom$CNT == 0)
rmsevalue_zero<-rmse(test_binom$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test_binom$CNT != 0)
rmsevalue_non_zero<-rmse(test_binom$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

AICvalue<-AIC(final_fit_binomial_interaction)

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("Binomial Final", "YES",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
newrow

results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models

#Negative Binomial:*********************************************************************************************************

fit_nb =glm.nb(CNT ~.-1, data = df)
summary( fit_nb)
layout(matrix(1:4,ncol=2))
plot(  fit_nb, caption = list("Residuals vs Fitted", "Normal Q-Q",
                              "Scale-Location", "Cook's distance",
                              "Residuals vs Leverage",
                              expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))
newdata <- test[,-1]
log_predictions <- fit_nb %>% predict(newdata)
predictions<- exp(log_predictions )

zero_rows<-which(test$CNT == 0)
rmsevalue_zero<-rmse(test$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test$CNT != 0)
rmsevalue_non_zero<-rmse(test$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

AICvalue<-AIC(fit_nb)
AICvalue

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("Neg Bin", "NO",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
newrow
dim(newrow)
dim(results_models)
results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models

fit_nb_interaction = glm.nb(formula_interaction, data = df)
summary(fit_nb_interaction)
layout(matrix(1:4,ncol=2))
plot( fit_nb_interaction, caption = list("Residuals vs Fitted", "Normal Q-Q",
                                         "Scale-Location", "Cook's distance",
                                         "Residuals vs Leverage",
                                         expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))

newdata <- test[,-1]
log_predictions <- fit_nb_interaction %>% predict(newdata)
predictions<- exp(log_predictions )

zero_rows<-which(test$CNT == 0)
rmsevalue_zero<-rmse(test$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test$CNT != 0)
rmsevalue_non_zero<-rmse(test$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

AICvalue<-AIC(fit_nb_interaction)
AICvalue

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("NB", "YES",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
newrow

dim(newrow)
dim(results_models)
results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models

# #
# toselect.x <- summary(fit_poisson_interaction)$coeff[-1,4] < 0.05 # credit to kith
# # select sig. variables
# relevant.x <- names(toselect.x)[toselect.x == TRUE] 
# 
# # formula with only sig variables
# restricted_formula_nb_interaction <- as.formula(paste("CNT ~",paste(relevant.x, collapse= "+")))
# restricted_formula_nb_interaction
# restricted_fit_nb_interaction <- glm(restricted_formula_nb_interaction, data = df, family = poisson(link =log))
# summary(restricted_fit_nb_interaction)
# layout(matrix(1:4,ncol=2))
# plot( restricted_fit_nb_interaction, caption = list("Residuals vs Fitted", "Normal Q-Q",
#                                                     "Scale-Location", "Cook's distance",
#                                                     "Residuals vs Leverage",
#                                                    expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))
# newdata <- test[,-1]
# log_predictions <- restricted_fit_nb_interaction %>% predict(newdata)
# predictions<- exp(log_predictions )
# rmsevalue<-rmse(test$CNT,predictions)
# rmsevalue
# zero_rows<-which(test$CNT == 0)
# rmsevalue_zero<-rmse(test$CNT[zero_rows],predictions[zero_rows])
# rmsevalue_zero
# non_zero_rows<-which(test$CNT != 0)
# rmsevalue_non_zero<-rmse(test$CNT[non_zero_rows],predictions[non_zero_rows])
# rmsevalue_non_zero
# AICvalue<-AIC(restricted_fit_nb_interaction)
# AICvalue
# #c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
# newrow<-data.frame("NB", "YES after step",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
# newrow
# dim(newrow)
# dim(results_models)
# results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
# results_models
# forwards <- step(restricted_fit_nb_interaction,
#                  scope=list(lower=formula(restricted_fit_nb_interaction),upper=formula(fit_nb_interaction)), direction="forward") 


final_Formula_nb<-as.formula(CNT ~ lon + lat + area + year + month + altiMean + altiSD + dew_temperature + 
                               temperature + potential_evaporation + solar_radiation + thermal_radiation + 
                               pressure + evaporation + precipitation + humidity + Wspeed + 
                               lc_Cropland + lc_Vegetation_Bare_areas + lc_Tree_broadleaved_evergreen + 
                               lc_Tree_broadleaved_deciduous + lc_Tree_needleleave_deciduous + 
                               lc_Tree_mixed + lc_Grassland + lc_Shrubs + lc_Urban + lc_Water_and_flooded_areas + 
                               temperature:precipitation + temperature:solar_radiation + 
                               temperature:thermal_radiation + temperature:Wspeed + evaporation:precipitation + 
                               potential_evaporation:precipitation + month:lc_Tree_broadleaved_deciduous + 
                               month:lc_Tree_needleleave_deciduous + lc_Tree_broadleaved_evergreen:lc_Shrubs + 
                               lc_Tree_broadleaved_deciduous:lc_Shrubs + lc_Shrubs:lc_Tree_needleleave_evergreen + 
                               lc_Tree_needleleave_deciduous:lc_Shrubs + lc_Tree_mixed:lc_Shrubs + 
                               lc_Tree_broadleaved_evergreen:lc_Grassland + lc_Tree_broadleaved_deciduous:lc_Grassland + 
                               lc_Grassland:lc_Tree_needleleave_evergreen + lc_Tree_needleleave_deciduous:lc_Grassland + 
                               lc_Tree_mixed:lc_Grassland + lc_Tree_broadleaved_evergreen:lc_Urban + 
                               lc_Tree_broadleaved_deciduous:lc_Urban + lc_Urban:lc_Tree_needleleave_evergreen + 
                               lc_Tree_needleleave_deciduous:lc_Urban + lc_Shrubs:lc_Urban + 
                               lc_Grassland:lc_Urban + solar_radiation:lc_Shrubs + thermal_radiation:lc_Shrubs + 
                               temperature:lc_Shrubs + solar_radiation:lc_Grassland + thermal_radiation:lc_Grassland + 
                               temperature:lc_Grassland)

final_fit_nb_interaction <- glm(final_Formula_nb, data = df, family = poisson(link =log))
summary(final_fit_nb_interaction)

layout(matrix(1:4,ncol=2))
plot( final_fit_nb_interaction, caption = list("Residuals vs Fitted", "Normal Q-Q",
                                               "Scale-Location", "Cook's distance",
                                               "Residuals vs Leverage",
                                               expression("Cook's dist vs Leverage  " * h[ii] / (1 - h[ii]))))

newdata <- test[,-1]
log_predictions <- final_fit_nb_interaction %>% predict(newdata)
predictions<- exp(log_predictions )

zero_rows<-which(test$CNT == 0)
rmsevalue_zero<-rmse(test$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test$CNT != 0)
rmsevalue_non_zero<-rmse(test$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

AICvalue<-AIC(final_fit_nb_interaction)
AICvalue

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("NB final", "Yes after step",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
newrow

dim(newrow)
dim(results_models)
results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models

#ZINB******************************************************************************************************************************

#before starting to fit, we scale all our data sets:

df_scaled<-as.data.frame(cbind(df[,1],scale(df[,-1])))
colnames(df_scaled)<-colnames(df)
test_scaled<-as.data.frame(cbind(test[,1],scale(test[,-1])))
colnames(test_scaled)<-colnames(df)
variables1<-colnames(df[,-1])
formula_all_variables<-as.formula(paste("CNT ~",paste(variables1, collapse= "+")))
formula_all_variables
fit_ZINB_scaled <- zeroinfl(formula_all_variables, data = df_scaled)
summary(fit_ZINB_scaled)


logLik(fit_ZINB_scaled)
rootogram(fit_ZINB_scaled, main = "ZIP")
plot(residuals(fit_ZINB_scaled) ~ fitted(fit_ZINB_scaled), xlab = "Fitted",ylab = "Residual")
plot(df$CNT ~ fitted(fit_ZINB_scaled), xlab = "Fitted",ylab = "Observed")

newdata <- test_scaled[,-1]
log_predictions <- fit_ZINB_scaled %>% predict(newdata)
predictions<- log_predictions 
plot(test_scaled$CNT ~ predictions,xlab = "Test",ylab = "Predicted")


newdata <- test_scaled[,-1]
log_predictions <- fit_ZINB_scaled %>% predict(newdata)
predictions<- log_predictions 
zero_rows<-which(test_scaled$CNT == 0)
rmsevalue_zero<-rmse(test_scaled$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test_scaled$CNT != 0)
rmsevalue_non_zero<-rmse(test_scaled$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

AICvalue<-AIC(fit_ZINB_scaled)
AICvalue

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("ZINB final", "NO",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
newrow
dim(newrow)
dim(results_models)
results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models

##
fit_ZINB_scaled_interaction <- zeroinfl(formula_interaction, data = df_scaled)
summary(fit_ZINB_scaled_interaction)
newdata <- test[,-1]
log_predictions <- fit_ZINB_scaled_interaction %>% predict(newdata)
predictions<- exp(log_predictions )
zero_rows<-which(test$CNT == 0)
rmsevalue_zero<-rmse(test$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test$CNT != 0)
rmsevalue_non_zero<-rmse(test$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

AICvalue<-AIC(fit_ZINB_scaled_interaction)

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("NB final", "Yes before step",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
newrow

results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models

toselect.x1<- summary(fit_ZINB_scaled_interaction)$coeff$count[-1,4] < 0.05 # credit to kith
# select sig. variables
relevant.x1 <- names(toselect.x1)[toselect.x1 == TRUE] 

toselect.x2<- summary(fit_ZINB_scaled_interaction)$coeff$zero[-1,4] < 0.05 # credit to kith
# select sig. variables
relevant.x2 <- names(toselect.x2)[toselect.x2 == TRUE] 

both_relevant<-paste(paste(relevant.x2, collapse= "+"),paste(relevant.x1, collapse= "+"))

# formula with only sig variables
restricted_ZINB_interaction <- as.formula(paste("CNT ~",paste(relevant.x1, collapse= "+")))
restricted_ZINB_interaction
restricted_fit_ZINB_scaled_interaction <- zeroinfl(restricted_ZINB_interaction, data = df)
summary(restricted_fit_ZINB_scaled_interaction)

newdata <- test[,-1]
log_predictions <- restricted_fit_ZINB_scaled_interaction %>% predict(newdata)
predictions<- exp(log_predictions )
zero_rows<-which(test$CNT == 0)
rmsevalue_zero<-rmse(test$CNT[zero_rows],predictions[zero_rows])
rmsevalue_zero

non_zero_rows<-which(test$CNT != 0)
rmsevalue_non_zero<-rmse(test$CNT[non_zero_rows],predictions[non_zero_rows])
rmsevalue_non_zero

AICvalue<-AIC(restricted_fit_ZINB_scaled_interaction)

#c("Model", "Interaction","AIC", "RMSE zero values","RMSE non zero values")
newrow<-data.frame("ZINB", "Yes before step",AICvalue, rmsevalue_zero,rmsevalue_non_zero)
newrow

results_models<-data.frame(rbind(as.matrix(results_models), as.matrix(newrow)))
results_models
write.csv(results_models, paste0("/Users/Mac/Desktop/mrm/summary_fit_model_ZINB.csv"), quote = F, row.names = F)
forwards <- step(restricted_fit_ZINB_scaled_interaction,
                 scope=list(lower=formula(restricted_fit_ZINB_scaled_interaction),upper=formula(fit_ZINB_scaled_interaction)), direction="forward") 





# synthesis:
mt<-xtable(results_models)
code_latex<-print(mt,type = getOption("xtable.type", "latex"),table.placement = getOption("xtable.table.placement", "ht"),
                  caption.placement = getOption("xtable.caption.placement", "bottom"),
                  caption.width = getOption("xtable.caption.width", NULL),
                  latex.environments = getOption("xtable.latex.environments", c("center")),
                  tabular.environment = getOption("xtable.tabular.environment", "tabular"))
