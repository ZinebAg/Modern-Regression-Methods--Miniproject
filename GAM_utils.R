setwd("~/GitHub/MRM_mini_project")
source('data_import.R')

# load libraries
library(ggplot2)
library(purrr)
library(tidyr)
library(cowplot)
library(dplyr)
library(mgcv)
library(MASS)
library(Metrics)
library(tidymv)
library(ggplot2)

type <- "deviance"  ## "pearson" & "response" are other valid choices
diagnostics.gam <- function(fit, df_val, label, type="deviance"){
    # function that do the diagnostics of fitted models and print out summaries, AIC, RMSE, MAE
    # inputs: 
    #   fit - fitted model,
    #   df_val - validation dataframe,
    #   label - label for saved figure,
    #   type - type of residuals

    print('AIC:')
    print(AIC(fit))
    print(summary(fit))

    linpred <- fit$linear.predictors
    observed.y <- fit$y    
    
    fitted.y <- fitted(fit)
    if (!is.na(df_val)){
        observed.y.val <- df_val$CNT
        fitted.y.val <- predict.bam(fit, df_val, type="response", discrete=TRUE, n.threads=8)
    }
    resid <- residuals(fit, type= type)
    
    # diagnostic plots
    par(mfrow=c(2, 2), family="A")
    qq.gam(fit, rep = 0, level = 0.9, type = type, rl.col = "black", col = cornflowerblue_transparent, pch=19, main = "Normal Q-Q Plot")
    hist(resid, xlab = "Residuals", main = "Histogram of residuals", col = "cornflowerblue")
    plot(linpred, resid, main = "Resids vs. linear pred.", xlab = "linear predictor", ylab = "residuals", col = cornflowerblue_transparent, pch=19)
    plot(fitted.y, observed.y, xlab = "Fitted Values", ylab = "Response", main = "Response vs. Fitted Values", col = cornflowerblue_transparent, pch=19)
    # save the figure
    # dev.print(png, paste('Figures/',label, 'diagnostics.png', sep=''), units="in", width=5, height=5, res=300)
    
    # if argument of df_val is not empty
    if (!is.na(df_val)){
        par(mfrow=c(1, 1), family="A")
        plot(fitted.y.val, observed.y.val, xlab = "Fitted Values", 
           ylab = "Response", main = "Validation - Response vs. Fitted Values", col = softorange_transparent, pch=19)
        #dev.print(png, paste('Figures/',label, 'val_resp_fitted.png', sep=''), units="in", width=4, height=4, res=300)

        print('RMSE (train):')
        print(splitted.rmse(fit$y, fitted.y))

        # validation rmse
        print('RMSE (val):')
        print(splitted.rmse(observed.y.val, fitted.y.val))
        
        print('MAE (train):')
        print(splitted.mae(fit$y, fitted.y))

        # validation rmse
        print('MAE (val):')
        print(splitted.mae(observed.y.val, fitted.y.val))
    }    
}

# set font family and set colours
windowsFonts(A=windowsFont("Segoe UI Light"))
windowsFonts(B=windowsFont("Segoe UI"))
cornflowerblue_transparent <- rgb(0.392,0.584,0.929, 0.25)
softorange_transparent <- rgb(0.929,0.737,0.392, 0.25)

# group the data
data_train_DF <- group.lc(data_train_DF)

# Use the grouped data for land cover (based on types of land to not deteriorate the information available in the land cover):
df <- group.lc(df) # this is subsampled for trying

# set seed
set.seed(42)

# train/val split
p <- 0.2
n.total <- nrow(data_train_DF %>% filter(!is.na(CNT)))
val.ind <- sample(n.total, floor(p * n.total))

df_train <- na.omit((data_train_DF %>% filter(!is.na(CNT)))[-val.ind,])
df_val <- na.omit((data_train_DF %>% filter(!is.na(CNT)))[val.ind,])
df_predict <- data_train_DF %>% filter(is.na(CNT))
