setwd("~/GitHub/MRM_mini_project")
load("data_train_DF.RData")
library(dplyr)

group.lc <- function(df, rescale.lc=TRUE){   
  # function that groups the land cover variables of the dataframe and rescale them by the area of the cell if rescale.lc==TRUE
  # variable pressure is omitted

  df <- df %>% 
    dplyr::mutate(lc_Cropland=lc1+lc2+lc3) %>%
    dplyr::mutate(lc_Vegetation_Bare_areas=lc4+lc13+lc17) %>%
    dplyr::mutate(lc_Tree_broadleaved_evergreen=lc5) %>%
    dplyr::mutate(lc_Tree_broadleaved_deciduous=lc6) %>%
    dplyr::mutate(lc_Tree_needleleave_evergreen=lc7) %>%
    dplyr::mutate(lc_Tree_needleleave_deciduous=lc8) %>%
    dplyr::mutate(lc_Tree_mixed=lc9) %>%
    dplyr::mutate(lc_Grassland=lc12) %>%
    dplyr::mutate(lc_Shrubs=lc10+lc11) %>%
    dplyr::mutate(lc_Urban=lc16) %>%
    dplyr::mutate(lc_Water_and_flooded_areas=lc14+lc15+lc18) %>%
    dplyr::select(-c(lc1, lc2, lc3, lc4, lc5, lc6, lc7, lc8, lc9, lc10, lc11, lc12, lc13, lc14, lc15, lc16, lc17, lc18))
    
  # scale land cover with area to get the absolute value
  if (rescale.lc){
    scale_area <- function(x, na.rm = TRUE) (x * df$area)
    df <- df %>% 
      dplyr::mutate_at(vars(starts_with("lc")), scale_area) %>%
      dplyr::select(-area)
  }

  # omit pressure variable
  df <- df %>% dplyr::select(-pressure)

  # return the modified dataframe
  return(df)
}

splitted.rmse <- function(actual, predicted){
  # function to evaluate RMSE on true zero counts and true positive counts
  rmse_zeros <- rmse(actual[actual==0], predicted[actual==0])
  rmse_pos <- rmse(actual[actual>0], predicted[actual>0])
  return(data.frame(rmse_zeros, rmse_pos))
}

splitted.mae <- function(actual, predicted){
  # function to evaluate MAE on true zero counts and true positive counts
  mae_zeros <- mae(actual[actual==0], predicted[actual==0])
  mae_pos <- mae(actual[actual>0], predicted[actual>0])
  return(data.frame(mae_zeros, mae_pos))
}

data_train_DF <- data_train_DF %>% 
  dplyr::rename(NSwind=clim1, 
                WEwind=clim2, 
                dew_temperature=clim3, 
                temperature=clim4, 
                potential_evaporation=clim5, 
                solar_radiation=clim6,
                thermal_radiation=clim7,
                pressure=clim8,
                evaporation=clim9, 
                precipitation=clim10) %>%
  dplyr::select(-BA) %>%
  dplyr::mutate(humidity=100*exp(17.625*(dew_temperature-273.15)/(dew_temperature-39.11))/exp(17.625*(temperature-273.15)/(temperature-39.11))) %>% # relative humidity
  dplyr::mutate(Wspeed=(sqrt(NSwind^2+WEwind^2))) %>%
  dplyr::select(-NSwind, -WEwind)

# subsample dataset
sub_sample <- c(1995, 1997, 2013, 2005, 2015)
df <- data_train_DF %>% filter(year %in% sub_sample)