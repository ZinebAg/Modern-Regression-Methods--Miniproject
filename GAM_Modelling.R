# # GAM Modelling

# ### Run GAM_utils.R
setwd("~/GitHub/MRM_mini_project")
source('GAM_utils.R')


# ## Poisson GAM
gam.poiss <- bam(CNT ~ 
              1 +          
              s(lat, lon, bs="sos") +
              s(year, k=5) +
              s(month, k=3) +
              s(altiMean) + 
              s(altiSD) +
            # smooths of climate variables
              s(temperature) + 
              s(dew_temperature) + 
              s(potential_evaporation) +
              s(evaporation) + 
              s(solar_radiation) + 
              s(thermal_radiation) + 
              s(precipitation) +
              s(humidity) + 
              s(Wspeed) + 
         
            # climate interactions:
              ti(temperature, precipitation) +
              ti(temperature, solar_radiation) +
              ti(temperature, thermal_radiation) +
              ti(temperature, Wspeed) +
              ti(evaporation, precipitation) + 
              ti(potential_evaporation, precipitation) + 
              
            # smooths of land cover
              s(lc_Cropland) +
              s(lc_Vegetation_Bare_areas) + 
              s(lc_Tree_broadleaved_evergreen) + 
              s(lc_Tree_broadleaved_deciduous) +
              s(lc_Tree_needleleave_evergreen) +
              s(lc_Tree_needleleave_deciduous) +
              s(lc_Tree_mixed) + 
              s(lc_Grassland) +
              s(lc_Shrubs) + 
              s(lc_Urban) + 
              s(lc_Water_and_flooded_areas) +
            # interactions of deciduous trees with month
              ti(lc_Tree_broadleaved_deciduous, month) +                 
              ti(lc_Tree_needleleave_deciduous, month) +  
           
            # interactions of trees with source of fire - Shrubs and urban areas                              
            # interactions of trees with shrubs - climbing fire
              ti(lc_Tree_broadleaved_evergreen, lc_Shrubs) +
              ti(lc_Tree_broadleaved_deciduous, lc_Shrubs) +
              ti(lc_Tree_needleleave_evergreen, lc_Shrubs) +
              ti(lc_Tree_needleleave_deciduous, lc_Shrubs) +
              ti(lc_Tree_mixed, lc_Shrubs) +
            # interactions of trees with grassland - climbing fire
              ti(lc_Tree_broadleaved_evergreen, lc_Grassland) +
              ti(lc_Tree_broadleaved_deciduous, lc_Grassland) +
              ti(lc_Tree_needleleave_evergreen, lc_Grassland) +
              ti(lc_Tree_needleleave_deciduous, lc_Grassland) +
              ti(lc_Tree_mixed, lc_Grassland) +
            # interactions of trees with urban areas
              ti(lc_Tree_broadleaved_evergreen, lc_Urban) +
              ti(lc_Tree_broadleaved_deciduous, lc_Urban) +
              ti(lc_Tree_needleleave_evergreen, lc_Urban) +
              ti(lc_Tree_needleleave_deciduous, lc_Urban) +
              ti(lc_Tree_mixed, lc_Urban) +
            # interactions of shrubs and grass with urban areas
              ti(lc_Shrubs, lc_Urban) +
              ti(lc_Grassland, lc_Urban) + 
            # interactions of shrubs and grass with radiations
              ti(lc_Shrubs, solar_radiation) + 
              ti(lc_Shrubs, thermal_radiation) +
              ti(lc_Shrubs, temperature) +
              ti(lc_Grassland, solar_radiation) + 
              ti(lc_Grassland, thermal_radiation) +
              ti(lc_Grassland, temperature),
            data= df_train, 
            family= poisson(link=log),
            method= "fREML", 
            discrete= TRUE,
            nthreads= 8)
AIC(gam.poiss)
summary(gam.poiss)


# ## Negative Binomial GAM
gam.NB <- bam(CNT ~ 
              1 +          
              s(lat, lon, bs="sos") +
              s(year, k=5) +
              s(month, k=3) +
              s(altiMean) + 
              s(altiSD) +
            # smooths of climate variables
              s(temperature) + 
              s(dew_temperature) + 
              s(potential_evaporation) +
              s(evaporation) + 
              s(solar_radiation) + 
              s(thermal_radiation) + 
              s(precipitation) +
              s(humidity) + 
              s(Wspeed) + 
         
            # climate interactions:
              ti(temperature, precipitation) +
              ti(temperature, solar_radiation) +
              ti(temperature, thermal_radiation) +
              ti(temperature, Wspeed) +
              ti(evaporation, precipitation) + 
              ti(potential_evaporation, precipitation) + 
              
            # smooths of land cover
              s(lc_Cropland) +
              s(lc_Vegetation_Bare_areas) + 
              s(lc_Tree_broadleaved_evergreen) + 
              s(lc_Tree_broadleaved_deciduous) +
              s(lc_Tree_needleleave_evergreen) +
              s(lc_Tree_needleleave_deciduous) +
              s(lc_Tree_mixed) + 
              s(lc_Grassland) +
              s(lc_Shrubs) + 
              s(lc_Urban) + 
              s(lc_Water_and_flooded_areas) +
            # interactions of deciduous trees with month
              ti(lc_Tree_broadleaved_deciduous, month) +                 
              ti(lc_Tree_needleleave_deciduous, month) +  
           
            # interactions of trees with source of fire - Shrubs and urban areas                              
            # interactions of trees with shrubs - climbing fire
              ti(lc_Tree_broadleaved_evergreen, lc_Shrubs) +
              ti(lc_Tree_broadleaved_deciduous, lc_Shrubs) +
              ti(lc_Tree_needleleave_evergreen, lc_Shrubs) +
              ti(lc_Tree_needleleave_deciduous, lc_Shrubs) +
              ti(lc_Tree_mixed, lc_Shrubs) +
            # interactions of trees with grassland - climbing fire
              ti(lc_Tree_broadleaved_evergreen, lc_Grassland) +
              ti(lc_Tree_broadleaved_deciduous, lc_Grassland) +
              ti(lc_Tree_needleleave_evergreen, lc_Grassland) +
              ti(lc_Tree_needleleave_deciduous, lc_Grassland) +
              ti(lc_Tree_mixed, lc_Grassland) +
            # interactions of trees with urban areas
              ti(lc_Tree_broadleaved_evergreen, lc_Urban) +
              ti(lc_Tree_broadleaved_deciduous, lc_Urban) +
              ti(lc_Tree_needleleave_evergreen, lc_Urban) +
              ti(lc_Tree_needleleave_deciduous, lc_Urban) +
              ti(lc_Tree_mixed, lc_Urban) +
            # interactions of shrubs and grass with urban areas
              ti(lc_Shrubs, lc_Urban) +
              ti(lc_Grassland, lc_Urban) + 
            # interactions of shrubs and grass with radiations
              ti(lc_Shrubs, solar_radiation) + 
              ti(lc_Shrubs, thermal_radiation) +
              ti(lc_Shrubs, temperature) +
              ti(lc_Grassland, solar_radiation) + 
              ti(lc_Grassland, thermal_radiation) +
              ti(lc_Grassland, temperature),
            data= df_train, 
            family= nb(link=log),
            method= "fREML", 
            discrete= TRUE,
            nthreads= 8)
AIC(gam.NB)
summary(gam.NB)


# ## Quasipoisson GAM
gam.qp <- bam(CNT ~ 
              1 +          
              s(lat, lon, bs="sos") +
              s(year, k=5) +
              s(month, k=3) +
              s(altiMean) + 
              s(altiSD) +
            # smooths of climate variables
              s(temperature) + 
              s(dew_temperature) + 
              s(potential_evaporation) +
              s(evaporation) + 
              s(solar_radiation) + 
              s(thermal_radiation) + 
              s(precipitation) +
              s(humidity) + 
              s(Wspeed) + 
         
            # climate interactions:
              ti(temperature, precipitation) +
              ti(temperature, solar_radiation) +
              ti(temperature, thermal_radiation) +
              ti(temperature, Wspeed) +
              ti(evaporation, precipitation) + 
              ti(potential_evaporation, precipitation) + 
              
            # smooths of land cover
              s(lc_Cropland) +
              s(lc_Vegetation_Bare_areas) + 
              s(lc_Tree_broadleaved_evergreen) + 
              s(lc_Tree_broadleaved_deciduous) +
              s(lc_Tree_needleleave_evergreen) +
              s(lc_Tree_needleleave_deciduous) +
              s(lc_Tree_mixed) + 
              s(lc_Grassland) +
              s(lc_Shrubs) + 
              s(lc_Urban) + 
              s(lc_Water_and_flooded_areas) +
            # interactions of deciduous trees with month
              ti(lc_Tree_broadleaved_deciduous, month) +                 
              ti(lc_Tree_needleleave_deciduous, month) +  
           
            # interactions of trees with source of fire - Shrubs and urban areas                              
            # interactions of trees with shrubs - climbing fire
              ti(lc_Tree_broadleaved_evergreen, lc_Shrubs) +
              ti(lc_Tree_broadleaved_deciduous, lc_Shrubs) +
              ti(lc_Tree_needleleave_evergreen, lc_Shrubs) +
              ti(lc_Tree_needleleave_deciduous, lc_Shrubs) +
              ti(lc_Tree_mixed, lc_Shrubs) +
            # interactions of trees with grassland - climbing fire
              ti(lc_Tree_broadleaved_evergreen, lc_Grassland) +
              ti(lc_Tree_broadleaved_deciduous, lc_Grassland) +
              ti(lc_Tree_needleleave_evergreen, lc_Grassland) +
              ti(lc_Tree_needleleave_deciduous, lc_Grassland) +
              ti(lc_Tree_mixed, lc_Grassland) +
            # interactions of trees with urban areas
              ti(lc_Tree_broadleaved_evergreen, lc_Urban) +
              ti(lc_Tree_broadleaved_deciduous, lc_Urban) +
              ti(lc_Tree_needleleave_evergreen, lc_Urban) +
              ti(lc_Tree_needleleave_deciduous, lc_Urban) +
              ti(lc_Tree_mixed, lc_Urban) +
            # interactions of shrubs and grass with urban areas
              ti(lc_Shrubs, lc_Urban) +
              ti(lc_Grassland, lc_Urban) + 
            # interactions of shrubs and grass with radiations
              ti(lc_Shrubs, solar_radiation) + 
              ti(lc_Shrubs, thermal_radiation) +
              ti(lc_Shrubs, temperature) +
              ti(lc_Grassland, solar_radiation) + 
              ti(lc_Grassland, thermal_radiation) +
              ti(lc_Grassland, temperature),
            data= df_train, 
            family= quasipoisson(link=log),
            method= "fREML", 
            discrete= TRUE,
            nthreads= 8)
AIC(gam.qp)
summary(gam.qp)


# Save fitted models
save(gam.poiss, gam.NB, gam.qp, file = "Martin/models_full_dataset.RData")


# ## Quasipoisson GAM - using full dataset
gam.qp.full <- bam(CNT ~ 
              1 +          
              s(lat, lon, bs="sos") +
              s(year, k=5) +
              s(month, k=3) +
              s(altiMean) + 
              s(altiSD) +
            # smooths of climate variables
              s(temperature) + 
              s(dew_temperature) + 
              s(potential_evaporation) +
              s(evaporation) + 
              s(solar_radiation) + 
              s(thermal_radiation) + 
              s(precipitation) +
              s(humidity) + 
              s(Wspeed) + 
         
            # climate interactions:
              ti(temperature, precipitation) +
              ti(temperature, solar_radiation) +
              ti(temperature, thermal_radiation) +
              ti(temperature, Wspeed) +
              ti(evaporation, precipitation) + 
              ti(potential_evaporation, precipitation) + 
              
            # smooths of land cover
              s(lc_Cropland) +
              s(lc_Vegetation_Bare_areas) + 
              s(lc_Tree_broadleaved_evergreen) + 
              s(lc_Tree_broadleaved_deciduous) +
              s(lc_Tree_needleleave_evergreen) +
              s(lc_Tree_needleleave_deciduous) +
              s(lc_Tree_mixed) + 
              s(lc_Grassland) +
              s(lc_Shrubs) + 
              s(lc_Urban) + 
              s(lc_Water_and_flooded_areas) +
            # interactions of deciduous trees with month
              ti(lc_Tree_broadleaved_deciduous, month) +                 
              ti(lc_Tree_needleleave_deciduous, month) +  
           
            # interactions of trees with source of fire - Shrubs and urban areas                              
            # interactions of trees with shrubs - climbing fire
              ti(lc_Tree_broadleaved_evergreen, lc_Shrubs) +
              ti(lc_Tree_broadleaved_deciduous, lc_Shrubs) +
              ti(lc_Tree_needleleave_evergreen, lc_Shrubs) +
              ti(lc_Tree_needleleave_deciduous, lc_Shrubs) +
              ti(lc_Tree_mixed, lc_Shrubs) +
            # interactions of trees with grassland - climbing fire
              ti(lc_Tree_broadleaved_evergreen, lc_Grassland) +
              ti(lc_Tree_broadleaved_deciduous, lc_Grassland) +
              ti(lc_Tree_needleleave_evergreen, lc_Grassland) +
              ti(lc_Tree_needleleave_deciduous, lc_Grassland) +
              ti(lc_Tree_mixed, lc_Grassland) +
            # interactions of trees with urban areas
              ti(lc_Tree_broadleaved_evergreen, lc_Urban) +
              ti(lc_Tree_broadleaved_deciduous, lc_Urban) +
              ti(lc_Tree_needleleave_evergreen, lc_Urban) +
              ti(lc_Tree_needleleave_deciduous, lc_Urban) +
              ti(lc_Tree_mixed, lc_Urban) +
            # interactions of shrubs and grass with urban areas
              ti(lc_Shrubs, lc_Urban) +
              ti(lc_Grassland, lc_Urban) + 
            # interactions of shrubs and grass with radiations
              ti(lc_Shrubs, solar_radiation) + 
              ti(lc_Shrubs, thermal_radiation) +
              ti(lc_Shrubs, temperature) +
              ti(lc_Grassland, solar_radiation) + 
              ti(lc_Grassland, thermal_radiation) +
              ti(lc_Grassland, temperature),
            data= rbind(df_train, df_val), 
            family= quasipoisson(link=log),
            method= "fREML", 
            discrete= TRUE,
            nthreads= 8,
            coef= gam.qp$coefficients)
AIC(gam.qp.full)
summary(gam.qp.full)


# Save the full Quasipoisson GAM
save(gam.qp.full, file = "Martin/gam_qp_tr_and_val.RData")


