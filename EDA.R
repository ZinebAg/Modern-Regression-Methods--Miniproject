# # Wildfires Modelling in the US - Exploratory Data Analysis

# ### Import data and load libraries
setwd("~/GitHub/MRM_mini_project")
source('data_import.R')


library(ggplot2)
library(purrr)
library(tidyr)
library(plotly)
library(png)
library(gifski)
library(gganimate)
library(cowplot)
library(ggcorrplot)
library(reshape2)


names = c('Cropland',
          'Vegetation/bare land',
          'Tree broadleaved evergreen',
          'Tree broadleaved deciduous',
          'Tree needleleave evergreen',
          'Tree needleleave deciduous',
          'Tree mixed',
          'Grass',
          'Shrubs and shrubs mixed with trees',
          'Urban',
          'Water and flooded areas')

df_grouped <- group.lc(df, TRUE)

data_train_DF_grouped <- group.lc(data_train_DF, TRUE)



head(data_train_DF)
head(data_train_DF_grouped)



summary(data_train_DF)


# Boxplots of variables


p <- ggplot(data = melt(data_train_DF_grouped), aes(x=variable, y=value)) + geom_boxplot(aes(fill=variable)) + theme_minimal() + 
  theme(legend.position = "none") + ggtitle("Box plots") + 
  facet_wrap( ~ variable, scales="free")
options(repr.plot.width=15, repr.plot.height=15)
p


# ## Spatial plots


p <- (data_train_DF %>% 
    select(c(CNT, month, year, lat, lon)) %>%
    group_by(.dots=c("lat", "lon", "year")) %>% 
    mutate(CNTy=sum(CNT, na.rm=1))) %>%
    group_by(.dots=c("lat", "lon")) %>% 
    mutate(CNTyMean=mean(CNTy)) %>% 
    arrange(desc(CNTyMean)) %>% 
    distinct(lat, lon, CNTyMean) %>% 
  ggplot(aes(x=lon, y=lat)) +
  geom_point(shape=15, size=2.1, colour="gray80") + 
  geom_point(aes(size = CNTyMean), alpha=0.15, color="red", stroke = 0, shape = 16) +
  scale_size(breaks = c(0, 10, 50, 100), 
             range = c(0, 15), 
             name="num. of wildfires", 
             labels = expression(0, 10, 50, 100)) + 
  theme_minimal() + 
  ggtitle('Wildfire occurences in USA (annual means from 1993 to 2015)')
options(repr.plot.width=7, repr.plot.height=4)
p



df %>% filter(year==2015, month==3) %>% ggplot(aes(x=lon, y=lat)) +
  geom_point(aes(colour = altiMean), shape=15, size=2.1) +
  theme_minimal() + 
  ggtitle('Altitude Mean') +
  scale_colour_gradient(low = "greenyellow", high = "firebrick4")



(data_train_DF %>% 
    select(c(CNT, month, year, lat, lon, altiMean)) %>%
    group_by(.dots=c("lat", "lon", "year")) %>% 
    mutate(CNTy=sum(CNT, na.rm=1))) %>%
    group_by(.dots=c("lat", "lon")) %>% mutate(CNTyMean=mean(CNTy)) %>% 
    arrange(desc(CNTyMean)) %>% distinct(lat, lon, CNTyMean, altiMean) %>%
  ggplot(aes(x=lon, y=lat)) +
  geom_point(aes(colour = altiMean), shape=15, size=2.1) +
  theme_minimal() + 
  ggtitle('Altitude Mean') +
  scale_colour_gradient(low = "greenyellow", high = "firebrick4") +
  geom_point(aes(size = CNTyMean), alpha=0.15, color="red", stroke = 0, shape = 16) +
  scale_size(breaks = c(0, 10, 50, 100), 
             range = c(0, 15), 
             name="num. of wildfires", 
             labels = expression(0, 10, 50, 100))



df %>% filter(year==2015) %>% ggplot(aes(x=lon, y=lat)) +
  geom_point(aes(colour = altiSD), shape=15, size=2.1) +
  theme_minimal() + 
  ggtitle('Altitude SD') +
  scale_colour_gradient(low = "greenyellow", high = "firebrick4")


# Visualize the major land cover (take the argmax by the location)


p <- df_grouped %>% 
  mutate(`Major land cover`=factor(max.col(df_grouped %>% select(starts_with("lc"))), labels = names[1:11 %in% max.col(df_grouped %>% select(starts_with("lc")))])) %>% 
  filter(year==2015) %>% ggplot(aes(x=lon, y=lat, colour = `Major land cover`)) +
  geom_point(shape=15, size=2.4) +
  theme_minimal() + 
  ggtitle('Major land cover in cells') + 
  scale_color_manual(values = c("wheat", "khaki4", "darkgreen", "seagreen4", "seagreen2", "yellow", "chartreuse3", "sandybrown", "black", "steelblue1"))
options(repr.plot.width=10, repr.plot.height=5)
p



p <- (data_train_DF_grouped %>% 
  mutate(`Major land cover`=factor(max.col(data_train_DF_grouped %>% select(starts_with("lc"))), 
                                   labels = names[1:11 %in% max.col(data_train_DF_grouped %>% select(starts_with("lc")))])) %>%
  select(c(CNT, month, year, lat, lon, `Major land cover`)) %>%
  group_by(.dots=c("lat", "lon", "year")) %>% 
  mutate(CNTy=sum(CNT, na.rm=1))) %>%
  group_by(.dots=c("lat", "lon")) %>% mutate(CNTyMean=mean(CNTy)) %>% 
  arrange(desc(CNTyMean)) %>% distinct(lat, lon, CNTyMean, `Major land cover`) %>%
  
  ggplot(aes(x=lon, y=lat, colour = `Major land cover`)) +
  geom_point(shape=15, size=2.4) +
  scale_color_manual(values = c("wheat", "khaki4", "darkgreen", "seagreen4", "seagreen2", "yellow", "chartreuse3", "sandybrown", "black", "steelblue1")) + 
  
  geom_point(aes(size = CNTyMean), alpha=0.25, color="red", stroke = 0, shape = 16) +
  scale_size(breaks = c(0, 10, 50, 100), 
             range = c(0, 15), 
             name="num. of wildfires", 
             labels = expression(0, 10, 50, 100)) + 
  theme_minimal() + 
  ggtitle('Yearly mean wildfire occurences in USA and major land cover')
p



names_all <- c('cropland rainfed',
  'cropland rainfed herbaceous cover',
  'mosaic cropland',
  'mosaic natural vegetation',
  'tree broadleaved evergreen closed to open',
  'tree broadleaved deciduous closed to open',
  'tree needleleave evergreen closed to open',
  'tree needleleaved deciduous closed to open',
  'tree mixed',
  'mosaic tree and shrub',
  'shrubland',
  'grassland',
  'sparse vegetation',
  'tree cover flooded fresh or brakish water',
  'shrub or herbaceous cover flooded',
  'urban',
  'bare areas',
  'water')

df %>% 
  mutate(`Major land cover`=factor(max.col(df %>% select(starts_with("lc"))), labels = names_all[1:18 %in% max.col(df %>% select(starts_with("lc")))])) %>% 
  filter(year==2015) %>% 
  ggplot(aes(x=lon, y=lat, colour = `Major land cover`)) +
    geom_point(shape = 15, size=2.4) +
    theme_minimal() + 
    ggtitle('Major land cover in cells (original land cover definitions)')


# Correlation plot (original data)


corr <- corr <- round(cor(data_train_DF, method = "spearman"), 2)
ggcorrplot(corr, method="square")



corr <- corr <- round(cor(data_train_DF_grouped[,-1], method = "spearman"), 2)
ggcorrplot(corr, method="square")


# Histograms


p <- data_train_DF_grouped %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram(fill="firebrick4") + 
    theme_minimal()
options(repr.plot.width=15, repr.plot.height=15)
p


# Zero observations frequency


d1 <- data.frame(sum(na.omit(data_train_DF)$CNT == 0)/dim(na.omit(data_train_DF))[1], "CNT=0")
colnames(d1) <- c("frequency", "type")

d2 <- data.frame(sum(na.omit(data_train_DF)$CNT > 0)/dim(na.omit(data_train_DF))[1], "CNT>0")
colnames(d2) <- c("frequency", "type")

rbind(d1, d2)