library(dplyr)
library(ggplot2)
library(sf)
val_zd <- st_read("val_zd.shp")

# EDA 
mean(val_zd$NB_VALD)
