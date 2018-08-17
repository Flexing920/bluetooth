library(plyr)
library(RPostgreSQL)
library(rgdal)
library(rgeos)
library(leaflet)
library(sp)
library(dplyr)
library(DT)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(rpostgis)


# The following script reads in HDR corrridor shapes (as shapes),
# pdf metadata project information,
# matching bluetooth and wavetronix sensor location (as lat lon), 
# and matching bus route shapes in geojson format. 
# Further, processing in R is needed to create spatial
# objects from geojson database information.

############################# ALL DATABASE CONNECTIONS MOVED TO SERVER ##############################

### Adding short names of the corridors
corr_short_list <- c("Airport", "Burnet", "N. Lamar", "E. Riverside", 'E. MLK', 'S. Lamar', 'William\nCannon', 'Slaughter', "Guadalupe") 
corr_id_list <- c(1, 2, 3, 4, 5, 6, 8, 9, 7)
corr_short <- setNames(data.frame(cbind(corr_short_list, corr_id_list)), c("corr_short", "corr_id"))


###################### Wavetronix location and data matchups ################
sensor_name <- c("BURNET RD / PALM WAY (IBM DRIVEWAY)", "BURNET RD / RUTLAND DR",
  "2021 BLK KINNEY AVE (NW 300ft NW of Lamar)", "LAMAR BLVD / MANCHACA RD",
  "1612 BLK S LAMAR BLVD (Collier)", "3201 BLK S LAMAR BLVD (BROKEN SPOKE)")

data_name <- c("BURNETPALM WAY", "BurnetRutland", "KINNEYLAMAR", 
  "LAMARMANCHACA","LAMARCOLLIER", "LamarBroken Spoke")

id <- c("wav-0", "wav-1", "wav-2", "wav-3", "wav-4", "wav-5")

wave_matchup <- as.data.frame(cbind(sensor_name, data_name, id))


################################ BLUETOOTH ################

blue_corrNames <- c("Test corridor", "Burnet", "South Lamar", "North Lamar")

blue_corrNames_alias <- c("Burnet", "Burnet", "Lamar", "Lamar")

sensor_list1 <- c('172.16.29.23', # Burnet @ Braker
                  '172.16.79.20', # Burnet @ Loop1
                  '172.16.179.46', # Lamar @ Brodie Oaks
                  '172.16.77.43') # Lamar @ Payton Gin

sensor_list2 <- c('172.16.28.20', # Burnet @ Anderson
                  '172.16.29.27', # Burnet @ 45th
                  '172.16.179.35', # Lamar @ Riverside
                  '172.16.77.38') # Lamar @ Parmer
                   
 
blue_corr_data <- setNames(as.data.frame(cbind(blue_corrNames, blue_corrNames_alias, sensor_list1, sensor_list2)), 
  c("corridor", "corridor_alias", "ip1", "ip2"))


travel_time_factor <- 5
temp_aggregation <- 3600
mad_factor <- 5
