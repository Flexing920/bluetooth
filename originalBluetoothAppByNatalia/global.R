library("RPostgreSQL")
library(plyr)
library (dplyr)
library(ggplot2)
library(leaflet)
library (DT)
library(shiny)


#Arguments that should be external
server = "nmc-compute2.ctr.utexas.edu"
uname = 'bluetooth'
pwd = 'bt2015'
net = "austin_bt"


##Extract sensor location and date availability from database
drv <- dbDriver("PostgreSQL")
con <-
  dbConnect(
    drv,
    dbname = net,
    host = server,
    port = 5432,
    user = uname,
    password = pwd
  )
sensors <- dbGetQuery(con, "select * from bt_location")
#Rename particular sensors for consistency with the rest of the dataset
sensors[sensors$db_ip == '172.16.179.46',]$id = "lamar_brodie_oak"
startdate <-
  dbGetQuery(con, "select \"date\"(min(first_time)) from prototype;")
enddate <-
  dbGetQuery(con, "select \"date\"(max(first_time)) from prototype;")

#Get sensor list for corridor tab panel
#sensor_sequence <- dbGetQuery(con, "select * from intersections")
#Rename particular sensors for consistency with the rest of the dataset

sensor_sequence <- dbGetQuery(con, "select corridor_name as corridor,location as sensor,location as location,sensor_ip as db_ip,x as long,y as lat,sequence as seq from bt_corridors;")

#sensor_sequence[sensor_sequence$db_ip == '172.16.179.46',]$id = "lamar_brodie_oak"
# corridor_names<- as.vector(distinct(sensor_sequence, corridor))
corridor_names<-unique(sensor_sequence$corridor)

dbDisconnect(con)


#Get rid of sensors with ip address==0
sensors <- tbl_df(sensors)
sensors <- filter(sensors, db_ip != 0)


#Format dates
startdate = startdate[1, 1]
enddate = enddate[1, 1]


#Functions
clean_data <- dget("clean_data_func.R")
get_data <- dget("get_data_func.R")


#Download properly sized icon (leaflet's built-in icon is not customizable)
markerIcon <-
  makeIcon(iconUrl = 'http://icons.iconarchive.com/icons/icons-land/vista-map-markers/32/Map-Marker-Marker-Outside-Azure-icon.png',
           iconAnchorX = 16,
           iconAnchorY = 30)