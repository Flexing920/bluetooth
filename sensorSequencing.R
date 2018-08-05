library("RPostgreSQL")
library(plyr)
library (dplyr)
library (DT)
library (geosphere)

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

if (dbExistsTable(con, "intersections") == TRUE)
  dbExecute(con, "DROP TABLE intersections;")
dbExecute(con, "create table intersections (name text);")
dbExecute(
  con,
  "insert into intersections select distinct * from (select REGEXP_REPLACE(location,' @.*$','','g') from bt_location_edit) a;"
)
dbExecute(
  con,
  "insert into intersections select distinct * from (select REGEXP_REPLACE(location,'^.*@ ','','g') as name from bt_location_edit) a where a.name not in (select name from intersections);"
)
sensorlist <- dbGetQuery(con, "select * from intersections;")

dbDisconnect(con)
sensors <- select(sensors,-c(area, wb_index, id_index, loc_index))


sequence_table <- data.frame("corridor" = NA,
                             "sensor" = NA,
                             "sequence" = NA)

#Get rid of sensors with ip address==0
sensors <- tbl_df(sensors)
sensors <- filter(sensors, db_ip != 0)

for (i in 1:nrow(sensors)) {
  temp_sensors <-
    filter(sensors, grepl(sensorlist[i, ], sensors$location) == TRUE)

  if (nrow(temp_sensors) >= 2) {
    temp_sensors$k <- 1
    temp_sensors <-
      temp_sensors %>% inner_join(temp_sensors, by = 'k') %>% select(-k)
    
    sensor_combo <-
      data.frame("sensor1" = NA,
                 "sensor2" = NA,
                 "distance" = NA)
    for (j in 1:nrow(temp_sensors)) {
      dist <-
        distCosine(
          c(temp_sensors$long.x[j], temp_sensors$lat.x[j]),
          c(temp_sensors$long.y[j], temp_sensors$lat.y[j])
        )
      sensor_combo <-
        rbind(sensor_combo,
              c(temp_sensors$id.x[j], temp_sensors$id.y[j], dist))
    }
    
    sensor_combo <- filter(sensor_combo, sensor_combo$sensor1 != 'NA')
    sensor_combo$distance <- as.numeric(sensor_combo$distance)

    temp_sensors <-
      inner_join(temp_sensors,
                 sensor_combo,
                 by = c('id.x' = 'sensor1', 'id.y' = 'sensor2'))
    rm(sensor_combo)
    
    name <-
      select(
        filter(
          temp_sensors,
          distance == max(temp_sensors$distance),
          temp_sensors$lat.x < temp_sensors$lat.y
        ),
        id.x
      )

    short_list <-
      arrange(filter(temp_sensors, id.x == as.character(name)),
              desc(distance))

    sensor_seq <- c(seq(1:nrow(short_list)))
    tempcol <- data.frame(sensor_seq)
    short_list <- cbind(short_list, sensor_seq)

    for (j in 1:nrow(short_list)) {
      sequence_table <-
        rbind(sequence_table,
              c(sensorlist[i, ], short_list$id.y[j], short_list$sensor_seq[j]))
    }
    rm(short_list)
    rm(name)
    rm(tempcol)
  }
  rm(temp_sensors)
}

sequence_table<-left_join(sequence_table,sensors,by = c("sensor"="id"))
sequence_table<-filter(sequence_table,corridor!='NA')
sequence_table<-sequence_table[c(1,2,5,4,6,7,3)]

con <-
  dbConnect(
    drv,
    dbname = net,
    host = server,
    port = 5432,
    user = uname,
    password = pwd
  )

dbExecute(con, "DROP TABLE intersections;")
dbWriteTable(con,"intersections",value=sequence_table)
dbExecute(con,"ALTER TABLE intersections DROP COLUMN \"row.names\";")



## Generate segment information for each corridor based on the sequence_table

temp_1 <- sequence_table[1:(nrow(sequence_table)-1),]
colnames(temp_1) <- c("corridor_1", "sensor_1", "location_1", "ip_1", "long_1", "lat_1", "sequence_1")
temp_2 <- sequence_table[2:nrow(sequence_table),]
colnames(temp_2) <- c("corridor_2", "sensor_2", "location_2", "ip_2", "long_2", "lat_2", "sequence_2")
segment_table <- cbind(temp_1, temp_2) %>% filter(corridor_1==corridor_2)
rm(temp_1)
rm(temp_2)

segment_table$segment_id <- NA

for (i in 1:nrow(segment_table)){
  if (segment_table$sequence_1[i]==1){
    segment_table$segment_id[i] <- 1
  } else {
    segment_table$segment_id[i] <- segment_table$segment_id[i-1] + 1
  }
  
}

## Calculate the length of each segment by using discosine method
segment_table$length <- NA
# unit in miles
for (i in 1:nrow(segment_table)){
  segment_table$length[i] <- 0.000621371*distCosine(c(segment_table$long_1[i], segment_table$lat_1[i]), 
                                          c(segment_table$long_2[i], segment_table$lat_2[i]))
}

# speed_limit in miles/hour
segment_table$speed_limit <- NA
segment_table$speed_limit[segment_table$corridor_1=="Burnet"] <- 40
segment_table$speed_limit[segment_table$corridor_1=="Lamar"] <- 45

# travel_time in seconds
segment_table$travel_time <- NA
segment_table$travel_time <- segment_table$length*3600/segment_table$speed_limit

if (dbExistsTable(con, "segment_table") == TRUE)
  dbExecute(con, "DROP TABLE segment_table;")
dbWriteTable(con, "segment_table", segment_table, row.names=TRUE, append=FALSE)

if (dbExistsTable(con, "sequence_table") == TRUE)
  dbExecute(con, "DROP TABLE sequence_table;")
dbWriteTable(con, "sequence_table", sequence_table, row.names=TRUE, append=FALSE)
dbDisconnect(con)


