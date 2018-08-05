####Import packages
library("RPostgreSQL")
library(dplyr)
library(ggplot2)

####Define functions

## sex_x_levels will be used in the get_data function to set up tod (time of day) levels
## 3600 for 1 hour, 1800 for half hour, and 900 for 15 minutes

set_x_levels <- function(temp_agg) {
  
  levels=strftime(seq(ISOdate(2016,1,1,hour=0,0,0,tz=""),
                      ISOdate(2016,1,1,hour=(24-temp_agg/3600)%/%1,
                              60*(24-temp_agg/3600)%%1,
                              tz=""),
                      by=sprintf("%s min",temp_agg/60)),
                  format="%H:%M")
  
  return(levels)
}


## calls database with set parameters
call_db <- function(query) {
  
  
  server = "nmc-compute2.ctr.utexas.edu"
  uname = 'bluetooth'
  pwd = 'bt2015'
  net = "austin_bt"
  
  
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
  
  data <- dbGetQuery(con, query)
  dbDisconnect(con)
  return(data)
  
}



##function: get the travel time threshold from database
get_tt_threshold_from_db <- function(sensor_ip_1, sensor_ip_2, tt_factor){
  
  df <- call_db("select * from segment_table;")
  ## Get the subset of records from the given sensors
  df <- df %>% filter((ip_1==sensor_ip_1 & ip_2==sensor_ip_2) |
                          (ip_1==sensor_ip_2 & ip_2==sensor_ip_1))
  ## Get the tt_average
  tt_average <- df$travel_time
  if(is.na(tt_average)){
    print("There is no average travel time information about this segment.")
    
  } else {
    return(as.integer(tt_average*tt_factor))
  }
}

# # test
# x <- get_tt_threshold_from_db("172.16.28.27", "172.16.28.20", 5)

get_sensors_given_corridor <- function(corr_name){
  
  sensors <- call_db("select * from intersections") %>%
    filter(corridor==corr_name) %>%
    mutate(seq = as.integer(sequence))
  
  return(sensors)
}

# cor <- "Burnet"
# test_get_sensors_given_corridor <- get_sensors_given_corridor(cor)

get_all_sensors_of_a_corridor <- function(corr_name, ip_1, ip_2){
  
  sensors <- get_sensors_given_corridor(corr_name)
  
  seq_sensor_1 <- sensors %>% filter(db_ip==ip_1) %>% select(seq)
  seq_sensor_2 <- sensors %>% filter(db_ip==ip_2) %>% select(seq)
  seq_1 <- min(seq_sensor_1, seq_sensor_2)
  seq_2 <- max(seq_sensor_1, seq_sensor_2)
  
  sensors_between <- filter(sensors, seq>=seq_1 & seq<=seq_2)
  
  # sensors_ip <- sensors_between$db_ip
  return(sensors_between)
}



call_traffic_data <- function(corr_name, ip_1, ip_2, date_1, date_2){
  
  ip_list <- sprintf(("'%s', '%s'"), ip_1, ip_2)
  
  ##Define query for getting traffic data
  db_query_traffic_data <- 
    sprintf(paste0("select * from prototype ",
                   "where ip in (%s) and ",
                   "first_time >='%s' and first_time<'%s';")
            ,ip_list,date_1,date_2)
  
  
  
  traffic_data <- call_db(db_query_traffic_data)
  sensor_data <- get_all_sensors_of_a_corridor(corr_name, ip_1, ip_2)
  
  if(nrow(traffic_data)==0 & unique(traffic_data$ip!=2)){
    print("There is no data during this time.")
  } else {
    # print(head(traffic_data))
    ## Add sequence information to the traffic_data
    traffic_data_merge_sequence <-
      merge(traffic_data,
            sensor_data[, c("db_ip", "seq")],
            by.x = "ip", by.y = "db_ip")
    return(traffic_data_merge_sequence)
  }
}



## first aspect of data processing: set original and destination points
set_od_pair <- function(traffic_data) {
  
  traffic_data <- traffic_data %>% 
    arrange(mac_address, first_time) %>%
    select(ip, first_time, location, mac_address, seq) ## Adjust later if we need period information
  
  origin <- traffic_data %>% slice(1:(n()-1))
  dest <- traffic_data %>% slice(2:n())
  
  origin <- setNames(origin,c("ip_o","time_o","location_o","mac_o","seq_o"))
  dest <- setNames(dest,c("ip_d","time_d","location_d","mac_d","seq_d"))
  
  od <- bind_cols(origin, dest)
  od_sub <- filter(od, mac_o == mac_d & ip_o != ip_d)
  
  ## Check if there is valid data
  if (nrow(od_sub) == 0){
    print("There is no valid data after setting od pairs.")
    
  } else {
    print(paste("There are", nrow(od_sub), "records after od pairing up."))
    return(od_sub)
  }
  
}



## second aspect of data processing: set new columns
set_new_cols <- function(od, tt_t_1, tt_t_2, temp_agg) {
  
  od <- od %>% 
    mutate(ttime = time_d-time_o, 
           dir = ifelse(seq_d-seq_o==1, "Direction 1", "Direction -1"),
           full_day = weekdays(time_o),
           day_type = ifelse(full_day =="Saturday" | 
                               full_day =="Sunday", "Weekend", "Weekday"),
           d_time = strptime(time_o,"%Y-%m-%d %H:%M:%S")$hour*60+
                    strptime(time_o,"%Y-%m-%d %H:%M:%S")$min,
           tod = cut(d_time,breaks=c(seq(0,1440,temp_agg/60)))) %>%
    filter((as.numeric(ttime, units = "secs") < tt_t_1 & dir == "Direction 1") | 
          (as.numeric(ttime, units = "secs") < tt_t_2 & dir == "Direction -1"))
  
  od$ttime <- as.numeric(od$ttime, units="secs")  
  
  od$tod <- mapvalues(od$tod, 
                      from = levels(od$tod), 
                      to=set_x_levels(3600))
  
  return(od)
}



apply_mad <- function(od_edit, mad_factor){
  

  od_aggregate <- od_edit %>% 
    group_by(tod, ip_o, ip_d, dir, day_type) %>% 
    summarize(ttime_median=median(ttime), 
              ttime_mad = mad(ttime), 
              ttime_count = length(ttime))
  
  od_mad <- merge(od_edit, 
                  od_aggregate, 
                  type='inner', 
                  by=c("tod","ip_o","ip_d","dir","day_type"))
  
  od_mad$valid <- "Invalid"
  
  od_mad$valid[od_mad$ttime<=od_mad$ttime_median+
                 od_mad$ttime_mad*1.4826*mad_factor & 
                 od_mad$ttime>=od_mad$ttime_median-
                 od_mad$ttime_mad*1.4826*mad_factor]<-"Valid"
  
  return(od_mad %>% filter(valid=="Valid"))
}



fill_missing_data <- function(df, ip_1, ip_2, tt_factor, temp_agg){

  default_ttime <- get_tt_threshold_from_db(ip_1, ip_2, tt_factor) / tt_factor 
  day_types <- unique(df$day_type)
  tods <- set_x_levels(temp_agg)
  dirs <- unique(df$dir)
  
  df$tod <- as.character(df$tod)
  for (dy in day_types){
    for (dr in dirs){
      for (td in tods){
        sub <- df[df$tod==td & df$dir==dr & df$day_type==dy,]
        if(nrow(sub)!=1){


          temp <- df[df$dir==dr & df$day_type==dy,]
          temp_1 <- temp[1,]
          
          temp_1$tod <- td
          temp_1$ttime_average <- default_ttime

          temp_1$ttime_count <- 0
          
          df <- rbind(df, temp_1)
        }
      }
    }
  }
  return(df)
}

segment_agg <- function(od_after_mad, ip_1, ip_2, tt_factor, temp_agg){
  
  od_mad_agg <- od_after_mad %>% 
    group_by(tod, ip_o, ip_d, dir, day_type) %>% 
    summarize(ttime_average=as.integer(mean(ttime)),
              ttime_count = length(ttime))
  
  od_mad_agg_na_omit <- na.omit(od_mad_agg)
  
  numOfGroups <- length(unique(od_mad_agg_na_omit$tod))*
    length(unique(od_mad_agg_na_omit$dir))*
    length(unique(od_mad_agg_na_omit$day_type))
  # print(numOfGroups)
  if(nrow(od_mad_agg_na_omit) != numOfGroups){
    od_mad_agg_fixed <- fill_missing_data(od_mad_agg_na_omit, ip_1, ip_2, tt_factor, temp_agg)
  } else {
    od_mad_agg_fixed <- od_mad_agg_na_omit
  }
  
  return(od_mad_agg_fixed)
}

single_2point <- function(corr_name, ip_1, ip_2, date_1, date_2, tt_factor, temp_agg, mad_factor){
  traffic_data <- call_traffic_data(corr_name, ip_1, ip_2, date_1, date_2)
  od_data <- set_od_pair(traffic_data)
  tt_t_1 <- get_tt_threshold_from_db(ip_1, ip_2, tt_factor)
  tt_t_2 <- get_tt_threshold_from_db(ip_1, ip_2, tt_factor)
  processed_od_data <- set_new_cols(od_data, tt_t_1, tt_t_2, temp_agg)
  apply_mad_data <- apply_mad(processed_od_data, mad_factor)
  # agg_data <- segment_agg(apply_mad_data, ip_1, ip_2, tt_factor, temp_agg)
  return(apply_mad_data)
}




# test_single_2point <- single_2point("Burnet", "172.16.29.23", "172.16.29.20", d_1, d_2, tt_fac, t_agg, mad_fac)
# test_segment_agg <- segment_agg(test_single_2point, ip_1, ip_2, tt_fac, t_agg)

apply_2point_corridor <- function(corr_name, ip_1, ip_2, date_1, date_2, tt_facor, temp_agg, mad_factor){
  all_sensors <- get_all_sensors_of_a_corridor(corr_name, ip_1, ip_2) %>%
    arrange(seq)
  all_sensors_ip <- all_sensors$db_ip
  df_agg <- data.frame()
  seg_num <- 1

  for(i in 1:(length(all_sensors_ip)-1)){
    sensor_ip_1 <- all_sensors_ip[i]
    sensor_ip_2 <- all_sensors_ip[i+1]
    single_2point_data_after_mad <- single_2point(corr_name, sensor_ip_1, sensor_ip_2, date_1, date_2, tt_facor, temp_agg, mad_factor)
    single_2point_data_after_mad_agg <- segment_agg(single_2point_data_after_mad, sensor_ip_1, sensor_ip_2, tt_facor, temp_agg) %>% 
      arrange(tod, day_type, dir) %>% mutate(seg_id=paste0("seg_", seg_num))
    # print(paste("num of columns of single_2point_agg: ", ncol(single_2point_data_after_mad_agg)))
    # print(head(single_2point_data_after_mad_agg))
    df_agg <- df_agg %>% rbind.data.frame(single_2point_data_after_mad_agg)
    # print(paste("num of columns of df_agg: ", ncol(df_agg)))
    # print(head(df_agg))
    # print(paste(seg_num, sensor_ip_1, sensor_ip_2))
    # print(single_2point_data_after_mad_agg)
    # print(paste("agg num of rows: ", nrow(single_2point_data_after_mad_agg)))
    seg_num <- seg_num + 1
  }


  return(df_agg)
}

# test_apply_2point_corridor <- apply_2point_corridor(cor_name, ip_1, ip_2, d_1, d_2, tt_fac, t_agg, mad_fac)

get_end_seq <- function(corr_name){
  sensors <- get_sensors_given_corridor(corr_name)
  end_seq <- max(sensors$seq)
  return(end_seq)
}


# Function used to check if the chosen ips are the end of corridor so there is no way to use 4point method
check_end <- function(corr_name, ip_1, ip_2){
  end_flag <- FALSE
  sensors <- get_all_sensors_of_a_corridor(corr_name, ip_1, ip_2)
  max_corr_seq <- get_end_seq(corr_name)
  # print(paste("The max_seq is ", max_corr_seq))
  # need a function to get the end seq of sensors of a corridor
  seq_1 <- min(sensors$seq)
  seq_2 <- max(sensors$seq)
  if(seq_1==1 | seq_2==max_corr_seq){
    end_flag <- TRUE
  }
  # print(end_flag)
  return(end_flag)
}

# find the sensor ip ahead 
get_ip_ahead <- function(corr_name, ip_1, ip_2){
  sensors <- get_sensors_given_corridor(corr_name)
  
  sensors_2 <- get_all_sensors_of_a_corridor(corr_name, ip_1, ip_2)
  # print(paste("The two sensors in get_ip_ahead", sensors_2))
  seq_min <- min(sensors_2$seq)
  seq_ahead <- seq_min - 1
  ip_ahead <- sensors$db_ip[sensors$seq==seq_ahead]
  return(ip_ahead)
}

# test get_ip_ahead
# test_get_ip_ahead <- get_ip_ahead(cor_name, ip_1, ip_2)

# find the sensor ip after
get_ip_after <- function(corr_name, ip_1, ip_2){
  sensors <- get_sensors_given_corridor(corr_name)
  
  sensors_2 <- get_all_sensors_of_a_corridor(corr_name, ip_1, ip_2)
  # print(paste("The two sensors in get_ip_ahead", sensors_2))
  seq_max <- max(sensors_2$seq)
  seq_after <- seq_max + 1
  ip_after <- sensors$db_ip[sensors$seq==seq_after]
  return(ip_after)
}
# test get_ip_after
# test_get_ip_after <- get_ip_after(cor_name, ip_1, ip_2)

# test_check_end <- check_end(cor_name, ip_1, ip_2)
# 

ip_list_for_4point <- function(corr_name, ip_1, ip_2){
  ip_ahead <- get_ip_ahead(corr_name, ip_1, ip_2)
  ip_after <- get_ip_after(corr_name, ip_1, ip_2)
  
  ip_list <- sprintf(("'%s', '%s', '%s', '%s'"), ip_ahead, ip_1, ip_2, ip_after)
  
  return(ip_list)
}

ip_list_for_4point_list_form <- function(corr_name, ip_1, ip_2){
  ip_ahead <- get_ip_ahead(corr_name, ip_1, ip_2)
  ip_after <- get_ip_after(corr_name, ip_1, ip_2)
  
  ip_vec <- c(ip_ahead, ip_1, ip_2, ip_after)
  
  return(ip_vec)
}


# ip_1 <- '172.16.29.23'
# ip_2 <- '172.16.28.27'
# test_ip_list_for_4point <- ip_list_for_4point(cor_name, ip_1, ip_2)
# test_ip_list_for_4point_list_form <- ip_list_for_4point_list_form(cor_name, ip_1, ip_2)



call_traffic_data_4point <- function(corr_name, ip_1, ip_2, date_1, date_2){
  
  ip_list <- ip_list_for_4point(corr_name, ip_1, ip_2)
  
  ##Define query for getting traffic data
  db_query_traffic_data <- 
    sprintf(paste0("select * from prototype ",
                   "where ip in (%s) and ",
                   "first_time >='%s' and first_time<'%s';")
            ,ip_list,date_1,date_2)
  
  traffic_data <- call_db(db_query_traffic_data)
  
  sensor_data <- get_sensors_given_corridor(corr_name)
  # print(sensor_data)
  
  if(nrow(traffic_data)==0 & unique(traffic_data$ip!=4)){
    print("There is no data during the chosen time period for 4point_method.")
  } else {
    # print(head(traffic_data))
    ## Add sequence information to the traffic_data
    traffic_data_merge_sequence <-
      merge(traffic_data,
            sensor_data[, c("db_ip", "seq")],
            by.x = "ip", by.y = "db_ip")
    # print("traffic_data_merge_seq")
    # print(head(traffic_data_merge_sequence))
  }
  
  return(traffic_data_merge_sequence %>% arrange(mac_address, first_time))
}

# test_call_traffic_data_4point <- call_traffic_data_4point(cor_name, ip_1, ip_2, d_1, d_2)

single_4point <- function(corr_name, ip_1, ip_2, date_1, date_2, tt_factor, temp_agg, mad_factor){

  end_flag <- check_end(corr_name, ip_1, ip_2)
  if(end_flag){
    print(paste("For segment: ", ip_1, ip_2))
    print("4point method can't be used, 2point_method applies")
    df <- single_2point(corr_name, ip_1, ip_2, date_1, date_2, tt_factor, temp_agg, mad_factor)
    # use 2point_method
    return(df)
  } else {
    print(paste("For segment: ", ip_1, ip_2))
    print("4point method can be used")
    traffic_data <- call_traffic_data_4point(corr_name, ip_1, ip_2, date_1, date_2) %>% select(-last_time)
    
    seq_list <- get_seq_of_sensors_for_4point(corr_name, ip_1, ip_2)
    df <- data.frame()
    df <- clean_data_asc(traffic_data, df, seq_list, 600)
    df <- clean_data_desc(traffic_data, df, seq_list, 600)
    
    if (nrow(df)){
      od_pair_df <- set_od_pair(df) %>% 
        filter((seq_o==seq_list[2] & seq_d==seq_list[3]) | 
                 (seq_o==seq_list[3] & seq_d==seq_list[2]))
      if (nrow(od_pair_df)){
        tt_t_1 <- get_tt_threshold_from_db(ip_1, ip_2, tt_factor)
        tt_t_2 <- get_tt_threshold_from_db(ip_2, ip_1, tt_factor)
        od_added_new_col <- set_new_cols(od_pair_df, tt_t_1, tt_t_2, temp_agg)
        
        od_after_apply_mad <- apply_mad(od_added_new_col, mad_factor)
        return(od_after_apply_mad)
      }
      
    } else {
      print("There is no data after data cleaning by clean_data_asc and clean_data_desc")
    }
    
  }
  
  
}


get_seq_of_sensors_for_4point <- function(corr_name, ip_1, ip_2){
  sensors <- get_sensors_given_corridor(corr_name)
  
  ip_ahead <- get_ip_ahead(corr_name, ip_1, ip_2)
  ip_after <- get_ip_after(corr_name, ip_1, ip_2)
  seq_list <- c(sensors$seq[sensors$db_ip==ip_ahead],
                sensors$seq[sensors$db_ip==ip_1],
                sensors$seq[sensors$db_ip==ip_2],
                sensors$seq[sensors$db_ip==ip_after])
  
  return(seq_list)
}

# test_get_seq_sensors_for_4point <- get_seq_of_sensors_for_4point(cor_name, ip_1, ip_2)


clean_data_asc <- function(all_points_1, all_points_2, seq_list, tt_threshold){
  
  
  seq_1 <- min(seq_list)
  seq_2 <- max(seq_list)
  mac_list <- unique(all_points_1$mac_address)
  # print("The mac_list are: ")
  # print(mac_list)
  
  for(mac_add in mac_list){
    
    subset_data <- all_points_1[all_points_1$mac_address==mac_add,]
    # print("The sub_data is: ")
    # print(subset_data)
    
    if(length(unique(subset_data$seq))==4){
      temp_ind <- min(which(subset_data$seq==seq_1))
      one_row <- subset_data[temp_ind,]
      df <- rbind(subset_data, one_row)
      index_1 <- which(df$seq==seq_1)
      n <- length(index_1)
      for (i in 1:(n-1)){
        if(index_1[i+1]-index_1[i]>=3){
          df_sub <- df[index_1[i]:(index_1[i+1]-1),]
          if(length(unique(df_sub$seq))==4){
            first_ind <- min(which(df_sub$seq==seq_2))
            df_sub <- df_sub[1:first_ind,]
            if(all(diff(df_sub$seq)>=0)){
              if(length(unique(df_sub$seq))==4){
                m <- nrow(df_sub)
                df_1 <- df_sub[1:(m-1),]
                df_1 <- setNames(df_1, c("ip_o","time_o","location_o","mac_o","seq_o"))
                df_2 <- df_sub[2:m,]
                df_2 <- setNames(df_2, c("ip_d","time_d","location_d","mac_d","seq_d"))
                df_3 <- cbind(df_1, df_2)
                df_3$ttime <- df_3$time_d-df_3$time_o
                # print("The timedif is: ")
                # print(df_3$ttime)
                # print(paste("The tt_threshold: ", tt_threshold))
                if(all(as.numeric(df_3$ttime, units="secs")<=(tt_threshold))){
                  all_points_2 <- rbind(all_points_2, df_sub)
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(all_points_2)
}

# Used to test clean_data_asc
# traffic_data <- call_traffic_data_4point(cor_name, ip_1, ip_2, d_1, d_2) %>% select(-last_time)
# 
# seq_list <- get_seq_of_sensors_for_4point(cor_name, ip_1, ip_2)
# df <- data.frame()
# df <- clean_data_asc(traffic_data, df, seq_list, 600)
# 
# df_1 <- data.frame()
# df_1 <- clean_data_asc(traffic_data, df_1, seq_list, 100)


clean_data_desc <- function(all_points_1, all_points_2, seq_list, tt_threshold){
  
  
  seq_2 <- min(seq_list)
  seq_1 <- max(seq_list)
  mac_list <- unique(all_points_1$mac_address)
  
  for(mac_add in mac_list){
    
    subset_data <- all_points_1[all_points_1$mac_address==mac_add,]
    if(length(unique(subset_data$seq))==4){
      temp_ind <- min(which(subset_data$seq==seq_1))
      one_row <- subset_data[temp_ind,]
      df <- rbind(subset_data, one_row)
      index_1 <- which(df$seq==seq_1)
      n <- length(index_1)
      for (i in 1:(n-1)){
        if(index_1[i+1]-index_1[i]>=3){
          df_sub <- df[index_1[i]:(index_1[i+1]-1),]
          if(length(unique(df_sub$seq))==4){
            first_ind <- min(which(df_sub$seq==seq_2))
            df_sub <- df_sub[1:first_ind,]
            if(all(diff(df_sub$seq)<=0)){
              if(length(unique(df_sub$seq))==4){
                m <- nrow(df_sub)
                df_1 <- df_sub[1:(m-1),]
                df_1 <- setNames(df_1, c("ip_o","time_o","location_o","mac_o","seq_o"))
                df_2 <- df_sub[2:m,]
                df_2 <- setNames(df_2, c("ip_d","time_d","location_d","mac_d","seq_d"))
                df_3 <- cbind(df_1, df_2)
                df_3$ttime <- df_3$time_d-df_3$time_o
                # print("The timedif is: ")
                # print(df_3$ttime)
                # print(paste("The tt_threshold: ", tt_threshold))
                if(all(as.numeric(df_3$ttime, units="secs")<=(tt_threshold))){
                  all_points_2 <- rbind(all_points_2, df_sub)
                }
              }
            }
          }
        }
      }
    }
  }
  
  return(all_points_2)
}

# Test single_4point
# Compare the amount of records from two methods
# Hugh differences in terms of amount of records mean the results could be deviated if we only use 2point_method
# test_4_1 <- single_4point(cor_name, ip_1, ip_2, d_1, d_2, tt_fac, t_agg, mad_fac)
# test_2_1 <- single_2point(cor_name, ip_1, ip_2, d_1, d_2, tt_fac, t_agg, mad_fac)


# ip_1 <- "172.16.29.23"
# ip_2 <- "172.16.28.27"
# test_4_2 <- single_4point(cor_name, ip_1, ip_2, d_1, d_2, tt_fac, t_agg, mad_fac)
# test_2_2 <- single_2point(cor_name, ip_1, ip_2, d_1, d_2, tt_fac, t_agg, mad_fac)


# ip_1 <- "172.16.28.27"
# ip_2 <- "172.16.28.20"
# test_4_3 <- single_4point(cor_name, ip_1, ip_2, d_1, d_2, tt_fac, t_agg, mad_fac)
# test_2_3 <- single_2point(cor_name, ip_1, ip_2, d_1, d_2, tt_fac, t_agg, mad_fac)

# Test end case, which can't use 4point, so 2point will apply
# ip_1 <- "172.16.29.27"
# ip_2 <- "172.16.29.20"
# test_4_3 <- single_4point(cor_name, ip_1, ip_2, d_1, d_2, tt_fac, t_agg, mad_fac)
# test_2_3 <- single_2point(cor_name, ip_1, ip_2, d_1, d_2, tt_fac, t_agg, mad_fac)


apply_4point_corridor <- function(corr_name, ip_1, ip_2, date_1, date_2, tt_facor, temp_agg, mad_factor){
  all_sensors <- get_all_sensors_of_a_corridor(corr_name, ip_1, ip_2) %>%
    arrange(seq)

  all_sensors_ip <- all_sensors$db_ip
  print("All sensors ips along the corridor are: ")
  print("-------------------------")
  print(all_sensors_ip)
  
  df_agg <- data.frame()
  seg_num <- 1
  
  for(i in 1:(length(all_sensors_ip)-1)){
    sensor_ip_1 <- all_sensors_ip[i]
    sensor_ip_2 <- all_sensors_ip[i+1]
    single_4point_data_after_mad <- single_4point(corr_name, sensor_ip_1, sensor_ip_2, date_1, date_2, tt_facor, temp_agg, mad_factor)
    single_4point_data_after_mad_agg <- segment_agg(single_4point_data_after_mad, sensor_ip_1, sensor_ip_2, tt_facor, temp_agg) %>% 
      arrange(tod, day_type, dir) %>% mutate(seg_id=paste0("seg_", seg_num))
    # print(paste("num of columns of single_4point_agg: ", ncol(single_4point_data_after_mad_agg)))
    # print(head(single_4point_data_after_mad_agg))
    df_agg <- df_agg %>% rbind.data.frame(single_4point_data_after_mad_agg)
    # print(paste("num of columns of df_agg: ", ncol(df_agg)))
    # print(head(df_agg))
    # print(paste(seg_num, sensor_ip_1, sensor_ip_2))
    # print(single_4point_data_after_mad_agg)
    # print(paste("agg num of rows: ", nrow(single_4point_data_after_mad_agg)))
    seg_num <- seg_num + 1
  }
  
  
  return(df_agg)
}


# # weekday should be 0 or 1
# # 1 means data only contains records for weekdays
# plot_single_segment <- function(plot_data, weekday){
#   if(weekday==1){
#     plot_data <- plot_data[plot_data$day_type=="Weekday",]
#   } else {
#     plot_data <- plot_data[plot_data$day_type=="Weekend",]
#   }
#   
#   p <- ggplot(plot_data, aes(x=tod, y=ttime/60, group = dir, color = dir)) + 
#     geom_point() +
#     xlab("Time of Day") +
#     ylab("Time") +
#     facet_grid(~ dir) +
#     stat_summary(fun.y = median, geom = "line")
#   
#   return(p)
# }
# 
# # # test for weekday data
# # plot_test_2point_1 <- plot_single_segment(test_2_1, 1)
# # plot_test_4point_1 <- plot_single_segment(test_4_1, 1)
# 
# 
# plot_corridor <- function(plot_data, weekday){
#   if(weekday==1){
#     plot_data <- plot_data[plot_data$day_type=="Weekday",]
#   } else {
#     plot_data <- plot_data[plot_data$day_type=="Weekend",]
#   }
# 
#   p <- ggplot(plot_data, aes(x=tod, y=ttime_average/60, group = dir,fill=seg_id, label=label)) +
#     geom_bar(stat="identity") +
#     geom_text(colour = 'white', size = 4, position = position_stack(vjust = .5)) +
#     xlab("Time of Day") +
#     ylab("Time") +
#     facet_grid(~ dir) +
#     theme(legend.position="bottom")
#   return(p)
# }
# 
# 
# 
# # This function is used to show the quality of corridor travel time calculation by
# # showing how many records of each group are missing from the traffic data.
# # There are 96 groups of each segment, #tod*#dir*#day_type(24*2*2=96)
# 
# # count_threshold indicates threshold of number of records count of each group
# # 0 means show the empty record group and 5 shows the group with number of records <= 5
# data_quality_check <- function(corridor_agg_data, count_threshold, weekday){
# 
#   
#   sub_df <- corridor_agg_data[corridor_agg_data$ttime_count<=count_threshold, ]
#   num_sub_df <- nrow(sub_df)
#   num_sub_agg_data <- nrow(corridor_agg_data)
#   if(weekday==1){
#     num_sub_agg_data <- num_sub_agg_data[which(num_sub_agg_data$day_type=="Weekday"), ]
#   } else if (weekday==0) {
#     num_sub_agg_data <- num_sub_agg_data[which(num_sub_agg_data$day_type=="Weekend"), ]
#   } else {
#     print("Please specify the weekday value: 1 means weekday, 0 means weekend.")
#   }
#   percent <- round(num_sub_df*100/num_sub_agg_data, 0)
#   print(paste(percent, " percent of record in the agg table is <= ", count_threshold))
#   return(num_sub_agg_data)
# }
  

#Test
cor_name <- "Burnet"
i_1 <- "172.16.29.23" # Burnet @ Braker
i_2 <- "172.16.28.20" # Burnet @ Anderson

# cor_name <- "Lamar"
# i_1 <- "172.16.179.35"
# i_2 <- "172.16.179.46"
#
# # Data will be missing from other period. Need be careful of choosing period
# # to avoid error of code.
# d_1 <- "2016-04-08"
# d_2 <- "2016-04-30"
# tt_fac <- 5
# t_agg <- 3600
# mad_fac <- 5
# #
# #
# test_apply_4point_corridor <- apply_4point_corridor(cor_name, i_1, i_2, d_1, d_2, tt_fac, t_agg, mad_fac)
# test_quality_check_4point <- data_quality_check(test_apply_4point_corridor, 1, 1)
# test_apply_4point_corridor$label <- as.character(round(test_apply_4point_corridor$ttime_average/60, 1))
# test_plot_corridor_4point <- plot_corridor(test_apply_4point_corridor, 1)

# test_apply_2point_corridor <- apply_2point_corridor(cor_name, i_1, i_2, d_1, d_2, tt_fac, t_agg, mad_fac)
# test_quality_check_2point <- data_quality_check(test_apply_2point_corridor, 0)
# test_plot_corridor_2point <- plot_corridor(test_apply_2point_corridor, 1)


# ## Test for segment missing data
# test_segment <- call_db("select * from segment_table;") %>% filter(tt_avg>0)
# 
# for (i in 1:nrow(test_segment)){
#   cor_name <- test_segment[i,]$corridor_1
#   i_1 <- test_segment[i,]$ip_1
#   i_2 <- test_segment[i,]$ip_2
#   d_1 <- "2016-04-08"
#   d_2 <- "2016-05-10"
#   tt_fac <- 5
#   t_agg <- 3600
#   mad_fac <- 5
#   
#   
#   df <- single_4point(cor_name, i_1, i_2, d_1, d_2, tt_fac, t_agg, mad_fac)
#   print(paste("For segment: ", i_1, " to ", i_2, "There are ", nrow(df), " records!"))
# }
# 
# 
# write.csv(test_segment, file = "MyData.csv")

show_segment_info <- function(cor, df_corridor){
  df <- subset(df_corridor, select=c("ip_o", "ip_d", "dir", "seg_id"))
  df <- unique(df)
  colnames(df)[which(names(df) == "ip_o")] <- "ip_origin"
  colnames(df)[which(names(df) == "ip_d")] <- "ip_destination"
  sequence_table <- call_db("select * from sequence_table;")
  sub_sequence_table <- sequence_table[sequence_table$corridor==cor,]
  # 
  # 
  df <- merge(x=df, y=sub_sequence_table[, c("db_ip", "location")], by.x="ip_origin", by.y="db_ip", all.x=TRUE)
  colnames(df)[which(names(df) == "location")] <- "location_origin"
  df <- merge(x=df, y=sub_sequence_table[, c("db_ip", "location")], by.x="ip_destination", by.y="db_ip", all.x=TRUE)
  colnames(df)[which(names(df) == "location")] <- "location_destination"

  # df <- df[with(df, order(seg_id, dir))]
  df <- df %>% dplyr::arrange(seg_id, dir)
  return(df)
}

# t <- show_segment_info("Burnet", test_apply_4point_corridor)

# weekday <- 1
# a <- test_apply_4point_corridor
# 
# if(weekday==1){
#   a <- a[a$day_type=="Weekday", ]
# } else if (weekday==0) {
#   a <- a[a$day_type=="Weekend", ]
# } else {
#   print("Please specify the weekday value: 1 means weekday, 0 means weekend.")
# }
# 
# a <- a[a$ttime_count<=1, ]
# num_sub_df <- nrow(a)
# num_sub_agg_data <- nrow(corridor_agg_data)
