function (all_points,sensor_list,tt_t_1,tt_t_2,type_clean,mad_factor,type_spatial_agg,temp_agg){
#DEBUG
#   all_points=bt_data

 # sensor_list=data.frame(ip=character(0),location=character(0),seq=integer(0),stringsAsFactors = FALSE)
#  sensor_list[1,]<-c(NA,NA,0)

  #  sensor_list[2,]<-c("172.16.179.42",'Lamar_and_Manchca_Barton_skyway',2)
 #sensor_list[3,]<-c("172.16.179.41",'Lamar_Blue_Bonnet',3)
#  sensor_list[4,]<-c("172.16.179.46",'lamar_brodie_oak',1)

  
  
  
   #tt_t_1=600
   #tt_t_2=600
   #type_clean="2points"
   #mad_factor=5
   #type_spatial_agg="one_segment"
   # temp_agg=as.numeric(temp_agg)
   
 #  ip_list=("'172.16.179.42','172.16.179.41','172.16.179.46'")
   
#   net="austin_bt"
#   server="nmc-compute2.ctr.utexas.edu"
#   uname="bluetooth"
#   pwd="bt2015"

#  drv <- dbDriver("PostgreSQL")
#  con <- dbConnect(drv, dbname = net,host = server, port = 5432,user = uname, password = pwd)
#  bt_data<-dbGetQuery(con,sprintf("select *,'Period 1' as period from prototype where ip in (%s) and first_time >='%s' and first_time<'%s';"
 #                          ,ip_list,"2016-04-08","2016-04-10"))
  
#  all_points=bt_data
  
#  dbDisconnect(con)
  ####################
  
  
  
  
  
  
  #all_points: subset of bt_data (original format) plus column with time period
  #sensor_list: ordered list of sensors
  #tt_t_1,tt_t_2: ttime threshold in direction 1 (selected order) and 2 (opposite)
  #type_clean: 2points,3points
  #mad_factor: 2-5 based on paper
  #type_spatial_agg: one segment (first_to_last) or multiple
  #temp_agg: temporal aggregation used for cleaning
  #outputs: all bt matches (points) including a field to identify if they're valid based on cleaning  
  

  ##Adjust to various aggregation levels if neded

  x_levels=strftime(seq(ISOdate(2016,1,1,hour=0,0,0,tz=""),ISOdate(2016,1,1,hour=(24-temp_agg/3600)%/%1,60*(24-temp_agg/3600)%%1,tz=""),by=sprintf("%s min",temp_agg/60)),format="%H:%M") ##Make consistent with aggregation
  
  
  #if only start and end are needed, remove intermediate sensors
  ##Assuming we pass a first row with null values
  sensor_list<-sensor_list[,c("sensor_ip","location","seq")]
  # write.table(sensor_list,"testsensorlist.csv")
  if(type_spatial_agg=="one_segment"){
    sensor_list<-rbind(sensor_list[2,],sensor_list[length(sensor_list$seq),])
    sensor_list[sensor_list$seq==max(sensor_list$seq),]$seq=2
  }
  # write.table(sensor_list,"testsensorlist1.csv")
  
  all_points<-all_points[with(all_points, order (period,mac_address,first_time)),]
  # write.table(all_points,"testallpoints.csv")
  #rename locations for brodie oaks (172.16.179.46)
  # if ('172.16.179.46'%in%all_points$ip)
  #     all_points[all_points$ip=='172.16.179.46',]$location="lamar_brodie_oak" 
  
  #Pair up succesive reads 
  origin1<-all_points[1:(length(all_points[,1])-1),c(1,2,4,5,6)]
  dest1<-all_points[2:(length(all_points[,1])),c(1,2,4,5,6)]
  origin1<-setNames(origin1,c("time_o","ip_o","location_o","mac_o","period"))
  dest1<-setNames(dest1,c("time_d","ip_d","location_d","mac_d","period2"))
  
  od_2points<-cbind(origin1,dest1)
  # write.table(od_2points,"testod2pointswifi1.csv")
  ##Additional pairing if we want to use 3 points
  if(type_clean =="3points"){
    origin2<-od_2points[1:(length(od_2points[,1])-1),]
    dest2<-dest1[2:(length(dest1[,1])),]
    dest2<-setNames(dest2,c("time_d2","ip_d2","location_d2","mac_d2","period3"))
    od_3points<-cbind(origin2,dest2)
  }
  
  #Clean up
  origin1=data.frame()
  origin2=data.frame()
  dest1=data.frame()
  dest2=data.frame()
  
  #Clean up by sensor pair and weekday type
  #Subset OD - discard records where origin is same as destination
  ##An od is valid is mac_o=mac_d and ip_o!=ip_d. Additional cleaning will be done later- see summary table below, and sample query
  
  if (type_clean =="3points"){
    od_3points<-od_3points[od_3points$period==od_3points$period2 & od_3points$period2==od_3points$period3 &
                             od_3points$mac_o==od_3points$mac_d & od_3points$mac_d2==od_3points$mac_d & od_3points$ip_o!=od_3points$ip_d & od_3points$ip_o!=od_3points$ip_d2 & od_3points$ip_d!=od_3points$ip_d2,]
    if (length(od_3points[,1])==0){
      return()
    } 
    
    ####Add ttime difference & eliminate rows that are not in the same day. We also eliminate unlikely travel times
    od_3points$ttime=od_3points$time_d-od_3points$time_o
    ####Make sure the third point is visited on the same date
    od_3points$ttime_check=od_3points$time_d2-od_3points$time_d 
    
    ####Identify relevant segments 
    sensor_list<-setNames(sensor_list,c("ip_o","location_o","seq_o"))
    od_3points<-merge(od_3points,sensor_list,type='inner',by="location_o")
    sensor_list<-setNames(sensor_list,c("ip_d","location_d","seq_d"))
    od_3points<-merge(od_3points,sensor_list,type='inner',by="location_d")
    sensor_list<-setNames(sensor_list,c("ip_d2","location_d2","seq_d2"))
    od_3points<-merge(od_3points,sensor_list,type='inner',by="location_d2")
    ####Distinguish directions
    od_3points$dir<-""
    od_3points$dir[as.integer(od_3points$seq_d)-as.integer(od_3points$seq_o)==1 & as.integer(od_3points$seq_d2)-as.integer(od_3points$seq_d)==1]<-"Direction 1"
    od_3points$dir[as.integer(od_3points$seq_d)-as.integer(od_3points$seq_o)==-1 & as.integer(od_3points$seq_d2)-as.integer(od_3points$seq_d)==-1]<-"Direction -1"
    
    ####Apply threshold by direction
    od_3points<-od_3points[(as.numeric(od_3points$ttime)<tt_t_1 & as.numeric(od_3points$ttime_check)<tt_t_1 & od_3points$dir=="Direction 1") 
                           |(as.numeric(od_3points$ttime)<tt_t_2 & as.numeric(od_3points$ttime_check)<tt_t_2 & od_3points$dir=="Direction -1"),]
    
    
    ####Identify Weekends
    od_3points$day_type<-"Weekday"
    od_3points$day_type[weekdays(od_3points$time_o)=="Saturday" | weekdays(od_3points$time_o)=="Sunday"]<-"Weekend"
    
    
    ####Add full day information
    od_3points$full_day<-weekdays(od_3points$time_o)
    
    
    ####Add half hour time cuts and dtime
    od_3points$d_time=strptime(od_3points$time_o,"%Y-%m-%d %H:%M:%S")$hour*60+strptime(od_3points$time_o,"%Y-%m-%d %H:%M:%S")$min
    od_3points$tod=cut(od_3points$d_time,breaks=c(seq(0,1440,temp_agg/60)))
    
    ####Aggregate based on departure time, weekday/weekend, time period and direction
    agg_3points=aggregate(ttime~tod+location_o+location_d+dir+day_type+period,data=od_3points,FUN=function(x) c(mn=median(x),mad=mad(x),n=length(x)))
    
    agg_3points$ttime_median=agg_3points[,7][,1]
    agg_3points$ttime_mad=agg_3points[,7][,2]
    agg_3pointsttime_count=agg_3points[,7][,3]
    
    ####rename fields to facilitate merging ad merge
    agg_3points$tod<-mapvalues(agg_3points$tod,from=levels(agg_3points$tod),to=c(seq(0,1440-temp_agg/60,temp_agg/60)))
    od_3points$tod<-mapvalues(od_3points$tod,from=levels(od_3points$tod),to=c(seq(0,1440-temp_agg/60,temp_agg/60)))
    c_data<-merge(od_3points,agg_3points,type='inner',by=c("tod","location_o","location_d","dir","day_type","period"))
    
    ####clean data
    c_data$valid<-"Invalid"
    c_data$valid[c_data$ttime.x<=c_data$ttime_median+c_data$ttime_mad*1.4826*mad_factor & c_data$ttime.x>=c_data$ttime_median-c_data$ttime_mad*1.4826*mad_factor]<-"Valid"
    
  } else{
    
    od_2points<-od_2points[od_2points$period==od_2points$period2 & 
                             od_2points$mac_o==od_2points$mac_d & od_2points$ip_o!=od_2points$ip_d ,]
    # write.table(od_2points,"testod2pointswifi2.csv")
    if (length(od_2points[,1])==0){
      return()
    } 
    
     ####Add ttime difference & eliminate rows that are not in the same day. We also eliminate unlikely travel times
    od_2points$ttime=od_2points$time_d-od_2points$time_o
    # write.table(od_2points$ttime,"testod2pointswifitime.csv")
    
    ####Identify relevant segments 
    
    sl_o<-sensor_list
    sl_d<-sensor_list
    sl_o<-setNames(sl_o,c("ip_o","location_o","seq_o"))
    sl_d<-setNames(sl_d,c("ip_d","location_d","seq_d"))
    od_2points<-merge(od_2points,sl_o,type='inner',by="location_o")
    od_2points<-merge(od_2points,sl_d,type='inner',by="location_d")
    #####Check if merge worked, otherwise return and warn
    
    if (length(od_2points[,1])==0){
      return("Name Issue")
    }
    
    
    ####Distinguish directions
    
    
    od_2points$dir<-""
    od_2points=data.frame(od_2points)
    od_2points$dir[as.integer(od_2points$seq_d)-as.integer(od_2points$seq_o)==1]<-"Direction 1"
    od_2points$dir[as.integer(od_2points$seq_d)-as.integer(od_2points$seq_o)==-1]<-"Direction -1"
    
    ####Apply threshold by direction
    od_2points<-od_2points[(as.numeric(od_2points$ttime)<tt_t_1 & od_2points$dir=="Direction 1") 
                           |(as.numeric(od_2points$ttime)<tt_t_2 & od_2points$dir=="Direction -1"),]
    
    
    ####Identify Weekends
    od_2points$day_type<-"Weekday"
    od_2points$day_type[weekdays(od_2points$time_o)=="Saturday" | weekdays(od_2points$time_o)=="Sunday"]<-"Weekend"
    
    ####Add full day information
    od_2points$full_day<-weekdays(od_2points$time_o)
    
    ####Add half hour time cuts and d_time
    od_2points$d_time=strptime(od_2points$time_o,"%Y-%m-%d %H:%M:%S")$hour*60+strptime(od_2points$time_o,"%Y-%m-%d %H:%M:%S")$min
    od_2points$tod=cut(od_2points$d_time,breaks=c(seq(0,1440,temp_agg/60)))
    
    ####Aggregate based on departure time, weekday/weekend, time period and direction
    agg_2points=aggregate(ttime~tod+location_o+location_d+dir+day_type+period,data=od_2points,FUN=function(x) c(mn=median(x),mad=mad(x),n=length(x)))
    
    agg_2points$ttime_median=agg_2points[,7][,1]
    agg_2points$ttime_mad=agg_2points[,7][,2]
    agg_2pointsttime_count=agg_2points[,7][,3]
    
    ####rename fields to facilitate merging ad merge
    agg_2points$tod<-mapvalues(agg_2points$tod,from=levels(agg_2points$tod),to=c(seq(0,1440-temp_agg/60,temp_agg/60)))
    od_2points$tod<-mapvalues(od_2points$tod,from=levels(od_2points$tod),to=c(seq(0,1440-temp_agg/60,temp_agg/60)))
    c_data_2<-merge(od_2points,agg_2points,type='inner',by=c("tod","location_o","location_d","dir","day_type","period"))
    
    ####clean data
    c_data_2$valid<-"Invalid"
    c_data_2$valid[c_data_2$ttime.x<=c_data_2$ttime_median+c_data_2$ttime_mad*1.4826*mad_factor & c_data_2$ttime.x>=c_data_2$ttime_median-c_data_2$ttime_mad*1.4826*mad_factor]<-"Valid"
    c_data_2$tod<-mapvalues( c_data_2$tod,from=levels( c_data_2$tod),to=x_levels)
    c_data=c_data_2
    
  }
  
  #Return all points
  c_data
}