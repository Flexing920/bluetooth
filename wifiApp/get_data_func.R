function(sl,is_subset,is_period2,p11,p12,p21,p22,nname,sname,usname,p){
  #sensor list
  #checkbox for subset
  #p11,p12,p21,p22: start end onf periods one and 2
  #network name
  #sever nameut
  #username
  #passworrd
  
  #DEBUG

  #sl=data.frame(sensor_ip="NA",stringsAsFactors = FALSE)

  #sl<-rbind(sl,"172.16.179.46")
  #sl<-rbind(sl,"172.16.179.42")
  #sl<-rbind(sl,"172.16.179.41")

  
  #is_subset=TRUE
  #is_period2=FALSE
  #p11="2016-04-08"
  #p12="2016-04-10"
  #p21="2016-04-08"
  #p22="2016-04-08"
  #nname="austin_bt"
  #sname="nmc-compute2.ctr.utexas.edu"
  #usname="bluetooth"
  #p="bt2015"
  ####################
  
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = nname,host = sname, port = 5432,user = usname, password = p)
    
    
   if (length(sl$sensor_ip)>=2 & is_subset==TRUE){
      
      ip_list=paste("'",sl$sensor_ip[2:length(sl$sensor_ip)],"'",sep="",collapse=",")
      query1=paste("select *,'Period 1' as period from prototype where ip in (",ip_list,") and first_time>='",p11,"' and first_time<='",p12,"';",sep="")
      query2=paste("select *,'Period 2' as period from prototype where ip in (",ip_list,") and first_time>='",p21,"' and first_time<='",p22,"';",sep="")
    }    else {
    
 
      query1=paste("select *,'Period 1' as period from prototype where first_time>='",p11,"' and first_time<='",p12,"';",sep="")
      query2=paste("select *,'Period 2' as period from prototype where first_time>='",p21,"' and first_time<='",p22,"';",sep="") 
    }

    
   
    
    bt_d<-dbGetQuery(con,query1)        
    if (is_period2==TRUE){
      incProgress(0.5, detail = "Period 2")
      bt_d2<-dbGetQuery(con,query2)
      bt_d<-rbind(bt_d,bt_d2)
    }
    dbDisconnect(con)
    bt_d
}