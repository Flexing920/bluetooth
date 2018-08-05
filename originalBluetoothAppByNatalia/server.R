library("RPostgreSQL")
library(plyr)
library (dplyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library (DT)
library(shiny)
library(progress)
library(shinyBS)
library(scales)
library(lubridate)
#library(plotly)

shinyServer(function(input, output, session) {
  # Plot Settings
  text_axis_size = 20
  text_axis_legend_size = 9
  title_size = 25
  label_angle = 45
  
  ###Point plots: parameters to define scale. The lable levels include one hour before and after of the shown labels.
  plot_x_levels = seq(ISOdate(year(Sys.Date()), month(Sys.Date()),day(Sys.Date()), hour = 0, 0, 0, tz = ""),
                      ISOdate(year(Sys.Date()+1), month(Sys.Date()+1),day(Sys.Date()+1), hour = 0, 0, 0, tz = ""),
                      by = "1 hour") 
  lim1=as.POSIXct(strptime(paste(year(Sys.Date()),month(Sys.Date()),day(Sys.Date()),"00:00:01"), format= "%Y %m %d %H:%M:%S"))
  lim2=as.POSIXct(strptime(paste(year(Sys.Date()),month(Sys.Date()),day(Sys.Date()),"23:59:59"), format= "%Y %m %d %H:%M:%S"))
  
  
  # Default Parameters for travel time cleaning
  
  #tt_threshold_sub_1 = 90 #maximum allowable ttime in sub segments
  #tt_threshold_sub_2 = 90 #maximum allowable ttime in sub segments
  
  mad<-reactiveValues()
  mad$factor = 5 #Used in cleaning see paper, value between 1 and 5
  #in_mad_factor = 5 #Used in cleaning see paper, value between 1 and 5
  old_mad_factor=5
  agg<-reactiveValues()
  agg$new<-3600
  old_agg=3600
  dtype<-reactiveValues()
  dtype$p1=NULL
  dtype$p2=NULL
  tt_threshold<-reactiveValues()
  tt_threshold$d1 = 600 #maximum allowable ttime between first and last sensor
  tt_threshold$d2 = 600 #maximum allowable ttime in opposite direction
  old_tt=600
  old_tt2=600
  
  # Define data frames and reactive values
  sensorlist <- reactiveValues()
  sensorlist2 <- reactiveValues()
  raw <- reactiveValues()
  raw$bt_data <- data.frame()
  sensorlist$coord <-
    data.frame(
      lat = NA,
      long = NA,
      sensor_ip = NA,
      location = NA,
      seq = 0
    )
  
  sensorlist2$coord <-
    data.frame(
      lat = NA,
      long = NA,
      sensor_ip = NA,
      location = NA,
      seq = 0
    )
  
  clean <- reactiveValues()
  clean$total <-
    data.frame() #dissagregate data for teh segment between the first and last sensor - Valid and inalid
  clean$segment <-
    data.frame() #dissagregate data for all subsegments - Valid and invalid
  clean$total_agg <-
    data.frame(tod=NA,dir=NA,day_type=NA,seq_o=NA,period=NA,location_o=NA,location_d=NA,ttime.x=NA) #average data by time of day using valid data points in clean$total
  clean$show<-data.frame(tod=NA,location_o=NA,location_d=NA,dir=NA,day_type=NA,period=NA,time_o=NA,time_d=NA,ttime.x=NA,seq_o=NA,seq_d=NA,d_time=NA,valid=NA)
  
  sequence = 0
  sequence2 = 0
  
  highlight<-reactiveValues()
  highlight$day=NULL
  
  sel_corridor="congress"
  
  
  #Do these need to be rective values?
  timeperiod <- reactiveValues()
  timeperiod$range1 <- data.frame(begin = startdate, end = enddate)
  timeperiod$range2 <- data.frame(begin = startdate, end = enddate)
  
  #initialize sensor list
  #initialize time period
  
  
  
  
  
  
  
  
  
  
  
  # Tab 1 processes
  output$mapAustin <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lat = 30.238703,
              lng = -97.756708,
              zoom = 13) %>%
      addMarkers(
        lng = sensors$long,
        lat = sensors$lat, 
        # popup = paste("IP: ", sensors$db_ip, "<br>", "Street: ", sensors$id),
        popup = paste("Street: ", sensors$id),
        icon = markerIcon
      )
  })
  
  
  #Monitors the selected blue tooth sensor locations and updates the sensor list
  observe({
    click <- input$mapAustin_marker_click
    if (is.null(click))
      return()
    bt_ip <-
      sensors$db_ip[sensors$lat == click$lat &
                      sensors$long == click$lng]
    bt_loc <-
      sensors$id[sensors$lat == click$lat &
                   sensors$long == click$lng]
    sequence <<- sequence + 1
    newLine <-
      isolate(c(click$lat, click$lng, bt_ip, bt_loc, sequence))
    sensorlist$coord <- isolate(rbind(sensorlist$coord, newLine))
    leafletProxy('mapAustin', data = click) %>%
      addCircleMarkers(
        lng = click$lng,
        lat = click$lat,
        stroke = FALSE,
        fill = TRUE,
        fillColor = 'darkorange',
        fillOpacity = 0.9,
        group = "selected_bt"
      )
    updateButton(session,"reset",disabled=FALSE,style="success")
    updateButton(session,"submit",style="danger")
    updateButton(session,"calculate",style="danger")
    output$nomatches_warning<-renderText({HTML("")})
    output$period_warning<-renderText({HTML("")})
    output$period_warning_2<-renderText({HTML("")})
    output$period_warning_3<-renderText({HTML("")})
    output$period_warning_4<-renderText({HTML("")})
    output$name_problem<-renderText({HTML("")})
    
    
    output$nodata_warning<-renderText({HTML("")})
    
    if (isolate(length(sensorlist$coord[,1]))>2){
          if (isolate(timeperiod$range1[1]<timeperiod$range1[2] & isolate(timeperiod$range2[1]<timeperiod$range2[2])))
           updateButton(session,"submit",disabled=FALSE,style="danger")
    }
      
    
  })
  
  #Monitors the date range inputs
  observe({
    timeperiod$range1 <- c(input$period1[1], input$period1[2])
    
    updateButton(session,"calculate",style="danger",disabled=TRUE)
    output$nomatches_warning<-renderText({HTML("")})
    output$nodata_warning<-renderText({HTML("")})
    output$period_warning<-renderText({HTML("")})
    output$period_warning_2<-renderText({HTML("")})
    output$period_warning_3<-renderText({HTML("")})
    output$period_warning_4<-renderText({HTML("")})
    output$input_period1_text<-renderText(HTML(paste0("<b>","Period 1: ","</b>","None Selected")))
    output$input_period2_text<-renderText(HTML(paste0("<b>","Period 2: ","</b>","None Selected")))
    output$period1_plot<-renderText(HTML(paste0("<b>","Period 1: ","</b>","None Selected")))
    output$period2_plot<-renderText(HTML(paste0("<b>","Period 2: ","</b>","None Selected")))
    
    
    if (isolate(timeperiod$range1[1]<timeperiod$range1[2] )
        & (isolate(timeperiod$range2[1]<timeperiod$range2[2]  | input$period2check==FALSE))
        & isolate(length(sensorlist$coord[,1]))>2){
      output$date_warning<-renderText({HTML("")})
      updateButton(session,"submit",style="danger",disabled=FALSE) 
    }
  

    if (isolate(timeperiod$range1[1]>timeperiod$range1[2] )){
      updateButton(session,"submit",style="warning",disabled=TRUE) 
      output$date_warning<-renderText({HTML("<s1 style=color:orange>End date earlier than start date.</s1>")})
     
      return()
    }
  #  else if (isolate(length(sensorlist$coord[,1]))>2 & (isolate(timeperiod$range2[1]<timeperiod$range2[2]|isolate(input$period2check==FALSE) ))){
  #    updateButton(session,"submit",style="danger",disabled=FALSE) 
  #  }

  })
  
  observe({
    
    timeperiod$range2 <- c(input$period2[1], input$period2[2])
  
    updateButton(session,"calculate",style="danger",disabled=TRUE)
    output$nomatches_warning<-renderText({HTML("")})
    output$nodata_warning<-renderText({HTML("")})
    output$period_warning<-renderText({HTML("")})
    output$period_warning_2<-renderText({HTML("")})
    output$period_warning_3<-renderText({HTML("")})
    output$period_warning_4<-renderText({HTML("")})
    output$input_period1_text<-renderText(HTML(paste0("<b>","Period 1: ","</b>","None Selected")))
    output$input_period2_text<-renderText(HTML(paste0("<b>","Period 2: ","</b>","None Selected")))
    output$period1_plot<-renderText(HTML(paste0("<b>","Period 1: ","</b>","None Selected")))
    output$period2_plot<-renderText(HTML(paste0("<b>","Period 2: ","</b>","None Selected")))
    
    if (isolate(timeperiod$range2[1]<timeperiod$range2[2]) & isolate(timeperiod$range1[1]<timeperiod$range1[2] 
                                                                    & isolate(length(sensorlist$coord[,1]))>2)){
      output$date_warning<-renderText({HTML("")})
      updateButton(session,"submit",style="danger",disabled=FALSE) 
    }

    if (isolate(timeperiod$range2[1]>timeperiod$range2[2]) & isolate (input$period2check==TRUE)){
      updateButton(session,"submit",style="warning",disabled=TRUE) 
      output$date_warning<-renderText({HTML("<s1 style=color:orange>End date earlier than start date.</s1>")})
      return()
    }
  #  else if (isolate(length(sensorlist$coord[,1]))>2 & (input$period2check==TRUE) & (isolate(timeperiod$range1[1]<timeperiod$range1[2]))){
  #    updateButton(session,"submit",style="danger",disabled=FALSE) 
  #  }
  })
  
  #Monitor Reset button and resets sensors as appropriate
  #This may be bugged. The app seems to crash after resets. -JWH
  observe({
    if (input$reset == 0)
      return()
    isolate({
      sensorlist$coord <-
        data.frame(
          lat = NA,
          long = NA,
          sensor_ip = NA,
          location = NA,
          seq = 0
        )
      
      leafletProxy('mapAustin', session) %>% clearMarkers() %>%
        addMarkers(
          lng = sensors$long,
          lat = sensors$lat,
         # popup = paste("IP: ", sensors$db_ip, "<br>", "Street: ", sensors$id),
          popup = paste( "Street: ", sensors$id),
          icon = markerIcon
        )
      sequence <<- 0
      raw$bt_data <- data.frame()
      clean$show<-data.frame(tod=NA,location_o=NA,location_d=NA,dir=NA,day_type=NA,period=NA,time_o=NA,time_d=NA,ttime.x=NA,seq_o=NA,seq_d=NA,d_time=NA,valid=NA)
      clean$total_agg<-data.frame(tod=NA,dir=NA,day_type=NA,seq_o=NA,period=NA,location_o=NA,location_d=NA,ttime.x=NA)
      updateButton(session,"reset",disabled=TRUE)
      updateButton(session,"calculate",style="danger",disabled=TRUE)
      updateButton(session,"submit",style="danger",disabled=TRUE)
      output$input_period1_text<-renderText(HTML(paste0("<b>","Period 1: ","</b>","None Selected")))
      output$input_period2_text<-renderText(HTML(paste0("<b>","Period 2: ","</b>","None Selected")))
      output$period1_plot<-renderText(HTML(paste0("<b>","Period 1: ","</b>","None Selected")))
      output$period2_plot<-renderText(HTML(paste0("<b>","Period 2: ","</b>","None Selected")))
      output$nodata_warning<-renderText({HTML("")})
      output$nomatches_warning<-renderText({HTML("")})
      output$period_warning<-renderText({HTML("")})
      output$period_warning_2<-renderText({HTML("")})
      output$period_warning_3<-renderText({HTML("")})
      output$period_warning_4<-renderText({HTML("")})
      output$name_problem<-renderText({HTML("")})
      
    })
  })
  
  
  #Monitor Period 2 button
  observe({
    output$date_warning<-renderText({HTML("")})
    output$nodata_warning<-renderText({HTML("")})
    output$nomatches_warning<-renderText({HTML("")})
    updateButton(session,"submit",style="danger")
    output$input_period1_text<-renderText(HTML(paste0("<b>","Period 1: ","</b>","None Selected")))
    output$input_period2_text<-renderText(HTML(paste0("<b>","Period 2: ","</b>","None Selected")))
    output$period1_plot<-renderText(HTML(paste0("<b>","Period 1: ","</b>","None Selected")))
    output$period2_plot<-renderText(HTML(paste0("<b>","Period 2: ","</b>","None Selected")))
    
    
    
    if (input$period2check==FALSE){
      if (isolate(timeperiod$range1[1]<timeperiod$range1[2] & isolate(length(sensorlist$coord[,1]))>2)){
            output$date_warning<-renderText({HTML("")})
            updateButton(session,"submit",style="danger",disabled=FALSE)
      }
      else if (isolate(timeperiod$range1[1]>timeperiod$range1[2])){
        updateButton(session,"submit",style="warning",disabled=TRUE) 
        output$date_warning<-renderText({HTML("<s1 style=color:orange>End date earlier than start date.</s1>")})
      }
    }
    if (input$period2check==TRUE & (isolate(timeperiod$range2[1]>timeperiod$range2[2]|isolate(timeperiod$range1[1]>timeperiod$range1[2] )))){
      updateButton(session,"submit",style="warning",disabled=TRUE) 
      output$date_warning<-renderText({HTML("<s1 style=color:orange>End date earlier than start date.</s1>")})
    }
    
    
  })
  
  #Monitor submit button
  observe({
    if (input$submit == 0)
      return()
    #write.table(sensorlist$coord,"test.csv")
    
    #Keep only 2 sensors for computational reasons-given that the fist row is null, we keep the 2nd and last rows.
    #We add this line to avoid downloading data for all sensors
    sensorlist$coord<-isolate(rbind(sensorlist$coord[1:2,],sensorlist$coord[length(sensorlist$coord$seq),]))
    updateDateInput(session,"comparisondate",value=isolate(timeperiod$range1[1]))
    
    updateButton(session,"reset",disabled=TRUE)
    updateButton(session,"submit",disabled=TRUE)
    updateButton(session,"calculate",disabled=TRUE)
    
    
    withProgress(message = "Updating Data", value = 0.3, {
      #Passing data with a null first row
      raw$bt_data <- isolate(
        get_data(
          sensorlist$coord,
          "TRUE",
          input$period2check,
          timeperiod$range1[1],
          timeperiod$range1[2],
          timeperiod$range2[1],
          timeperiod$range2[2],
          net,
          server,
          uname,
          pwd
        )
        
      )
      
      if(isolate(length(raw$bt_data)==0)){
        updateButton(session,"submit",style="warning") 
        output$nodata_warning<-renderText({HTML("<s1 style=color:orange>No data for selected sensors during either period.</s1>")})
        updateButton(session,"reset",disabled=FALSE)
        updateButton(session,"submit",disabled=FALSE)
        updateButton(session,"calculate",disabled=FALSE) 
        
        return()
      }
      
      else if (isolate("Period 1" %in% raw$bt_data$period & ("Period 2" %in% raw$bt_data$period | input$period2check==FALSE)) ){
          updateButton(session,"submit",style="success") 
          output$date_warning<-renderText({HTML("")})
          updateButton(session,"calculate",style="danger",disabled=FALSE)
          clean$show<-data.frame(tod=NA,location_o=NA,location_d=NA,dir=NA,day_type=NA,period=NA,time_o=NA,time_d=NA,ttime.x=NA,seq_o=NA,seq_d=NA,d_time=NA,valid=NA)
          clean$total_agg<-data.frame(tod=NA,dir=NA,day_type=NA,seq_o=NA,period=NA,location_o=NA,location_d=NA,ttime.x=NA)
          output$input_period1_text<-renderText(HTML(paste0("<b>","Period 1: ","</b>","None Selected")))
          output$input_period2_text<-renderText(HTML(paste0("<b>","Period 2: ","</b>","None Selected")))
          output$period1_plot<-renderText(HTML(paste0("<b>","Period 1: ","</b>","None Selected")))
          output$period2_plot<-renderText(HTML(paste0("<b>","Period 2: ","</b>","None Selected")))
          updateButton(session,"reset",disabled=FALSE)
          updateButton(session,"submit",disabled=FALSE)
          updateButton(session,"calculate",disabled=FALSE) 
          
      }
      else {
        updateButton(session,"submit",style="warning") 
        
              if( isolate("Period 1" %in% raw$bt_data$period==FALSE ))
                  output$nodata_warning<-renderText({HTML("<s1 style=color:orange>No data for selected sensors during Period 1.</s1>")})
              else 
                  output$nodata_warning<-renderText({HTML("<s1 style=color:orange>No data for selected sensors during Period 2.</s1>")})
       
              updateButton(session,"reset",disabled=FALSE)
              updateButton(session,"submit",disabled=FALSE)
              updateButton(session,"calculate",disabled=FALSE) 
            
              
         return()
              
      
      }
        
 
            
        
      
    })
    
  })
  
  
  
  
  
  #Monitor Calculate button
  observe({
    
    
    
    if (input$calculate == 0)
      return()
    
    
    
    withProgress(message = "Cleaning Data", value = 0.3, {
      #Get dissagregate data tod,location_o,loction_d,dir,day_type,period,time_o,ip_o.x,mac_o,time_d,ip_d.x,mac_d,period2,ttime.x,ip_o.y,seq_o,ip_d.y,seq_d,d_time,ttime.y.mn,ttime.y.mad,ttime.y.n,ttime_median,ttime_mad,valid
      clean$total <- isolate(
        clean_data(
          raw$bt_data,
          sensorlist$coord,
          tt_threshold$d1,
          tt_threshold$d2,
          "2points",
          mad$factor,
          "one_segment",
          as.numeric(input$aggregation)
        )
      )
     
      if (is.character(isolate(clean$total))){
        updateButton(session,"calculate",style="warning") 
        output$name_problem<-renderText({HTML("<s1 style=color:orange>There are issues with the location name in the area you're analyzing, please contact CTR researchers (see 'About').</s1> ")})
        return()
      }
      
      
      else if (isolate(is.null(clean$total)) | isolate ("Period 1"%in%clean$total$period==FALSE) | (isolate("Period 2"%in%clean$total$period==FALSE ) & isolate(input$period2check==TRUE))){
        updateButton(session,"calculate",style="warning") 
        
        if (isolate(is.null(clean$total))){
          output$nomatches_warning<-renderText({HTML("<s1 style=color:orange>No matches between selected sensors during either period.</s1>")})
          return()
        }
        else if (isolate ("Period 1"%in%clean$total$period==FALSE) ){
          output$nomatches_warning<-renderText({HTML("<s1 style=color:orange>No matches between selected sensors during Period 1.</s1>")})
          return()
        }
        else if (isolate("Period 2"%in%clean$total$period==FALSE )){
          output$nomatches_warning<-renderText({HTML("<s1 style=color:orange>No matches between selected sensors during Period 2.</s1>")})
          return()
        }
        
      }
      else {
                    clean$show<-isolate(clean$total[,c("tod","location_o","location_d","dir","day_type","period","time_o","time_d","ttime.x","seq_o","seq_d","d_time","valid")])
                    #Aggregate Valid data points : columns: tod       dir.x day_type.x seq_o period.y       location_o        location_d ttime.x.m ttime.x.n ttime.x.sd
                    clean$total_agg <-
                      isolate(
                        aggregate(
                          ttime.x ~ tod + dir + day_type + seq_o + period + location_o + location_d,
                          data = clean$total[clean$total$valid ==
                                               "Valid", ],
                          FUN = function(x)
                            c(
                              m = mean(x),
                              n = length(x),
                              sd = sd(x)
                            )
                        )
                      )
                    updateNavbarPage(session, "nav", selected = "nav3")
                    output$input_period1_text<-renderText(HTML(paste0("<b>","Period 1: ","</b>",timeperiod$range1[1], " to ",timeperiod$range1[2])))
                    output$input_period2_text<-renderText(HTML(paste0("<b>","Period 2: ","</b>",timeperiod$range2[1], " to ",timeperiod$range2[2])))
                    output$period1_plot<-renderText(HTML(paste0("<b>","Period 1: ","</b>",timeperiod$range1[1], " to ",timeperiod$range1[2])))
                    output$period2_plot<-renderText(HTML(paste0("<b>","Period 2: ","</b>",timeperiod$range2[1], " to ",timeperiod$range2[2])))
                    
                     
      }
              
    })
    
    # # Monitor Upload File button
    # output$uploaded <- renderTable({
    #   
    #   # input$file1 will be NULL initially. After the user selects
    #   # and uploads a file, it will be a data frame with 'name',
    #   # 'size', 'type', and 'datapath' columns. The 'datapath'
    #   # column will contain the local filenames where the data can
    #   # be found.
    #   
    #   inFile <- input$uploadFile
    #   
    #   if (is.null(inFile))
    #     return(NULL)
    #   
    #   read.csv(inFile$datapath, header=input$header, sep=input$sep, 
    #            quote=input$quote)
    # })
  
    
    ####Set UI controls in Plot Tabe (not all periods include weekdays and weekends)
    
    output$weekday_control<-renderUI({
      daytypes<-unique(clean$total_agg$day_type)
      if ("Weekday"%in%clean$total_agg$day_type)
        sel="Weekday"
      else
        sel="Weekend"
      selectInput("day",NULL,sort(daytypes),selected = sel)
      
    }) 
    
    output$weekday_control_2<-renderUI({
      daytypes<-unique(clean$total_agg$day_type)
      if ("Weekday"%in%clean$total_agg$day_type){
        sel="Weekday"
        dtype$p1="Weekday"
        dtype$p2="Weekday"
      }
      else{
        sel="Weekend"
          dtype$p1="Weekend"
          dtype$p2="Weekend"
          
        }
      radioButtons("day_2",NULL,sort(daytypes),selected = sel)
      
    }) 

    ####Address potential incosistencies in teh day type of two selected periods that may prevent comparisons
    
    if (isolate("Period 2" %in% clean$total_agg$period)){
         if (isolate(length(unique(clean$total_agg$day_type[clean$total_agg$period=="Period 1"]))==1 |isolate(length(unique(clean$total_agg$day_type[clean$total_agg$period=="Period 2"]))==1 ))){
                if (isolate(length(unique(clean$total_agg$day_type[clean$total_agg$period=="Period 1"]))==2 | isolate(length(unique(clean$total_agg$day_type[clean$total_agg$period=="Period 2"]))==2))){
                    updateButton(session,"calculate",style="warning")
                    
                    output$period_warning=renderText({HTML("<s1 style=color:orange>Inconsistent day type across periods. Comparison may not be possible for weekdays or weekends.</s1>")})
                    output$period_warning_2=renderText({HTML("<s1 style=color:orange>Inconsistent day type across periods.  Comparison may not be possible for weekdays or weekends.</s1>")})
                    output$period_warning_3=renderText({HTML("<s1 style=color:orange>Inconsistent day type across periods.  Data for weekdays or weekends is missing during one period.</s1>")})
                    output$period_warning_4=renderText({HTML("<s1 style=color:orange>Inconsistent day type across periods.  Data for weekdays or weekends is missing during one period.</s1>")})
                }
                else if(isolate(clean$total_agg$day_type[clean$total_agg$period=="Period 1"][1]!=clean$total_agg$day_type[clean$total_agg$period=="Period 2"][1])){
                    updateButton(session,"calculate",style="warning")
                    output$period_warning=renderText({HTML("<s1 style=color:orange>Inconsistent day type across periods.  Comparison may not be possible for weekdays or weekends.</s1>")})
                    output$period_warning_2=renderText({HTML("<s1 style=color:orange>Inconsistent day type across periods.  Comparison may not be possible for weekdays or weekends.</s1>")})
                    output$period_warning_3=renderText({HTML("<s1 style=color:orange>Inconsistent day type across periods. Data for weekdays or weekends is missing during one period.</s1>")})
                    output$period_warning_4=renderText({HTML("<s1 style=color:orange>Inconsistent day type across periods. Data for weekdays or weekends is missing during one period.</s1>")})
                }
    
         }
    }
    
       

  })
  
  
  ########################Tab 2 processes
  ####Data table of stored Bluetooth locations
  
  output$input_period1_text<-renderText(HTML(paste0("<b>","Period 1: ","</b>","None Selected")))
  output$input_period2_text<-renderText(HTML(paste0("<b>","Period 2: ","</b>","None Selected")))
  output$period1_plot<-renderText(HTML(paste0("<b>","Period 1: ","</b>","None Selected")))
  output$period2_plot<-renderText(HTML(paste0("<b>","Period 2: ","</b>","None Selected")))
  
  
  
  output$dtable <- DT::renderDataTable({
    DT::datatable(data.frame(sensorlist$coord[,c(1,2,4,5)]))
  })
  ##Download sensors
    output$dl_sensors<-downloadHandler(
    
    filename = function () {paste("sensor-data-",Sys.Date(),".csv",sep="")},
    content = function (file) {
      write.csv(sensorlist$coord,file)
    }
  )
  
  ####Data table of dissagregate data

  output$raw_dtable <- DT::renderDataTable({
    DT::datatable(data.frame(clean$show))
  })
  
  output$dl_raw<-downloadHandler(
    
    filename = function () {paste("raw-data-",Sys.Date(),".csv",sep="")},
    content = function (file) {
      write.csv(data.frame(clean$show),file)
    }
  )
  
  

  
  
  ####Data table of travel times (first-to-last)
  output$agg_dtable <- DT::renderDataTable({
    DT::datatable(data.frame(clean$total_agg))
  })
  
  output$dl_agg<-downloadHandler(
    
    filename = function () {paste("average-data-",Sys.Date(),".csv",sep="")},
    content = function (file) {
      write.csv(data.frame(clean$total_agg),file)
    }
  )
  
  
  
  
  
  #######################Tab 3 processes
  #Redraw graph based on user input on 3rd panel
  
  observe({
    
    agg$new<-input$aggregation
    if (agg$new!=old_agg){
      
      updateButton(session,"calculate2",style="danger") 
    }
    
  })
  
  
  
  observe({
    mad$factor<-as.numeric(input$select_factor)
    
    #######Observe cleaning factors (tt threshold and factors)

   
    
    if (input$calculate2 == 0 & tt_threshold$d1==old_tt & tt_threshold$d2==old_tt2 & mad$factor==old_mad_factor & old_agg==isolate(agg$new))
      return()
   
    old_agg=isolate(agg$new)
    old_tt=tt_threshold$d1
    old_tt2=tt_threshold$d2
    old_mad_factor=mad$factor
    withProgress(message = "Re-Computing Travel Time", value = 0.3, {
      #Get dissagregate data tod,location_o,loction_d,dir,day_type,period,time_o,ip_o.x,mac_o,time_d,ip_d.x,mac_d,period2,ttime.x,ip_o.y,seq_o,ip_d.y,seq_d,d_time,ttime.y.mn,ttime.y.mad,ttime.y.n,ttime_median,ttime_mad,valid
      clean$total <- isolate(
        clean_data(
          raw$bt_data,
          sensorlist$coord,
          tt_threshold$d1,
          tt_threshold$d2,
          "2points",
          mad$factor,
          "one_segment",
          isolate(as.numeric(agg$new))
        )
      )
      #Aggregate Valid data points : columns: tod       dir.x day_type.x seq_o period.y       location_o        location_d ttime.x.m ttime.x.n ttime.x.sd
      clean$total_agg <-
        isolate(
          aggregate(
            ttime.x ~ tod + dir + day_type + seq_o + period + location_o + location_d,
            data = clean$total[clean$total$valid ==
                                 "Valid", ],
            FUN = function(x)
              c(
                m = mean(x),
                n = length(x),
                sd = sd(x)
              )
          )
        )
      updateButton(session,"calculate2",style="success")
    })
 
  })
  
  
  # Observe dtype buttons
  
  observe({
    
    dtype$p1<-input$day
    dtype$p2<-isolate(dtype$p1)
    updateRadioButtons(session,"day_2",selected=isolate(dtype$p1))
   })
  
  observe({
    
    dtype$p2<-input$day_2
    dtype$p1<-isolate(dtype$p2)
    updateRadioButtons(session,"day",selected=isolate(dtype$p2))
  })
  
  
  # Observe tt threshold slide bar
  
  observe({
    ###Converting inout to seconds
    tt_threshold$d1<-input$slider1*60
    tt_threshold$d2<-input$slider2*60
    
  })
  

  
  # Observe day for highlighting
  
  observe({
    output$hl_warning<-renderText({HTML("")})
    highlight$day<-input$comparisondate
    
    
  })
  
  
  # Plot 1
  plot1_data <- reactive({
      
      if(is.null(input$day))
        return(NULL)
    

      #updateRadioButtons(session,"day_2",selected=isolate(dtype$p1))
    
      if (length(clean$total_agg$tod) < 2) {
        data.frame(
          tod = character(0),
          dir = character(0),
          day_type = character(0),
          seq_o = numeric(0),
          period = character(0),
          location_o = character(0),
          location_d = character(0),
          ttime.x.m = numeric(0),
          ttime.x.n = numeric(0),
          ttime.x.sd = numeric(0)
        )
        
      } else if (dtype$p1 == "Weekday") {
        
        clean$total_agg[clean$total_agg$day_type == "Weekday", ]
        
      }
        else if (dtype$p1 == "Weekend") {
      
        subset(clean$total_agg,
               clean$total_agg$day_type == "Weekend")
        }
      
      
    

    
  })
  
  # Data for Plot 2
  plot2_data <- reactive({
    
    if(is.null(input$day_2))
      return(NULL)
    

  
    
      if (length(clean$total_agg$tod) < 2) {
        data.frame(
          tod = character(0),
          dir = character(0),
          day_type = character(0),
          seq_o = numeric(0),
          period = character(0),
          location_o = character(0),
          location_d = character(0),
          ttime.x.m = numeric(0),
          ttime.x.n = numeric(0),
          ttime.x.sd = numeric(0),
          period = character(0),
          dir.x = character(0)
        )
      } else if (dtype$p2 == "Weekday") {
        clean$total[clean$total$day_type == "Weekday",
                    c("location_o",
                      "location_d",
                      "time_o",
                      "valid",
                      "ttime.x",
                      "period",
                      "dir")]
      }
     else if (dtype$p2 == "Weekend") {
      
        clean$total[clean$total$day_type == "Weekend",
                    c("location_o",
                      "location_d",
                      "time_o",
                      "valid",
                      "ttime.x",
                      "period",
                      "dir")]
     }
    
    
    
  })
  
  
  #End-to-end travel time plots (using first and last sensor only)
  output$graph1_1 <- renderPlot({
    
    
    if (length(clean$total_agg$tod) < 2 | is.null(plot1_data())){
      
      return(NULL)
     
    }
    
    
    
    p1_data <- plot1_data()
    p1_data <- setNames(
      p1_data,
      c(
        "tod",
        "dir",
        "day_type",
        "seq",
        "period",
        "location_o",
        "location_d",
        "ttime"
      )
    )
    p1_data$period <- factor(p1_data$period,
                             levels = c("Period 1", "Period 2"))
    p1_data$tod <-
      strftime(as.POSIXlt(as.character(p1_data$tod), format = ("%H:%M")), format =
                 "%T")
    
   hl_data<-clean$total[date(clean$total$time_o)==highlight$day,]
   hl_period<-unique(hl_data$period)
   hl_data$tod<-strftime(as.POSIXlt(as.character(hl_data$tod), format = ("%H:%M")), format =
                           "%T")
   
    
    plot_total <- ggplot() +
      geom_line(
        data = p1_data,
        aes(
          x = strptime(tod,format="%T",tz=""),
          y = ttime[, 1]/60,
          group = period,
          color = period
        ),
        size = 1
      ) +
      facet_wrap( ~ dir) +
      theme(
        axis.text.x = element_text(size = text_axis_legend_size),
        axis.title.x = element_text(size = text_axis_size),
        axis.text.y = element_text(size = text_axis_legend_size),
        axis.title.y = element_text(size = text_axis_size)
      ) +
      xlab("Hour") +
      scale_x_datetime(date_breaks="1 hour",limits=c(lim1,lim2),expand=c(0,0),labels=strftime(plot_x_levels,format="%H:%M"))+
      ylab("Travel Time (min)") +
      theme(legend.position = "right") +
      #scale_color_brewer(name="", palette="Set1")+
      scale_color_manual(values=c("Period 1"="tomato","Period 2"="skyblue2"))+
      
      theme(plot.title = element_text(size = title_size, face =
                                        "bold")) +

      theme(axis.text.x = element_text(angle = label_angle, hjust = 1))+
      theme(legend.position = "bottom") +
      theme(legend.title=element_blank())
    if (input$ribbondisplay==TRUE) {
      plot_total<-plot_total+
        geom_ribbon(
          data = p1_data,
          aes(
            ymax = ttime[, 1]/60 + 2 * ttime[, 3]/60,
            ymin = ttime[, 1]/60 - 2 * ttime[, 3]/60,
            group = period,
            color = period,
            x = strptime(tod,format="%T",tz="")
          ),
          alpha = 0.1
        ) 
    }
    
    if (isolate(highlight$day%in%date(clean$total$time_o))){
      if (isolate(clean$total$day_type[date(clean$total$time_o)==highlight$day][1])==input$day & input$hl_hide==FALSE ){
        plot_total<-plot_total+geom_point(data=hl_data,aes(x=strptime(tod,format="%T",tz=""),y=ttime.x/60,color="Highlight"))+
        scale_color_manual(values=c("Period 1"="tomato","Period 2"="skyblue2","Highlight"="goldenrod1"))
      }
      
    }
    else{
      output$hl_warning<-renderText({HTML("<s1 style=color:orange>Selected day not in Period 1 or Period 2.</s1>")})
    }
    #ggplotly(plot_total)
    return(plot_total)
  })
  
  # Graph Displays the number of counts that were used to derive the averages displayed in graph1_1.
  output$graph1_2<-renderPlot({
    if (length(clean$total_agg$tod) < 2 | is.null(plot1_data()))
      return()
    p1_data <- plot1_data()
    p1_data <- setNames(
      p1_data,
      c(
        "tod",
        "dir",
        "day_type",
        "seq",
        "period",
        "location_o",
        "location_d",
        "ttime"
      )
    )
    p1_data$period <- factor(p1_data$period,
                             levels = c("Period 1", "Period 2"))
    
    p1_data$tod <-
      strftime(as.POSIXlt(as.character(p1_data$tod), format = ("%H:%M")), format =
                 "%T")

    plot_count<-ggplot()+
      geom_bar(
        data=p1_data,
        stat="identity",
        position="dodge",
        aes(
          x=strptime(tod,format="%T",tz=""),
          y=ttime[,2],
          group=period,
          fill=period
          )
      )+
      xlab("Hour")+
      ylab("Number of Counts")+
      facet_wrap( ~ dir) +
      theme(
        axis.text.x = element_text(size = text_axis_legend_size),
        axis.title.x = element_text(size = text_axis_size),
        axis.text.y = element_text(size = text_axis_legend_size),
        axis.title.y = element_text(size = text_axis_size)
      )+theme(axis.text.x = element_text(angle = label_angle, hjust = 1)) +
      scale_x_datetime(date_breaks="1 hour",limits=c(lim1,lim2),expand=c(0,0),labels=strftime(plot_x_levels,format="%H:%M"))+
      scale_fill_manual(values=c("Period 1"="tomato","Period 2"="skyblue2"))+
      theme(legend.position = "bottom") +
      theme(legend.title=element_blank())
    return(plot_count)
  })

  
  output$graph2 <- renderPlot({
   
    p2_data <- plot2_data()[plot2_data()$period == "Period 1", ]
    if (length(clean$total$tod[clean$total$period=="Period 1"]) < 2 | length(p2_data[,1])==0){
     
      return(NULL)
   
  }
   

    
    avg_p1 <- plot1_data()
    avg_p1 <- setNames(
      avg_p1,
      c(
        "tod",
        "dir",
        "day_type",
        "seq",
        "period",
        "location_o",
        "location_d",
        "ttime"
      )
    )
    avg_p1 <- avg_p1[avg_p1$period == "Period 1", ]
    avg_p1$tod <-
      strftime(as.POSIXlt(as.character(avg_p1$tod), format = ("%H:%M")), format =
                 "%T")
    
  
    p2_data <- setNames(p2_data,
                        c(
                          "location_o",
                          "location_d",
                          "time",
                          "valid",
                          "ttime",
                          "period",
                          "dir"
                        ))
    p2_data$time <- strftime(p2_data$time, format = "%T")
    
    plot2_total <- ggplot() +
      geom_point(
        data = p2_data,
        #data=test_points,
        aes(
          x = strptime(time,format="%T",tz=""),
          y = ttime/60,
          color = valid,
          group = valid
        ),
        size = 1.5
      ) +
    geom_line(data = avg_p1,
              #data=test_line,
              aes(x = strptime(tod, format = "%T", tz = ""), y = ttime[,1]/60, group=dir,color="Average"),
              size = 1)+ 
  
    scale_x_datetime(date_breaks="1 hour",limits=c(lim1,lim2),expand=c(0,0),labels=strftime(plot_x_levels,format="%H:%M"))+
    theme(axis.text.x = element_text(angle = label_angle, hjust = 1))+

    theme(
      axis.text.x = element_text(size = text_axis_legend_size),
      axis.title.x = element_text(size = text_axis_size),
      axis.text.y = element_text(size = text_axis_legend_size),
      axis.title.y = element_text(size = text_axis_size)
    ) +
    xlab("Hour") +
    ylab("Travel Time (min)") +
    theme(legend.position = "bottom") +
    theme(legend.title=element_blank())+
   
    scale_color_manual(values=c("Valid"="royalblue1","Invalid"="sienna1","Average"="black"))+
    theme(plot.title = element_text(size = title_size, face = "bold")) +
    facet_wrap( ~ dir)
    return(plot2_total)
  })
  
  output$graph3 <- renderPlot({
    p3_data <- plot2_data()[plot2_data()$period == "Period 2", ]
    if (length(clean$total$tod[clean$total$period=="Period 2"]) < 2 | length(p3_data[,1])==0){
    
      return(NULL)
      
    }
    
  
    
    avg_p2 <- plot1_data()
    avg_p2 <- setNames(
      avg_p2,
      c(
        "tod",
        "dir",
        "day_type",
        "seq",
        "period",
        "location_o",
        "location_d",
        "ttime"
      )
    )
    avg_p2 <- avg_p2[avg_p2$period == "Period 2", ]
    avg_p2$tod <-
      strftime(as.POSIXlt(as.character(avg_p2$tod), format = ("%H:%M")), format =
                 "%T")
   
    p3_data <- setNames(p3_data,
                        c(
                          "location_o",
                          "location_d",
                          "time",
                          "valid",
                          "ttime",
                          "period",
                          "dir"
                        ))
    p3_data$time <- strftime(p3_data$time, format = "%T")
    

    plot3_total <- ggplot() +
      geom_point(
        data = p3_data,
        aes(
          x = strptime(time, format = "%H:%M:%S", tz = ""),
          y = ttime/60,
          color = valid,
          group = valid
        ),
        size = 1.5
      ) +
      geom_line(data = avg_p2,
                aes(x = strptime(tod, format = "%H:%M:%S", tz = ""), y = ttime[, 1]/60, group=dir,color="Average"),
                size = 1) +
     # scale_color_manual(values = c("black", "blue")) +
      theme(
        axis.text.x = element_text(size = text_axis_legend_size),
        axis.title.x = element_text(size = text_axis_size),
        axis.text.y = element_text(size = text_axis_legend_size),
        axis.title.y = element_text(size = text_axis_size)
      ) +
      xlab("Hour") +
      ylab("Travel Time (min)") +
      theme(legend.position = "bottom") +
      theme(legend.title=element_blank())+
      scale_x_datetime(date_breaks="1 hour",limits=c(lim1,lim2),expand=c(0,0),labels=strftime(plot_x_levels,format="%H:%M"))+
      scale_color_manual(values=c("Valid"="royalblue1","Invalid"="sienna1","Average"="black"))+
      theme(plot.title = element_text(size = title_size, face = "bold")) +
      theme(axis.text.x = element_text(angle = label_angle, hjust = 1)) +
      facet_wrap( ~ dir)
    return(plot3_total)
  })
  

  
  #Tab 4 processes
  
  output$mapAustin2 <- renderLeaflet({
    p <- filter(sensor_sequence, corridor == sel_corridor)
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
         setView(lat = 30.238703,
              lng = -97.756708,
              zoom = 10) %>%addMarkers(
                lng = p$long,
                lat = p$lat,
              #  popup = paste("IP: ", p$db_ip, "<br>", "Street: ", p$location),
                 popup = paste( "Street: ", p$location),
                icon = markerIcon
              )
  })
  

  
  observe({
    sel_corridor<- input$corridor
    proxy <- leafletProxy("mapAustin2")
    
    p <- filter(sensor_sequence, corridor == sel_corridor)
    clearMarkers(proxy)
    proxy %>%
      addMarkers(
        lng = p$long,
        lat = p$lat,
       #popup = paste("IP: ", p$db_ip, "<br>", "Street: ", p$location),
        popup = paste("Street: ", p$location),
        icon = markerIcon
      )
  })
  
  #Monitors the selected blue tooth sensor locations and updates sensor list 2
#  observe({
#    click <- input$mapAustin2_marker_click
#    if (is.null(click))
#      return()
#    bt_ip2 <-
#      sensor_sequence$db_ip[sensor_sequence$lat == click$lat &
  #                            sensor_sequence$long == click$lng]
 #   bt_loc2 <-
#      sensor_sequence$location[sensor_sequence$lat == click$lat &
   #                              sensor_sequence$long == click$lng]
#    sequence2 <<- sequence2 + 1
#    newLine <-
#      isolate(c(click$lat, click$lng, bt_ip2, bt_loc2, sequence2))
#    sensorlist2$coord <- isolate(rbind(sensorlist2$coord, newLine))
    
#    leafletProxy('mapAustin2', data = click) %>%
 #     addCircleMarkers(
  #      lng = click$lng,
   #     lat = click$lat,
    #    stroke = FALSE,
     #   fill = TRUE,
      #  fillColor = 'darkorange',
       # fillOpacity = 0.9,
      #  group = "selected_bt2"
      #)
#  })
  
  
})
