library(RPostgreSQL)
library(rgdal)
library(rgeos)
library(sp)
library(DT)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(reshape2)
library(plotly)
library(dplyr)
library(rpostgis)

source('functions.R', local=TRUE)
source("bluetooth.R", local=TRUE)

# The following server script is separated into three sections: construction of variables, output of variables, 
# and interactivity. The construction of variables interactively filteres corridor, bluetooth, and other data
# based on corridor summary table selection. If there is no selection, all the corridors are selected. 

# The output of variables section contains code to output the information to the ui. 
# Note that the construction of the map, and the output of the leaflet map is done in one step.

# The interactivity of the map watches for corridor selection and updates the map accordingly.

# All present functions are for corridor page only, the same structure can be applied to the intersections page.

# ############################# DATABASE CONNECTIONS ##############################
# pgis_parameters <- "PG:host='nmc-compute1.ctr.utexas.edu' dbname='coa_corridors' user='vista' password='vista00' port='5432'"

# Connecting to database
drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, dbname = "coa_corridors",
                 host = "nmc-compute1.ctr.utexas.edu", port = 5432,
                 user = "vista", password = "vista00")

############################# SETTING UP CORRIDOR DATA AND METADATA ##############################
#getting corridor info
corridors <- pgGetGeom(con, name="corridor_geom", geom="the_geom")
# corridors <- readOGR(dsn= pgis_parameters, layer="corridor_geom")
corr_info <- subset(corridors@data, select = c("corr_id", "corr_name"))
corr_info <- merge(corr_info, corr_short, by="corr_id")

#Queries
projects_query <- "select url, corr_id from projects, corridor_info where projects.project_id = corridor_info.project_id"

#Fetching data
pdf_data <- dbGetQuery(con, projects_query)

#Processing data
corridors@data <- left_join(corridors@data, pdf_data, by="corr_id") # Merging pdf data to corridor information



############################# SETTING UP INTERSECTION DATA AND METADATA ##############################

#Queries
intersections_query = "select * from (select foo.*, fun.intersection_name, fun.latitude, fun.longitude from (select a.intersection_id, a.source_id, b.corr_id from intersection_metdat a, intersection_corr b where a.intersection_id = b.intersection_id) foo inner join intersection_geom fun
on foo.intersection_id = fun.intersection_id) x inner join traffic_counts_metdat y on x.source_id = y.source_id"


#Fetching data
intersections = dbGetQuery(con, intersections_query)

############################# SETTING UP BLUETOOTH DATA ##############################

#Queries
bluetooth_query <- "select bluetooth_locations.location_name, bluetooth_locations.location_longitude, bluetooth_locations.location_latitude, corridor_geom.corr_id from bluetooth_locations, corridor_geom where ST_DWithin(corridor_geom.the_geom, st_setsrid(st_makepoint(bluetooth_locations.location_longitude, bluetooth_locations.location_latitude), 4326), .001);"


#Fetching data
bluetooth <-  dbGetQuery(con, bluetooth_query)

#Processing data
colnames(bluetooth) <- c("sensor_name", "lon", "lat", "corr_id")

############################# SETTING UP WAVETRONIX DATA ##############################

#Queries
wavetronix_query <- "select wavetronix_locations.location_name, wavetronix_locations.location_longitude, wavetronix_locations.location_latitude, corridor_geom.corr_id from wavetronix_locations, corridor_geom where ST_DWithin(corridor_geom.the_geom, st_setsrid(st_makepoint(wavetronix_locations.location_longitude, wavetronix_locations.location_latitude), 4326), .001);"

#Fetching data
wavetronix <-  dbGetQuery(con, wavetronix_query)
colnames(wavetronix) <- c("sensor_name", "lon", "lat", "corr_id")
wavetronix <- merge(wavetronix, wave_matchup, by="sensor_name")



############################# SETTING UP GRIDSMART DATA ##############################

gridsmart_query <- "select distinct a.location_name, a.location_longitude::numeric, a.location_latitude::numeric, b.corr_id from traffic_detectors a, corridor_geom b where a.detector_type = 'GRIDSMART' and ST_DWithin(b.the_geom, st_setsrid(st_makepoint(a.location_longitude::numeric, a.location_latitude::numeric), 4326), .001);"


#Fetching data
gridsmart <- dbGetQuery(con, gridsmart_query)
colnames(gridsmart) <- c("sensor_name", "lon", "lat", "corr_id")



############################# SETTING UP TRANSIT DATA ##############################

############################# BUS SHAPES #############################
#Queries
bus_query <- "select st_asgeojson(routes_geom.the_geom), routes_geom.st_length, routes_geom.route_id, corridor_geom.corr_id from routes_geom, corridor_geom where ST_DWithin(corridor_geom.the_geom, routes_geom.the_geom, .001);"

# Fetching data
bus <- dbGetQuery(con, bus_query)

#Processing data

# spatial <- lapply(bus$st_asgeojson, function(x) coordinates(readOGR(x, 'OGRGeoJSON')))

# lines_list <- list()

# for (i in 1:length(spatial)) {

# 	line <- SpatialLines(list(Lines(list(Line(spatial[i])), ID=paste(bus$corr_id[i], "-", bus$route_id[i], sep=""))), proj4string=CRS("+proj=longlat +datum=WGS84"))
# 	lines_list <- c(lines_list, line)

# }

# lines_df <- do.call(rbind, lines_list)

# bus <- SpatialLinesDataFrame(lines_df, data = bus[, 2:4], match.ID=FALSE)

############################# END BUS SHAPES #############################

############################# SETTING UP CORRIDOR SUMMARY TABLE ##############################

## Count number of bluetooth and wavetronix sensors as well as bus routes by corr_id + intersection and gridsmart
bluetooth_count <- as.data.frame(table(bluetooth$corr_id))
colnames(bluetooth_count) <- c("corr_id", "bluetooth_count")

wavetronix_count <- as.data.frame(table(wavetronix$corr_id))
colnames(wavetronix_count) <- c("corr_id", "wavetronix_count")

bus_count <- as.data.frame(table(bus$corr_id))
colnames(bus_count) <- c("corr_id", "bus_count")

##Count number of intersections by corr_id
intersection_count <- as.data.frame(table(intersections$corr_id))
colnames(intersection_count) <- c("corr_id", "intersection_count")

##Count number of gridsmart by corr_id
gridsmart_count <- as.data.frame(table(gridsmart$corr_id))
colnames(gridsmart_count) <- c("corr_id", "gridsmart_count")

## Creting corr_info dataframe by merging count data
corr_info <- merge(corr_info, bluetooth_count, by="corr_id", all.x = TRUE)
corr_info <- merge(corr_info, wavetronix_count, by="corr_id", all.x = TRUE)
corr_info <- merge(corr_info, bus_count, by="corr_id", all.x = TRUE)
corr_info <- merge(corr_info, intersection_count, by="corr_id", all.x = TRUE)
corr_info <- merge(corr_info, gridsmart_count, by="corr_id", all.x = TRUE)


server <- function(input, output, session) {


	############# Server script organized by tabs ###############

	## Interactivity

		# Selecting corridors to display by corridor ID (corr_id) when row selected from table
		selected_corridors <- reactive({

		if(!is.null(input$home_table_rows_selected)) {

			info <- subset(corridors@data, corr_id %in% corr_table()$corr_id[input$home_table_rows_selected])
			return(info$corr_id)

		} else {

			return(corridors@data$corr_id)
		}

		})

		
		# Filtering corridor data by those that are selected

		filtered_corr_data <- reactive({

			subset(corridors, corr_id %in% selected_corridors())

		})



		# Filtering bluetooth sensor location data by those corridors that are selected


			# bluetooth points (what data does it give you? WHERE CAN I GET THE IP ADDRESSES? 
			# FOR THESE MATCHED BLUETOOTH SENSORS????)


		filtered_blue_data <- reactive({

			subset(bluetooth, corr_id %in% selected_corridors())

		})


			#corr data with bluetooth relevant information

		filtered_blue_info <- reactive({

			filter(blue_corr_data, blue_corr_data$corridor==input$blue_corridor)

			})


		####################################################################################################


		## Filtering intersection data by those corridors that are selected
		filtered_wavetronix_data <- reactive({

			subset(wavetronix, corr_id %in% selected_corridors())

		}) ## HOW CAN I ADVANCE THIS???

		filtered_gs_locs <- reactive ({

			subset(gridsmart, corr_id %in% selected_corridors())

			})

		## Filtering wavetronix sensor location data by those corridors that are selected

			## Radar data interactivity --> still need to only show those intersections matched with corridors
		radar_intersection_query <- "select intname, curdatetime, direction, hour, speed, volume from radar_data where intname = '%s' and (curdatetime::date >= '%s' and curdatetime:: date <= '%s')"

		get_radar_vol_data <- reactive ({

			vol_data <- dbGetQuery(con, sprintf(radar_intersection_query, input$radar_vol_intersection, input$radar_vol_p1[1], input$radar_vol_p2[2]))
			vol_data$period <- ifelse(as.Date(vol_data$curdatetime) >= input$radar_vol_p2[1], "Period 2", "Period 1")
			vol_data$curdatetime <- lubridate::round_date(vol_data$curdatetime, "15 minutes")

			vol_data$minute <- as.POSIXlt(vol_data$curdatetime)$min
			vol_data$week_day <- as.POSIXlt(vol_data$curdatetime)$wday
			
			vol_data <- vol_data %>% arrange(intname, direction)

			return(vol_data)

		})

		get_radar_vel_data <- reactive ({

			speed_data <- dbGetQuery(con, sprintf(radar_intersection_query, input$radar_vel_intersection, input$radar_vel_p1[1], input$radar_vel_p2[2]))
			speed_data$period <- ifelse(as.Date(speed_data$curdatetime) >= input$radar_vel_p2[1], "Period 2", "Period 1")
			speed_data$curdatetime <- lubridate::round_date(speed_data$curdatetime, "15 minutes")
			speed_data$minute <- as.POSIXlt(speed_data$curdatetime)$min
			speed_data$week_day <- as.POSIXlt(speed_data$curdatetime)$wday
			

			speed_data <- speed_data %>% arrange(intname, direction)

			return(speed_data)

		})

		radar_vol_direction <- reactive({

			directions <- unique(as.character(get_radar_vol_data()$direction))
			names(directions) <- as.character(directions)

			})

		output$radar_vol_direction <- renderUI({

			radioButtons("radar_vol_chosen_direction", h5("Direction"),
				choices=radar_vol_direction(), selected=names(radar_vol_direction())[1])

			})

		radar_vel_direction <- reactive({


			directions <- unique(as.character(get_radar_vel_data()$direction))
			names(directions) <- as.character(directions)


			})


		output$radar_vel_direction <- renderUI({


			radioButtons("radar_vel_chosen_direction", h5("Direction"),
				choices=radar_vel_direction(), selected=names(radar_vel_direction())[1])

			})

		radar_vol_data <- reactive({

			if (!is.null(input$radar_vol_chosen_direction)) {

				p1 <- subset(get_radar_vol_data(), period=="Period 1" & direction == input$radar_vol_chosen_direction & week_day %in% c(1 ,2, 3, 4, 5)) %>% wave_vol_process() 
				p2 <- subset(get_radar_vol_data(), period=="Period 2" & direction == input$radar_vol_chosen_direction & week_day %in% c(1, 2, 3, 4, 5)) %>% wave_vol_process()
				# combined <- get_radar_vel_data() %>% wave_speed_agg_clean() %>% wave_speed_process()
				data <- as.data.frame(rbind(p1, p2))

				return(data)
			}

		})

		radar_vel_data <- reactive({

			if (!is.null(input$radar_vel_chosen_direction)) {

				p1 <- subset(get_radar_vel_data(), period=="Period 1" & direction == input$radar_vel_chosen_direction & week_day %in% c(1, 2, 3, 4, 5)) %>% wave_speed_process() 
				p2 <- subset(get_radar_vel_data(), period=="Period 2" & direction == input$radar_vel_chosen_direction & week_day %in% c(1, 2, 3, 4, 5)) %>% wave_speed_process()
				# combined <- get_radar_vel_data() %>% wave_speed_agg_clean() %>% wave_speed_process()
				data <- as.data.frame(rbind(p1, p2))

				return(data)

			}

			})


		# Filtering intersection data by those corridors that are selected

		filtered_intersect_data <- reactive({

			subset(intersections, corr_id %in% selected_corridors())


			})

		selected_intersections <- reactive({

			return(unique(filtered_intersect_data()$intersection_id))

		})

		## Filtering gridsmart location data by those corridors that are selected
			## to do

		## Filtering bus route data by those corridors that are selected
			## to do

		## Filtering avl data by those corridors that are selected
			## to do


	## Corridor summary

		# Creating interative corridor information table

		corr_table <- reactive({

			return(corr_info %>% select(corr_id, corr_short, bluetooth_count, wavetronix_count, bus_count, intersection_count, gridsmart_count))

		})

		# Outputting corridor summary table
		output$home_table <- DT::renderDataTable(
			datatable(corr_table(),
				options=list(dom='t', autoWidth = TRUE), rownames=FALSE, colnames=c("ID", 
				"Name", "Bluetooth Locations", "Wavetronix Locations", "Bus Routes", "Count Locations", "GRIDSMART Locations")))



	## Travel Time

################# Bluetooth Analysis ######################
	blue_data_range1 <- reactive({

	  # Added by TL 08032018
	  cor_name <- filtered_blue_info()$corridor_alias
	  i_1 <- filtered_blue_info()$ip1
	  i_2 <- filtered_blue_info()$ip2
	  d_1 <- as.Date(input$blue_dates[1])
	  d_2 <- as.Date(input$blue_dates[2])
	  
	  data <- apply_4point_corridor(corr_name=cor_name, 
	                                ip_1=i_1, 
	                                ip_2=i_2, 
	                                date_1=d_1, 
	                                date_2=d_2, 
	                                tt_facor=travel_time_factor, 
	                                temp_agg=temp_aggregation, 
	                                mad_factor=mad_factor

	                                )
	  
		data$period <- "Period 1"

		return(data)

		})

	blue_data_range2 <- reactive({

	  # Added by TL 08032018
	  cor_name <- filtered_blue_info()$corridor_alias
	  i_1 <- filtered_blue_info()$ip1
	  i_2 <- filtered_blue_info()$ip2
	  d_1 <- as.Date(input$blue_dates2[1])
	  d_2 <- as.Date(input$blue_dates2[2])
	  
	  data <- apply_4point_corridor(corr_name=cor_name, 
	                                ip_1=i_1, 
	                                ip_2=i_2, 
	                                date_1=d_1, 
	                                date_2=d_2, 
	                                tt_facor=travel_time_factor, 
	                                temp_agg=temp_aggregation, 
	                                mad_factor=mad_factor)

		data$period <- "Period 2"

		return(data)

		})

	blue_data <- reactive({
	  
	  data <- dplyr::bind_rows(blue_data_range1(), blue_data_range2())
	  # 
	  # data$tt <- data$ttime/60
	  # 
	  # summarized <- data %>% group_by(tod, dir, period) %>% summarise(med_tt = median(tt), 
	  # 	error = sd(tt), mean_tt = mean(tt))
	  
	  return(data)
	  
	})

	blue_plot1 <- reactive({
	  
	  data <- blue_data()[blue_data()$period == "Period 1" & blue_data()$day_type=="Weekday",]
	  
	  pd <- position_dodge(0.1)
	  
	  p <- ggplot(data=data, aes(x=tod, y=ttime_average/60, group=dir, fill=seg_id,
	                             label=as.character(round(ttime_average/60, 1))))
	  p <- p + geom_bar(stat="identity")
	  p <- p + facet_grid(~ dir) + geom_text(colour = 'white', size = 4, position = position_stack(vjust = .5))
	  # p <- p + geom_point(position=pd, size=3) + geom_line(position=pd, size=1.25)
	  # p <- p + geom_errorbar(aes(ymin=mean_tt - error, ymax=mean_tt + error), width=0, position=pd)
	  p <- p + theme(axis.text.x = element_text(angle=30, hjust=1, size=14), 
	                 plot.title=element_text(size=18, hjust=0.5), 
	                 axis.text.y = element_text(size=14), legend.title = element_blank(), legend.position="bottom")
	  p <- p + labs(title=paste(input$blue_corridor, "Period 1", "Weekday",sep=" "), 
	                x="Hour", y="Travel Time (mins)")
	  
	  return(p)
	  
	})
	
	
	blue_plot2 <- reactive({
	  
	  data <- blue_data()[blue_data()$period == "Period 2" & blue_data()$day_type=="Weekday",]
	  
	  pd <- position_dodge(0.1)
	  
	  q <- ggplot(data=data, aes(x=tod, y=ttime_average/60, group=dir, fill=seg_id,
	                             label=as.character(round(ttime_average/60, 1))))
	  q <- q + geom_bar(stat="identity")
	  q <- q + facet_grid(~ dir) + geom_text(colour = 'white', size = 4, position = position_stack(vjust = .5))
	  # q <- q + geom_point(position=pd, size=3) + geom_line(position=pd, size=1.25)
	  # q <- q + geom_errorbar(aes(ymin=mean_tt - error, ymax=mean_tt + error), width=0, position=pd)
	  q <- q + theme(axis.text.x = element_text(angle=30, hjust=1, size=14), 
	                 plot.title = element_text(size = 18, hjust=0.5), 
	                 axis.text.y = element_text(size=14), legend.title=element_blank(), legend.position="bottom")
	  q <- q + labs(title=paste(input$blue_corridor, "Period 2", "Weekday", sep=" "), 
	                x="Hour", y="Travel Time (mins)")
	  
	  
	  return(q)
	  
	})		

	segment_info_table <- reactive({
	  cor_name <- filtered_blue_info()$corridor_alias
	  df_corridor <- blue_data()
	  segment_info_df <- show_segment_info(cor_name, df_corridor)
	  return(segment_info_df)
	})
	
	# For Period 1
	segment_travel_time_table_1 <- reactive({
	  data <- blue_data()[blue_data()$period == "Period 1" & blue_data()$day_type=="Weekday",]
	  return(data)
	})
	
	# For Period 2
	segment_travel_time_table_2 <- reactive({
	  data <- blue_data()[blue_data()$period == "Period 2" & blue_data()$day_type=="Weekday",]
	  return(data)
	})
	
	output$blue_plot1 <- renderPlot({

		tt1_progress <- shiny::Progress$new(min = 0, max=3)

		on.exit(tt1_progress$close())

		tt1_progress$set(message = "Conducting Analysis for Period 1", value = 0)

		if (!is.null(filtered_blue_info())) {

			tt1_progress$set(detail = "Processing Data", value = 1)

			if (is.null(blue_data_range1())) {

				tt1_progress$set(detail = "Processing Data", value = 2)

				} else if (is.null(blue_plot1())){

					tt1_progress$set(detail = "Plotting Data", value = 3)

				}

		} 

		return(blue_plot1())

		})


	output$blue_plot2 <- renderPlot({

		tt2_progress <- shiny::Progress$new(min = 0, max=3)

		on.exit(tt2_progress$close())

		tt2_progress$set(message = "Conducting Analysis for Period 2", value = 1)

		if (!is.null(filtered_blue_info())) {

			tt2_progress$set(detail = "Processing Data", value = 2)

			if (is.null(blue_data_range2())) {

				tt2_progress$set(detail = "Processing Data", value = 2)

				} else if (is.null(blue_plot2)) {

					tt2_progress$set(detail = "Plotting Data", value = 3)

				}

		} 

		return(blue_plot2())

		})

	output$segment_info <- DT::renderDataTable({
	  
	  segment_info_table()
	  
	})
	
	output$segment_travel_time_1 <- DT::renderDataTable({
	  segment_travel_time_table_1()
	})
	
	output$segment_travel_time_2 <- DT::renderDataTable({
	  segment_travel_time_table_2()
	})
	
	## Volume Counts

		## Manual

		#Creating reactive intersection data table
		intersect_data <- reactive({

			metadata <- unique(subset(intersections, intersection_id %in% selected_intersections()))
		
			return(metadata)

		})


	intersect_info <- reactive({

		req(input$vol_peaks_filter)
 

		apply_peak_fil <- intersection_peak_filter(data = intersect_data(), peak_filter=input$vol_peaks_filter)
		apply_mod_fil <- intersection_mod_filter(data=apply_peak_fil, mod_filter=input$vol_mod_filter)
		data <- intersection_counts_filter(data=apply_mod_fil, counts_filter=input$vol_counts_filter)

		data$agg_level <- as.factor(paste(data$agg_level, "mins"))
		data$start_date <- as.Date(data$start_date)
		# data$link <- ifelse(data$origin == 'city_counts', renderUI({actionLink(download_csv, "Download")}),
		# 	sprintf('<a href="%s" target="_blank"> %s </a>', data$access, "View"))

		# data$link <- provide_access(data$origin, data$access)
		data$origin <- as.factor(data$origin)


		return(data)

	})


	output$intersect_table <- DT::renderDataTable(

	datatable(subset(intersect_info(),
		select=c("intersection_name", "start_date", 
			"agg_level", "origin", "source_id")), filter = 'top', options=list(dom='tp', autoWidth = TRUE), rownames=FALSE,
	colnames=c("Intersection Name", "Date", "Aggregation", "Source", "Filename"), escape=FALSE), escape=FALSE

	)


	int_table_clicked_row <- reactive({input$intersect_table_rows_selected})
	int_access <- reactive({intersect_info()[input$intersect_table_rows_selected, ]$access})
	int_origin <- reactive({intersect_info()[input$intersect_table_rows_selected, ]$origin})
	int_filename <- reactive({intersect_info()[input$intersect_table_rows_selected, ]$source_id})


	data_access <- reactive({

		req(int_table_clicked_row())

		if (int_origin() == 'city_counts') { 

			query <- "select data_file, channel, datetime, count_channel, count_total, time, day_of_month, month, year from city_counts where data_file = '%s' order by datetime"

			data <- dbGetQuery(con, sprintf(query, int_filename()))

			return(data)

		} else {

			return(NULL)
		}

		})



	output$data_access_table <- DT::renderDataTable({

		datatable(data_access())

		})

	output$clicked_info <- renderPrint({

		int_access()

		})



	output$download_study <- downloadHandler(

		filename = function() {

		if(!is.na(int_origin()) & int_origin() == 'city_counts') {

			return(paste("dataset", "csv", sep="."))

		} else if (int_origin() == 'city_pdf') {

			return(paste("dateset", "pdf", sep="."))
		} else {

			return(paste("dataset", "xlsx", sep="."))

		}
	},

	content = function(file) {

		if(length(int_origin()) == 1 & int_origin() == 'city_counts') {

			return(write.csv(data_access(), file, row.names=FALSE))

		} else if (length(int_origin()) == 1) {

			return(download.file(int_access(), destfile=file, mode="wb"))
		}

	}

		)


# output$num_intersections <- reactive({

# 		return(length(unique(intersect_metadata()$intersection_id)))

# 		})


		## Sensor

	radar_vol_plot <- reactive({

		req(radar_vol_data())

		wave_vol_plot(data=radar_vol_data(), 
			intname=input$radar_vol_intersection, direction=input$radar_vol_chosen_direction)

		})


	output$radar_vol_plot <- renderPlotly({

		vol_progress <- shiny::Progress$new(min = 0, max= 3)
		on.exit(vol_progress$close())
		vol_progress$set(message = "Conducting Traffic Volume Analysis", value = 0)

		if (is.null(radar_vol_direction())) {

			vol_progress$set(detail="Processing data", value = 1)

			} else if (is.null(radar_vol_data())) {

				vol_progress$set(detail = "Plotting data", value = 2)

				} else {

					vol_progress$set(detail = "Finishing up", value = 3)

				}		

		return(radar_vol_plot())

		})
	# output$radar_vol_table <- DT::renderDataTable(radar_vol_data())


	## Speed

	radar_vel_plot <- reactive({

		req(radar_vel_data())

		
		wave_speed_plot(data=radar_vel_data(), 
			intname=input$radar_vel_intersection, direction=input$radar_vel_chosen_direction)

		})

	output$radar_vel_plot <- renderPlotly({


		vel_progress <- shiny::Progress$new(min = 0, max= 3)
		on.exit(vel_progress$close())
		vel_progress$set(message = "Conducting Speed Analysis", value = 0)

		if (is.null(radar_vel_direction())) {

			vel_progress$set(detail="Processing data", value = 1)

			} else if (is.null(radar_vel_data())) {

				vel_progress$set(detail = "Plotting data", value = 2)

				} else {

					vel_progress$set(detail = "Finishing up", value = 3)

				}

			return(radar_vel_plot()) 

		})
	# output$radar_vel_table <- DT::renderDataTable(radar_vel_data())

	## Transit

		apc_volume_query <- reactive({

			"select avg(ons) as on, avg(offs) as off, corr_id, year from (select sum(ons) as ons, sum(offs) as offs, corr_id, datetime::date as day, extract(year from datetime) as year from apc_matchup_june where extract(dow from datetime) not in (0, 6) group by corr_id, day, year) as foo group by corr_id, year order by corr_id, year"

			})

		apc_volume_data <- reactive({

			data <- dbGetQuery(con, apc_volume_query())

			data <- melt(data, id.vars = c("corr_id", "year"))
			data <- merge(data, corr_info[, c("corr_id", "corr_name", "corr_short")], by="corr_id")
			data <- subset(data, corr_id %in% selected_corridors())

			total_volume <- data %>% group_by(corr_id, corr_short) %>% summarise(totalVol = sum(value))
			corr_levels <- total_volume[order(-total_volume$totalVol),]$corr_short
			data$corr_short <- factor(data$corr_short, levels = corr_levels)

			return(data)

			})

		apc_volume_plot <- reactive({

			p <- ggplot(apc_volume_data()[order(apc_volume_data()$corr_id, apc_volume_data()$variable),], aes(x=year, y=value, 
				fill=factor(variable,levels=c("off", "on")), ordered=TRUE))
			p <- p + geom_bar(stat='identity', position="stack", width=.9) + theme_classic() + coord_flip()
			p <- p + facet_grid(corr_short ~ ., switch="y", space="free_y")
			p <- p + scale_x_discrete(limits=c(2016, 2017))
			p <- p + scale_fill_manual(name="", values=c("slategray", "slategray1"), labels=c("Alightings", "Boardings"))
			p <- p + labs(x="Year", y="Number of Passengers", title="June Average Daily Passenger Counts by Corridor")
			p <- p + theme(legend.title=element_blank(), 
				strip.text = element_text(size=15), axis.text=element_text(size=13), 
				axis.title = element_text(size=16), 
				plot.title=element_text(hjust=0.5, size=19), 
				legend.position=c(0.9,.1),
				strip.text.y = element_text(angle=180, hjust=0.5), 
				strip.background=element_blank(),
				panel.spacing=unit(1, "lines"),
				axis.title.y = element_blank())

			return(p)

			})

		output$apc_volume_plot <- renderPlot({

			apc_vol_progress <- shiny::Progress$new(min = 0, max=4)
			on.exit(apc_vol_progress$close())
			apc_vol_progress$set(message = "Conducting Analysis", value = 1)

			if (is.null(apc_volume_data())) {

				apc_vol_progress$set(detail = "Processing Data", value = 2)

			} else if (is.null(apc_volume_plot())) {

				apc_vol_progress$set(detail = "Plotting Data", value = 3)

			} else {

				apc_vol_progress$set(detail = "Finishing up", value = 4)

			}

			return(apc_volume_plot())

			})


		get_dwell_data <- reactive({

			query <- "select *, extract(year from datetime) as year from apc_matchup_june where extract(dow from datetime) not in (0, 6)" #string with query

			data <- dbGetQuery(con, query)


			})


		dwell_data <- reactive({
			## processing data

			dwell_data <- get_dwell_data() %>% filter(between(dwell_time, 1, 300)) %>% 
			filter(between(ons, 1, 15)) %>% filter(between(offs, 1, 15)) 

			dwell_corr <- dwell_data %>% group_by(corr_id, year) %>% summarise(avg_dwell = mean(dwell_time))

			dwell <- left_join(dwell_corr, corr_info, by = "corr_id")

			return(dwell)
			}) ##as of now distinguishes between 2016 and 2017

		dwell_plot <- reactive({


			dwell <- dwell_data()
			total_dwell <- dwell %>% group_by(corr_short) %>% summarise(sum_dwell = sum(avg_dwell))

			corr_levels <- total_dwell[order(total_dwell$sum_dwell), ]$corr_short
			dwell$corr_short <- factor(dwell$corr_short, levels = corr_levels)

			p <- ggplot(data = dwell, aes(x=corr_short, y=avg_dwell, fill=factor(year)))
			p <- p + geom_bar(stat='identity', position=position_dodge(width=.8), width = 0.75) + 
			theme_classic() + coord_flip() + scale_y_continuous(breaks=c(0, 30, 60, 90, 120)) +
			labs(x="Year", y="Dwell time (sec)", title="June Average Dwell Time per Stop by Corridor") +
			scale_fill_manual(values= c('#93ABBB', '#4B738E')) +
			theme(legend.title=element_blank(),
				strip.text = element_text(size=15), axis.text=element_text(size=13), 
        		axis.title = element_text(size=16), 
        		plot.title=element_text(hjust=0.5, size=19),
        		strip.text.y = element_text(angle=180, hjust=0.5), 
        		strip.background=element_blank(),
        		panel.spacing =unit(1, "lines"),
        		axis.title.y = element_blank(),
        		axis.line.y = element_blank())

			return(p)

			})



		output$dwell_plot <- renderPlot({


			dwell_progress <- shiny::Progress$new(min = 0, max = 4)
			on.exit(dwell_progress$close())
			dwell_progress$set(message = "Conducting Dwell Time Analysis", value = 1)

			if (is.null(get_dwell_data())) {

				dwell_progress$set(detail = "Processing Data", value = 2)

			} else if (is.null(dwell_plot())) {

				dwell_progress$set(detail = "Plotting Data", value = 3)

			} else {

				dwell_progress$set(detail = "Finishing up", value = 4)

			}

        	return(dwell_plot())

			})



	## Map

		# Creating content for map popups when corridors are clicked
		content <- reactive({

			sprintf('%s <br> <a href="%s" target="_blank">View Corridor Study</a>', 
				filtered_corr_data()$corr_name, filtered_corr_data()$url)

			# paste(filtered_corr_data()$corr_name, "<br>", 
			# 	tags$a(href=filtered_corr_data()$url, target="_blank", "View Corridor Study"))

		})

		# Outputting map



		output$map <- renderLeaflet({ 

			leaflet(options = leafletOptions(zoomControl=FALSE)) %>% addPolylines(data=corridors) %>% addTiles(options = 
				tileOptions(minZoom = 10, maxZoom=14, opacity=0.65)) %>% 

			setView(lat=30.2672, lng=-97.7431, zoom=11) %>% 

			addLegend("topright", colors = c("#EB5252", '#a500ff', '#FFFF00', "#51EADE"),
				labels = c("Bluetooth", "Wavetronix", "GRIDSMART", "Traffic Studies"), 
				title = "Information Source", opacity = 1) 
			})

		# Map interactivity

		observe({

			leafletProxy("map")  %>% clearShapes() %>%

			addPolylines(data=filtered_corr_data(), 
				group="Corridors", popup=content(), 
				layerId = paste("corr-",filtered_corr_data()$corr_id, sep=""),
				opacity=.8, weight=3) %>% 

			addCircles(data=filtered_blue_data(), 
				color="#EB5252", fillColor="#EB5252", group="Bluetooth Sensors", opacity=0.9, 
				fillOpacity=0.9, weight=9)  %>%

			addCircles(data=filtered_intersect_data(),
				color="#51EADE", fillColor="#51EADE", group="Traffic Studies", opacity=0.9, weight=9, 
				label=paste("Intersection:", filtered_intersect_data()$intersection_name), 
				layerId = paste("int-",filtered_intersect_data()$intersection_id, sep=""))	%>%

			addCircles(data=filtered_gs_locs(), lng = ~lon, lat = ~lat,
				color='#FFFF00', fillColor='#ffa500', group='Gridsmart', opacity=0.9, 
				fillOpacity = 0.9, weight = 9) %>%

			addCircles(data=filtered_wavetronix_data(),
				color = '#a500ff', fillColor='#a500ff', group="Wavetronix", opacity=0.9, 
				fillOpacity=0.9,weight=9) %>%

			addLayersControl(overlayGroups = c("Bluetooth Sensors", "Traffic Studies", "Gridsmart", "Wavetronix"),
				options = layersControlOptions(collapsed = FALSE), position="topleft")

			})


		observe({

			if (input$tabs == 'home_tab') {

				leafletProxy("map") %>%
				 
				showGroup(c("Traffic Studies", "Bluetooth Sensors", "Gridsmart", "Wavetronix"))


			}


			if(input$tabs == 'travel_time_tab') {

				leafletProxy("map") %>%

				hideGroup(c("Traffic Studies", "Gridsmart", "Wavetronix")) %>%

				showGroup("Bluetooth Sensors")


			}

			if(input$tabs == 'traffic_volume_tab') {


				leafletProxy("map") %>% clearGroup('wavetronix_chosen') %>%

				hideGroup("Bluetooth Sensors") %>%

				showGroup(c("Traffic Studies", "Gridsmart")) 

				if(input$vol_source == 'sensor') {

					wave_vol_data <- subset(filtered_wavetronix_data(), data_name == input$radar_vol_intersection)

					leafletProxy("map") %>%  

					hideGroup(c("Traffic Studies", "Gridsmart")) %>%

					showGroup(c("Wavetronix")) %>% 
					
					addCircles(data=wave_vol_data, 
						color="#00FA9A", fillColor="#00FA9A", group="wavetronix_chosen", opacity=0.9, 
						fillOpacity=0.9, weight=12, popup=as.character(wave_vol_data$data_name))


				}

			}

			if(input$tabs == 'speed_tab') {

				wave_vel_data <- subset(filtered_wavetronix_data(), data_name == input$radar_vel_intersection)

				leafletProxy("map") %>% clearGroup('wavetronix_chosen') %>%

				hideGroup(c("Traffic Studies", "Bluetooth Sensors", "Gridsmart")) %>%

				showGroup("Wavetronix") %>%

				addCircles(data=wave_vel_data,
				color="#00FA9A", fillColor="#00FA9A", group="wavetronix_chosen", opacity=0.9,
				fillOpacity=0.9, weight=12, popup=as.character(wave_vel_data$data_name))

			}

			if(input$tabs == 'transit_tab') {

				leafletProxy("map") %>% 
				hideGroup(c("Traffic Studies", "Bluetooth Sensors", "Gridsmart", "Wavetronix"))
			}

			if(!(input$tabs == 'traffic_volume_tab' | input$tabs == 'speed_tab')) {

				leafletProxy("map") %>% clearGroup('wavetronix_chosen')

			}


			})

		observeEvent(input$radar_vel_intersection, {

			leafletProxy("map") %>% clearGroup('wavetronix_chosen')
		}

			)

		observeEvent(input$radar_vol_intersection, {

			leafletProxy("map") %>% clearGroup('wavetronix_chosen')
		}

			)


	############################ OLD ##############################################



	## Outputting selected corridor names #### not in use ######

	# output$corridor_selection <- renderPrint({
	# 	if (length(input$my_table_rows_selected)) {
	# 		cat(as.character(selected_corridors()), sep="\n")
	# 		}
	# 	})


}
