library(RPostgreSQL)
library(rgdal)
library(rgeos)
library(leaflet)
library(sp)
library(DT)
library(lubridate)
library(zoo)
library(ggplot2)
library(scales)
library(plotly)
library(dplyr)

## For wavetronix tab's drop down menus to choose intersection names and dates.

drv <- dbDriver("PostgreSQL")

con_ui <- dbConnect(drv, dbname = "coa_corridors",
                 host = "nmc-compute1.ctr.utexas.edu", port = 5432,
                 user = "vista", password = "vista00")

## For wavetronix tab's drop down menus to choose intersection names and dates.
# radar_intersections_query <- "select distinct intname from radar_data;"
# radar_intersections <- dbGetQuery(con_ui, radar_intersections_query)

radar_intersection_names <- as.character(list("BURNETPALM WAY", "KINNEYLAMAR", "LAMARCOLLIER", "BurnetRutland", "LamarBroken Spoke", "LAMARMANCHACA"))
names(radar_intersection_names) <- radar_intersection_names

radar_dates_query <- "select distinct day, month, year from radar_data;"
radar_dates_df <- dbGetQuery(con_ui, radar_dates_query)
radar_dates <- as.Date(paste(radar_dates_df$year, radar_dates_df$month, radar_dates_df$day, sep="-"))


apc_corr <- setNames(corr_id_list, corr_short_list)




ui <- fluidPage(

	tags$hr(),

	titlePanel('City of Austin Bond Corridor Tool'),

	tags$hr(),
	tags$br(),

	fluidRow(
		column(6,

			tabsetPanel(id ='tabs',

				tabPanel("Corridor Summary", value = 'home_tab',

					DT::dataTableOutput('home_table')
					
					),

				tabPanel("Travel Time", value = 'travel_time_tab',

					fluidRow(

						column(5, offset=1,

							selectInput("blue_corridor", h5("Corridors"),
								choices = blue_corrNames,
								selected = blue_corrNames[1]

								)

							),

						column(6, 

							dateRangeInput("blue_dates", h5("Choose a date range"),
								start="2016-04-08",
								end = "2016-04-20"

								),

							dateRangeInput("blue_dates2", h5("Choose a second date range"),
								start = "2016-04-08",
								end = "2016-04-20"

								)

							)

						),

					fluidRow(

						plotOutput("blue_plot1"),

						tags$br(),
						tags$br(),
						tags$br(),

						plotOutput("blue_plot2"),

						tags$br(),
						tags$br(),
						tags$br(),

						h2("Segment Information Table"),
						DT::dataTableOutput("segment_info"),
						
						tags$br(),
						tags$br(),
						tags$br(),
						
						h2("Segment Travel Time Table for Period 1"),
						DT::dataTableOutput("segment_travel_time_1"),

						tags$br(),
						tags$br(),
						tags$br(),
						
						h2("Segment Travel Time Table for Period 2"),
						DT::dataTableOutput("segment_travel_time_2")
						)

					),

				tabPanel("Traffic Volume", value = 'traffic_volume_tab',

					tags$br(),

					fluidRow(

						column(2, offset=1,

							radioButtons("vol_source",

								h5("Data source"),

								choices = list("Traffic Studies" = 'traffic_studies',
									"Sensor" = 'sensor'),
								selected = 'traffic_studies'

								)

							),

						column(9,

							conditionalPanel(condition="input.vol_source == 'traffic_studies'",

								column(3,

									checkboxGroupInput("vol_counts_filter",

									h5("Count Type"),

									choices = list("Tube" = 0,
										"Turn Movement" = 1),
									selected = c(0, 1)
									)

									),

								column(3,

									checkboxGroupInput("vol_mod_filter",

									h5("Modality Type"),

									choices = list("Auto" = 'autos',
										"Heavy" = 'heavy',
										"Pedestrian" = 'peds',
										"Bike" = 'bike'),
									selected = c("autos", "heavy")

									)

									),

								column(3,

									checkboxGroupInput("vol_peaks_filter",

									h5("Time of Day"),

									choices = list("AM peak" = 'am_peak',
										"PM peak" = 'pm_peak',
										"Off peak" = 'off_peak'),
									selected = c("am_peak", "pm_peak", "off_peak")

									)

									)

								),

							conditionalPanel(condition="input.vol_source == 'sensor'",

								column(6,

									selectInput("radar_vol_intersection", h5("Intersections"),
										choices = radar_intersection_names,
										selected=radar_intersection_names[[1]])

									),

								column(4, 

									dateRangeInput("radar_vol_p1", h5("Choose Period 1"), 
										min=min(radar_dates),
										max = max(radar_dates),
										start=min(radar_dates),
										end="2017-12-31"

										),

									dateRangeInput("radar_vol_p2", h5("Choose Period 2"), 
										min=min(radar_dates),
										max = max(radar_dates),
										start = "2018-01-01",
										end = max(radar_dates)

										)

									),

								column(2,

									uiOutput("radar_vol_direction")

									)

								)

							)

						),

					fluidRow(

						conditionalPanel(

							condition = "input.vol_source == 'traffic_studies'",

							tags$br(),

							DT::dataTableOutput('intersect_table')

							),

						conditionalPanel(

							condition = "input.vol_source == 'sensor'",

							tags$br(),

							plotlyOutput("radar_vol_plot")

							# DT::dataTableOutput("radar_vol_table")


							)
						)

					),


				tabPanel("Speed", value = 'speed_tab',

					fluidRow(

						column(4,

							selectInput("radar_vel_intersection", h5("Intersections"),

								choices = radar_intersection_names,
								selected= radar_intersection_names[[1]])
							),

						column(4,

							dateRangeInput("radar_vel_p1", h5("Choose Period 1"), 
								min=min(radar_dates),
								max = max(radar_dates),
								start=min(radar_dates),
								end="2017-12-31"

								),

							dateRangeInput("radar_vel_p2", h5("Choose Period 2"), 
								min=min(radar_dates),
								max = max(radar_dates),
								start = "2018-01-01",
								end = max(radar_dates)

								)
							),

						column(2,

							uiOutput("radar_vel_direction")

							)


						),

					fluidRow(

						plotlyOutput("radar_vel_plot")
						# DT::dataTableOutput("radar_vel_table")


						)

					),

				tabPanel("Transit", value = 'transit_tab',

					fluidRow(

						column(2, offset = 1,

							radioButtons("transit_analysis",

								h5("Analysis Type"),

								choices = list("Dwell Time" = 'dwell_time',
									"Occupancy" = "occupancy",
									"Transit Speed" = 'transit_speed',
									"Boardings and Alightings" = 'on_off'),
								selected = 'on_off'

								)

							),

						column(5,

							selectInput("apc_chosen_corr", h5("Choose Corridor"), 
								choices= apc_corr, selected = apc_corr[[1]])


							)

						# column(4, 

						# 	h5("Date filter place holder")

						# 	)

						),

					tags$br(),
					tags$hr(),
					tags$br(),

					fluidRow(

						conditionalPanel(condition="input.transit_analysis == 'dwell_time'",

							plotOutput("apc_dwell_plot")

							),

						conditionalPanel(condition="input.transit_analysis == 'occupancy'",

							h3("Occupancy Analysis Place Holder")

							),

						conditionalPanel(condition="input.transit_analysis == 'transit_speed'",

							h3("Transit Speed Analysis Place Holder")

							),
						conditionalPanel(condition="input.transit_analysis == 'on_off'",

							plotOutput("apc_plot")

							)

						)

					)

				)
			),

		column(6,

			leafletOutput("map", height=400),

			tags$br(),
			tags$hr(),
			tags$br(),

			conditionalPanel( condition = "input.tabs == 'traffic_volume_tab' && input.vol_source == 'traffic_studies'",

				h4("Select a table row, then click to download"),

				tags$br(),

				downloadButton("download_study", "Download Counts"),

				DT::dataTableOutput("data_access_table")

				# textOutput("int_filename_class")


				)

			)

		)
	)