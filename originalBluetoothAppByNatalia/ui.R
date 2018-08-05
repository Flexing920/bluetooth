library(leaflet)
library(shiny)
library(shinyBS)
#library(shinydashboard)


#Parameters (Not sure how these are different from values in Server.R. -JWH)
#tt_threshold = 600 #maximum allowable ttime between sensors
# agg=1800 #aggregation for analysis (in seconds)

######UI
shinyUI(navbarPage(
  "City of Austin Bluetooth Data",
  id = "nav",
  #The first tab panel, contains a map of Austin with selectable bluetooth sensors and date selection
  tabPanel(
    "Interactive map",
    value = "nav1",
    div(
      class = "outer",
      
      tags$head(
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      #The background of the first panel is a map of Austin.
      leafletOutput("mapAustin", width = "100%", height =
                      "130%"),
      
      #This side panel allows the user to select dates for consideration.
      absolutePanel(
        id = "controls",
       # class = "panel panel-default",
       # fixed = TRUE,
        draggable = FALSE,
        fixed=FALSE,
        top = 60,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = 400,
        height = "auto",
        tags$head(includeCSS("styles.css")),
        h2("Data Selection"),
        h3("1 - Select Sensors"),
        p(
          class = "text-muted",
          HTML("Select <b>2 sensors</b> for travel time estimation by clicking on the interactive map. The order in which sensors are selected defines travel <b>Direction 1</b>. If more than two sensors are selected, travel times between the <b>first and last </b>will be provided")
        ),
        bsButton("reset", "Reset Selected",disabled=TRUE, style="primary"),
      
        h3("2 - Select Time Period"),
        p(
          class = "text-muted",
          "Select or edit analysis period and retrieve data. Must select sensors before. Button turns green when retrieved data matches selected period(s)."
        ),
        
        h4("Period 1"),
        dateRangeInput(
          'period1',
          label = NULL,
          start = startdate,
          # The end date is hard coded right now to make testing easier.
          end = startdate +
            2,
          max = enddate,
          min = startdate,
          separator =
            "-",
          format = "mm/dd/yyyy"
        ),

        checkboxInput("period2check", "Select time period for comparison (optional)."),

        conditionalPanel(
          condition = "input.period2check==true",
          h4("Period 2"),
          dateRangeInput(
            'period2',
            label = NULL,
            start = startdate,
            end = enddate,
            max = enddate,
            min = startdate,
            separator =
              "-",
            format =
              "mm/dd/yyyy"
          )
        ),
        bsButton("submit", "Retrieve Data",style="primary",disabled=TRUE),
        uiOutput("date_warning"),
        uiOutput("nodata_warning"),
        #checkboxInput("subset", "Retrieve Only Selected?", value =
        #                TRUE),
        h3("3 - Analysis"),
        p(
          class = "text-muted",
          "Clean data and compute travel times. Must select sensors and time period. Button turns green when travel times are computed for the selected time period(s)."
        ),
        bsButton("calculate", "Calculate Travel Times",disabled=TRUE, style="primary"),
        uiOutput("period_warning"),
        uiOutput("nomatches_warning"),
        uiOutput("name_problem")
       
       
        #  h3("Upload Data"),
        #  p(class="text-muted",
        #    "Upload your data for analysis. After data is uploaded, only sensors in your dataset will be displayed."
        #  ),
        # checkboxInput("uploadDataCheck", "Select to upload data."),
        # conditionalPanel(
        #   condition="input.uploadDataCheck==true",
        #   fileInput("uploadFile", "Choose CSV File",
        #             accept=c("text/csv",
        #                      'text/comma-separated-values,text/plain', 
        #                      '.csv')),
        #   checkboxInput('header', 'Header', TRUE),
        #   radioButtons('sep', 'Separator',
        #                c(Comma=',',
        #                  Semicolon=';',
        #                  Tab='\t'),
        #                ','),
        #   radioButtons('quote', 'Quote',
        #                c(None='',
        #                  'Double Quote'='"',
        #                  'Single Quote'="'"),
        #                '"')
        # )
          
        
                 
        )
    )  
        
  ),
  
  
  # The 2nd panel allows the user to verify that they have selected the relevant sensors by displaying
  # the list of selected sensors. It also allows visualiztion and downloading of data sets
  tabPanel(
    "Data & Downloads",
    value = "nav2",
    
    fluidPage(
      #class = "outer",
      tags$head(
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      h3("Analysis Period(s)"),
      p(class="text-muted",
        "Analyzed periods. Updated after calculating travel times and/or reseting selected sensors."
      ),
      
      uiOutput(outputId="input_period1_text"),
      uiOutput(outputId="input_period2_text"),
      uiOutput(outputId="period_warning_2"),
      
     # p(class="muted-text",paste("Period 1= ",input$period1),
      h3("Selected Sensors"),
      p(class="text-muted",
        "List of selected sensors and corresponding sequence. Available only when sensors are selected. If more than 2 sensors are selected, data is retrieved only for travel times between the first and last one."
      ),
  
      DT::dataTableOutput("dtable"),
      downloadButton('dl_sensors', 'Download'),
      
      h3("Dissagregate Data"),
  

      p(class="text-muted",
        "All device matches between first and last sensor. Field 'Valid' is used to identify data points eliminated in the cleaning process. Available only when data has been retrieved and travel times calculated."
        
      ),
      
      DT::dataTableOutput("raw_dtable"),
      downloadButton('dl_raw', 'Download'),
      
      h3("Average Travel Times First-to-Last"),
    # verbatimTextOutput("input_period1_text"),
    # verbatimTextOutput("input_period2_text"),
      p(class="text-muted",
        "Average travel time by time of day betwen first and last sensor. Available only after computing travel times"
      ),
      DT::dataTableOutput("agg_dtable"),
      downloadButton('dl_agg', 'Download')
     )
  ),
  
  
  # The 3rd panel displays plots for the user. The app will automatically move the user from nav1 to nav3 when "Calculate Travel Times" is clicked.
  tabPanel(
    "Average Travel Time",
    value = "nav3",
    fluidPage(
    #  class = "outer",
      tags$head(
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      
      mainPanel(
        
       
         h3("Average Travel Time Analysis"),
         #If one of the periods does not have weekdays/weekends not all plots are available
        h4("Travel Times"),
        plotOutput("graph1_1"),
        h4("Number of Valid Data Points"),
        plotOutput("graph1_2")

        ),
      
      # This sidebar allows the user to alter parameters for the graphs and redraw them without needing to download new data from the server
      # (date range and selected sensors must remain the same).
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
      
        draggable = FALSE,
        top = 80,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = 380,
        height = "auto",
        fixed=FALSE,
        tags$head(includeCSS("styles.css")),
        #width = 2,
        h2("Analysis Period(s)"),
        p(class="text-muted",
          "Analyzed periods. Updated after calculating travel times and/or reseting selected sensors."
        ),
        
        uiOutput(outputId="period1_plot"),
        uiOutput(outputId="period2_plot"),
        h2("Customize Graph"),
        h4(HTML("<b>Day Type</b>")),
        uiOutput("weekday_control"),
        uiOutput(outputId="period_warning_3"),
        
        h4(HTML("<b>Visualize Variability</b>")),
        p(
          class="text-muted",
          HTML("High variability may suggest the need to refine the cleaning parameters (time step and/or travel time threshold). Analyze also individual data points (in next tab).")
        ),
        checkboxInput("ribbondisplay", "Show 2 Standard Deviations",value=FALSE),
        h4(HTML("<b>Highlight Specific Date</b>")),
        p(
          class="text-muted",
          HTML(" Visualize all data points from a single day. All points collected during the same time step are displayed at the beginning of the time step. The daily average is likley to be near the highest concentration of points, which may be above or below the average for the corresponding period. Points are shown <b>only for the corresponding day type </b>.")
        ),
        dateInput(
          "comparisondate",
          label = NULL,
          value = startdate,
          min = startdate,
          max = enddate,
          format = "mm/dd/yyyy"
        ),
        checkboxInput("hl_hide", "Hide Day Data",value=FALSE),
        uiOutput("hl_warning")
     
      
       
      )
    )
      
  ),
  
  tabPanel(
    "Travel Time Points",
    value = "nav4",
    fluidPage(
      #  class = "outer",
      tags$head(
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      
      mainPanel(
        
        
        h3("Individual Travel Times"),
       
        h4("Period 1"),
        plotOutput("graph2"),
        conditionalPanel(condition="input.period2check==true",
          h4("Period 2"),
          plotOutput("graph3")
        )
        
        
        
      ),
      
      # This sidebar allows the user to alter parameters for the graphs and redraw them without needing to download new data from the server
      # (date range and selected sensors must remain the same).
      absolutePanel(
        id = "controls",
        class = "panel panel-default",
        fixed = FALSE,
        draggable = FALSE,
        top = 80,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = "35%",
        height = "auto",
        tags$head(includeCSS("styles.css")),
        
        h2("Customize Data Cleaning"),
        p(
          class="text-muted",
          HTML("Visualize all data points and the impact of the data cleaning methodology and adjust as needed. If <b>average travel times do not track observations </b> consider moving to a smaller time step in Tab 'Average Travel Times'. If <b> valid travel times are excessively high for the anlyzed segment</b>, consider lowering the travel time threshold or reducing the cleaning factor.")
        ),
         h4(HTML("<b>Day Type</b>")),
        uiOutput(outputId="period_warning_4"),
       
        uiOutput("weekday_control_2"),
        h4(HTML("<b>Travel Time Threshold</b>")),
        
        
        fluidRow(
          column(3,
                 numericInput("slider1", label = "Direction 1 (minutes).", min = 1, max = 60, value = 10,step=1)
                 ),
          column(4,
                 numericInput("slider2",label="Direction -1 (minutes).",min=1,max=60, value=10,step=1)
                 )
        ),
        
        p(
          class="text-muted",
          HTML("Data points with higher travel times than the threshold will be <b>discarded</b> before cleaning the data and are <b> NOT </b> reported in the plots. Set this value based on expected maximum travel times during </b>congested periods</b>. High threshold values may lead to unrealistically high travel time estimations by including data points that do not represent moving passenger cars. If the number of shown points is low compared to avilable data, the selected threshold value is likely to be too low.")
        ),
      
      
        
        h4(HTML("<b>Cleaning Factor</b>")),

        selectInput("select_factor", label = NULL, 
                    choices = list("2" = 2, "3" = 3, "4" = 4,"5"=5), selected=5
        ),
        p(
          class="text-muted",
          HTML("Higher values allow for a wider spread of valid travel time values. <b>The use of 5 is recommended</b> unless travel times for alternative data sources can be used to find an optimal value. Lower values will produce more <b>Invalid</b> points.")
        ),
       
      
        h4(HTML("<b>Time Step</b>")),
        p(
          class="text-muted",
          HTML("Smaller <b> time steps </b> provide a more detailed representation of traffic conditions and may improve data cleaning. If there are <b>less than 5-10 data points</b> per time interval consider a longer time period or a larger time step. If too many points are <b> not valid </b> consider adjusting the travel time threshold. Push <b>Re-Calculate</b> to visualize the effect of changes. The button is green when the time step used for travel time calculation matches the selected time step.")
        ),
        selectInput(
          "aggregation",
          NULL,
          c(
            "15 mins" = "900",
            "30 mins" = "1800",
            "1 hour" = "3600",
            "3 hours" = "10800",
            "6 hours" = "21600",
            "12 hours" = "43200"
          ),
          selected = "3600"
        ),
        
        bsButton("calculate2", "Re-Calculate",style="success")
        
        
      )
      
    )
    
  ),

  #This 4th panel contains the prototype UI for Corridor Analysis
  tabPanel(
    "Corridor Analysis Preview",
    value = "nav5",
    div(
      class = "outer",
      tags$head(
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      #The background of the first panel is a map of Austin.
      leafletOutput("mapAustin2", width = "100%", height =
                      "100%"),
      
      #This side panel allows the user to select dates for consideration.
      absolutePanel(
        id = "dates2",
        class = "panel panel-default",
        fixed = TRUE,
        draggable = FALSE,
        top = 80,
        left = "auto",
        right = 20,
        bottom = "auto",
        width = 330,
        height = "auto",
        tags$head(includeCSS("styles.css")),
        h2("Corridor Analysis"),
        p(
          class="text-muted",
          HTML("This is a prototype interface to suport expedited anlaysis of corridor travel time. Analysis capabilities will be incorporated soon.")
        ),
        
        
        selectInput(
          "corridor",
          "Which corridor do you wish to examine?",
          corridor_names,
          selected = "congress"
        ),
        
        selectInput(
          "year",
          "Which year do you wish to examine?",
          c("2016" = "sixteen",
            "2015" = "fifteen"),
          selected = "sixteen"
        ),
        
        selectInput(
          "month",
          "Which month do you wish to examine?",
          c(
            "January" = "jan",
            "February" = "feb",
            "March" = "mar",
            "April" = "apr",
            "May" = "may",
            "June" = "jun",
            "July" = "jul",
            "August" = "aug",
            "September" = "sep",
            "October" = "oct",
            "November" = "nov",
            "December" = "dec"
          ),
          selected = "jan"
        ),
        
        
        selectInput(
          "dayofweek",
          "Which day do you wish to examine?",
          c(
            "Monday" = "mon",
            "Tuesday" = "tue",
            "Wednesday" = "wed",
            "Thursday" = "thur",
            "Friday" = "fri",
            "Weekdays" = "wkday2",
            "Weekends" = "wkend2"
          ),
          selected = "wkday2"
        )
        
      #  actionButton("calculate", "Calculate Travel Times"),
      #  actionButton("reset", "Reset Selected")
        
      )
    )
  ),
  
  tabPanel(
    "About",
    value = "nav5",
    fluidPage(
      class = "inner",
      
      tags$head(
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
   
        top=200,
      p(
        class="text-muted",
        HTML("This application provides access to approximately 2 months of data collected from bluetooth-enabled devices by sensors placed throughout the City of Austin.
              </p>Application users may analyze travel times between any given pair of sensors, visualize all available data points, and compare average travel times by time of day across two time periods.
              </p>Users may also customize the parameters of the methodology used to 'clean' the raw data in order to avoid biasing the travel time estimates by considering travel times provided by devices that are not in moving personal vehicles.
              </p>More information about the sensor characteristics and analysis methodologies can be requested from CTR researchers.
              </p>Please let us know any issues accessing and using the app.<b> More data will be shared through the Data Rodeo in the Spring of 2017</b>, along with new visualizations and interactive tools.")
      ),
      p(
        class="text",
        HTML("<b>Contact Information</b><p/b>Natalia Ruiz Juri:</b> nruizjuri@mail.utexas.edu
            <p/b>John Helsel:</b> john.helsel@utexas.edu <p/b>Itamar Gal:</b> igal@utexas.edu ")
        )
      
     
      
    )
  )#Close tab panel "About"
      
  
    

)#CLOSE sHUNY ui
)#cLOSE nAVbARpAGE
