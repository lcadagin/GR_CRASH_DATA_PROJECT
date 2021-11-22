#Load Packages

library(shiny)
library(tidyverse)
library(sf)
library(osmdata)
library(ggpubr)
library(shinythemes)

#Load Grand Rapids Crash Data Set

crash_data <- read_csv(here::here("data", "CGR_Crash_Data.csv"))

#Configure features necessary for Grand Rapids map (using openstreetmap api)

location_gr <- getbb("Grand Rapids") %>% 
    opq()

major_roads_gr <- location_gr %>%
    add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary", "tertiary")) %>% 
    osmdata_sf()

#minor_roads_gr <- location_gr %>%
    #add_osm_feature(key = "highway", value = c("unclassified", "residential")) %>%
    #osmdata_sf()

water_gr <- location_gr %>% 
    add_osm_feature(key = "waterway", value = c("river")) %>% 
    osmdata_sf()

boundary_gr <- location_gr %>% 
    add_osm_feature(key = "boundary", value = "administrative") %>%
    add_osm_feature(key = "name", value = "Grand Rapids") %>% 
    osmdata_sf()

#Function to simplify reactive filtering of data

apply_filter <- function(variable, i) {
    if(i == "Yes" | i == "No") {
        return(variable == i)
    }
    else {return(TRUE)}
}

#Define User Interface
ui <- fluidPage(
    
    #Apply Flatly Shiny Theme to App
    theme = shinytheme("flatly"),

    #Application title
    titlePanel("Grand Rapids Accident Data 2008 - 2017"),

    #Sidebar Layout Selected
    sidebarLayout(
        
        sidebarPanel(
            
            #Filter on Date Range with calendars
            dateRangeInput("date",
                           label = "Select a Date Range: ",
                           min = "2008-01-01",
                           max = "2017-12-31",
                           start = "2017-01-01",
                           end = "2017-12-31"
                           ),
            
            #Select Hit and Run, Not Hit and Run, or No Filter
            radioButtons("hitrun", 
                         label = "Filter on Hit and Run Data: ",
                         choices = c(
                             "Hit and Run" = "Yes",
                             "Not Hit and Run" = "No",
                             "No Filter" = "N/A"),
                         selected = "N/A"
                         ),
            
            #Select Involving Train, Not Involving Train, or No Filter
            radioButtons("train", 
                         label = "Filter on Crashes Involving a train: ",
                         choices = c(
                             "Involving Train" = "Yes",
                             "Not Involving Train" = "No",
                             "No Filter" = "N/A"),
                         selected = "N/A"
                        ),
            
            #Select Involving School Bus, Not Involving School Bus, or No Filter
            radioButtons("schoolbus", 
                         label = "Filter on Crashes Involving a School Bus: ",
                         choices = c(
                             "Involving School Bus" = "Yes",
                             "Not Involving School Bus" = "No",
                             "No Filter" = "N/A"),
                         selected = "N/A"
                        ),
            
            #Range of hour of crash using slidebar (only hour is record, not minutes)
            sliderInput("hour",
                        label = "Filter by Hour of the Day: ",
                        min = 0,
                        max = 24,
                        value = c(0,24)
                        ),
            
            #Dropdown selection to filter on type of weather
            selectInput("weather",
                        label = "Select Type of Weather: ",
                        choices = distinct(crash_data, WEATHER),
                        selected = c("Clear", "Cloudy", "Unknown", "Rain", "Snow", "Fog", "Sleet or Hail", "Uncoded & Errors", "Severe Crosswind", "Blowing Snow", "Smoke"),
                        multiple = TRUE)
            
                    ),
        
        mainPanel(
            
           #Location of Grand Rapids map output
           plotOutput("gr_map", height = 700, width = 700),#, hover = "plot_hover"),
           
           #Placing Longitude and Latitude adjustments below GR map
           fluidRow(
               
               column(6,
                      sliderInput("longitude",
                                  label = "Longitude Range: ",
                                  min = -85.75,
                                  max = -85.57,
                                  value = c(-85.75,-85.57),
                                  step = .01
                      )
               ),
               
               column(6,
                      sliderInput("latitude",
                                  label = "Latitude Range: ",
                                  min = 42.88,
                                  max = 43.03,
                                  value = c(42.88,43.03),
                                  step = .01
                        )
               )
                    
           )
           
           #tableOutput("data")
        )
    )
)

# Define Server
server <- function(input, output) {
    
    #Reactive filtering of GR crash dataset based on user input
    crash_data_filtered <- reactive(crash_data %>% 
                                       filter(CRASHDATE >= input$date[1],
                                              CRASHDATE <= input$date[2],
                                              apply_filter(HITANDRUN, input$hitrun),
                                              apply_filter(TRAIN, input$train),
                                              apply_filter(SCHOOLBUS, input$schoolbus),
                                              HOUR >= input$hour[1],
                                              HOUR <= input$hour[2],
                                              WEATHER %in% input$weather
                                              )
    )
    
    #plotting crash data on map of Grand Rapids
    output$gr_map <- renderPlot({
        ggplot()+
            geom_sf(data = major_roads_gr$osm_lines, size = .6, alpha = .6, color = 'black') +
            #geom_sf(data = minor_roads_gr$osm_lines, size = .3, alpha = .3, color = 'black') +
            geom_sf(data = water_gr$osm_lines, size = 1, alpha = .4, color = 'steelblue') +
            geom_sf(data = boundary_gr$osm_lines, size = 1, alpha = .6, color = "orange") +
            geom_point(data = crash_data_filtered(), mapping = aes(x = X, y = Y), color = "blue") +
            coord_sf(xlim = c(input$longitude[2], input$longitude[1]), ylim = c(input$latitude[1], input$latitude[2])) +
            labs(title = "Grand Rapids City Limits", x = "Longitude", y = "Latitude") +
            font("title", size = 20, color = "blue", face = "bold") +
            font("x", size = 16) +
            font("y", size = 16)
    })
    
    #output$data <- renderTable({
        #nearPoints(crash_data_filtered() %>% select(X, Y, CRASHDATE, HITANDRUN), input$plot_hover, xvar = "X", yvar = "Y")
    #})

}

# Run the application 
shinyApp(ui = ui, server = server)
