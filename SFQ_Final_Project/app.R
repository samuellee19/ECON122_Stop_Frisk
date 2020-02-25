#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(plotly)
library(tidyverse)
library(sp)
library(sf)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(mapdeck)
library(rgdal)

# download SQF1718 dataset and clean data (load if the file exists)
# possibly insert date and time settings for parsing
#SQF1718 <- read_csv("https://github.com/samuellee19/econ122_sqf/raw/master/Data%20Files%20Only/SQF1718.csv")
#SQF1718 <- SQF1718[!is.na(SQF1718$STOP_LOCATION_X),]
#SQF1718 <- SQF1718[!is.na(SQF1718$STOP_LOCATION_Y),] %>%
#    select(STOP_FRISK_ID, STOP_FRISK_DATE, STOP_FRISK_TIME, YEAR2, MONTH2, DAY2, STOP_WAS_INITIATED, SUSPECTED_CRIME_DESCRIPTION, SUSPECT_ARREST_OFFENSE, SUSPECT_RACE_DESCRIPTION, STOP_LOCATION_FULL_ADDRESS, STOP_LOCATION_X, STOP_LOCATION_Y)
load(url("https://www.dropbox.com/s/mv6mgp9quu92fia/plot_locations_1718.RData?dl=1"))
load(url("https://www.dropbox.com/s/pf5fedyiflf8x34/plot_locations_1718SF.RData?dl=1"))

# 2013 - 2016 Data
load(url("https://www.dropbox.com/s/srdebs76epur2rx/plot_locations_1316.RData?dl=1"))
load(url("https://www.dropbox.com/s/u6us5n6jsp41sa5/plot_locations_1316SF.RData?dl=1"))

# 2008 - 2012 Data
#if(!file.exists("SQF0812_a.RData", "SQF0812_b.RData")) {
#    load(url("https://github.com/samuellee19/econ122_sqf/raw/master/Data%20Files%20Only/SQF0812_a.RData"))
#    load(url("https://github.com/samuellee19/econ122_sqf/raw/master/Data%20Files%20Only/SQF0812_b.RData"))
#}else{
#    load("SQF0812_a.RData")
#    load("SQF0812_b.RData")
#}
# Example Coding for Data Munging (Simplified processes due to unresponsiveness on Shinyapps)
#SQF0812 <- rbind(as.data.frame(SQF0810), as.data.frame(SQF0812_b))
#SQF0812 <- SQF0812[!is.na(SQF0812$xcoord),]
#SQF0812 <- SQF0812[!is.na(SQF0812$ycoord),]
#SQF0812$xcoord <- as.numeric(SQF0812$xcoord)
#SQF0812$ycoord <- as.numeric(SQF0812$ycoord)

# Data Transformation Part (sp object)
#plot_locations_0812 <- SQF0812
#coordinates(plot_locations_0812) <- ~xcoord + ycoord
#proj4string(plot_locations_0812) <- CRS('+init=epsg:2263 +proj=lcc +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0')
#plot_locations_0812 <- plot_locations_0812 %>% spTransform(CRS("+init=epsg:4326"))
#plot_locations_0812 <- st_as_sf(plot_locations_0812, coords = c("xcoord", "ycoord"), crs = utm18nCRS)

#SQF0812SF <- SQF0812 %>%
#    filter(crimsusp %in% c("GRAND LARCENY", "ROBBERY", "BURGLARY", "ASSAULT", "RAPE", "TERRORISM", "GRAND LARCENY AUTO", "AUTO STRIPPING", "MURDER"))

#plot_locations_0812SF <- SQF0812SF
#coordinates(plot_locations_0812SF) <- ~xcoord + ycoord
#proj4string(plot_locations_0812SF) <- CRS('+init=epsg:2263 +proj=lcc +lat_1=41.03333333333333 +lat_2=40.66666666666666 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000.0000000001 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0')
#plot_locations_0812SF <- plot_locations_0812SF %>% spTransform(CRS("+init=epsg:4326"))
#plot_locations_0812SF <- st_as_sf(plot_locations_0812SF, coords = c("xcoord", "ycoord"), crs = utm18nCRS)
load(url("https://www.dropbox.com/s/4ewfxpzqk0pta9k/plot_locations_0812.RData?dl=1"))
load(url("https://www.dropbox.com/s/so7tsoi4bz39k8d/plot_locations_0812SF.RData?dl=1"))

# 2004 - 2007 Data
# 2003 SQF not loaded since it is missing x and y coordinates
load(url("https://www.dropbox.com/s/5chpznaostaoxev/plot_locations_0407.RData?dl=1"))
load(url("https://www.dropbox.com/s/zi0wb0fqwu2bwuj/plot_locations_0407SF.RData?dl=1"))

# App Functions (ui, server)
Range <- sort(c("2004-2007", "2008-2012", "2013-2016", "2017-2018"))
              
# Mapbox Token
# FYI: this is free Mapbox account with limited free tile generation
#      Please use different token / API key if possible.

Sys.setenv("MAPBOX_TOKEN" = "pk.eyJ1Ijoic2xlZTE5IiwiYSI6ImNrNDZmbWl1aTBqcmgzZW8xeDJzcTBsMXEifQ.OvJvDwdVBfB02NaJVLq7Fw")

# Define UI for application that draws a histogram
ui <- dashboardPage(

    # Application title
    dashboardHeader(title = "SQF Dashboard"),

    # Sidebar disabled
    dashboarinclude_app(dSidebar(),
    
    # Dashboard Body 
    dashboardBody(
        useShinyjs(),
        fluidRow(
            column(width = 9,
                   box(width = NULL, solidHeader = TRUE,
                       mapdeckOutput(outputId = "NY_map")
                       ),
                   box(width=NULL,
                       dataTableOutput("sqf_table")
                       )
                   ),
            column(width = 3,
                   box(width = NULL,
                       selectInput("YearSelect", "Period:", Range, selected = Range[4], multiple = FALSE),
                       #radioButtons(),
                       checkboxInput("sf", "Add Seven Major Felonies Hexagons", FALSE)
                       )
                   )
            )
    )
)

# Define server logic required to draw map and tibble
# server
server <- function(input, output, session){
    addClass(selector = "body", class = "sidebar-collapse")
    
    # get a relevant dataframe from Year Range input
    get_data_plot <- reactive({
        
        # if else commands for checking which year range
        if(input$YearSelect == "2004-2007"){
            plot_locations <- plot_locations_0407
        }
        
        else if(input$YearSelect == "2008-2012"){
            plot_locations <- plot_locations_0812
        }
        
        else if(input$YearSelect == "2013-2016"){
            plot_locations <- plot_locations_1316
        }
        
        else if(input$sf == TRUE){
            if(input$YearSelect == "2004-2007"){
                plot_locations <- plot_locations_0407SF
            }
            
            else if(input$YearSelect == "2008-2012"){
                plot_locations <- plot_locations_0812SF
            }
            
            else if(input$YearSelect == "2013-2016"){
                plot_locations <- plot_locations_1316SF
            }
            
            else if(input$YearSelect == "2017-2018") {
                plot_locations <- plot_locations_1718SF
            }
        }
        
        else{
            plot_locations <- plot_locations_1718
        }
        # output relevant sp objects
        plot_locations
    })

    # get a relevant dataframe from Year Range input
    get_data_table <- reactive({
        
        # if else commands for checking which year range
        if(input$YearSelect == "2004-2007"){
            plot_locations_table <- plot_locations_0407 %>%
                select(-year, -timestop, -sex, -geometry)
        }
        
        else if(input$YearSelect == "2008-2012"){
            plot_locations_table <- plot_locations_0812 %>%
                select(-year, -timestop, -sex, -geometry)
        }
        
        else if(input$YearSelect == "2013-2016"){
            plot_locations_table <- plot_locations_1316 %>%
                select(-year, -timestop, -sex, -geometry)
        }
        
        else if(input$sf == TRUE){
            if(input$YearSelect == "2004-2007"){
                plot_locations_table <- plot_locations_0407SF %>%
                    select(-year, -timestop, -sex, -geometry)
            }
            
            else if(input$YearSelect == "2008-2012"){
                plot_locations_table <- plot_locations_0812SF %>%
                    select(-year, -timestop, -sex, -geometry)
            }
            
            else if(input$YearSelect == "2013-2016"){
                plot_locations_table <- plot_locations_1316SF %>%
                    select(-year, -timestop, -sex, -geometry)
            }
            
            else {
                plot_locations_table <- plot_locations_1718SF %>%
                    select(-STOP_FRISK_ID, -STOP_FRISK_DATE, -STOP_FRISK_TIME, -YEAR2, -MONTH2, -DAY2, STOP_WAS_INITIATED, -SUSPECT_RACE_DESCRIPTION)
            }
        }
        
        else{
            plot_locations_table <- plot_locations_1718 %>%
                select(-STOP_FRISK_ID, -STOP_FRISK_DATE, -STOP_FRISK_TIME, -YEAR2, -MONTH2, -DAY2, STOP_WAS_INITIATED, -SUSPECT_RACE_DESCRIPTION)
        }
        # output relevant table
        plot_locations_table
    })
    
    # set Mapbox Token
    # FYI: this is free Mapbox account with limited free tile generation
    #      Please use different token / API key if possible.
    
    # Initialize Mapdeck
    set_token('pk.eyJ1Ijoic2xlZTE5IiwiYSI6ImNrNDZmbWl1aTBqcmgzZW8xeDJzcTBsMXEifQ.OvJvDwdVBfB02NaJVLq7Fw')
    output$NY_map <- renderMapdeck({
        mapdeck(location = c(-73.985, 40.7588), zoom = 10, style = mapdeck_style("dark"), pitch = 45)
    })
    
    observeEvent(input$YearSelect,{
        plot_locations <- get_data_plot()
        # set text for the clickable popup labels
        
        # If data setting changes
        if (input$YearSelect == "2004-2007"){
            output$NY_map <- renderMapdeck({
                mapdeck(location = c(-73.985, 40.7588), zoom = 10, style = mapdeck_style("dark"), pitch = 45) %>%
                    add_screengrid(
                        data = plot_locations,
                        layer_id = "sg_layer",
                        cell_size = 6,
                        opacity = 0.4,
                        colour_range = colourvalues::colour_values(1:6, palette = "plasma"))
            })
        }
        else if (input$YearSelect == "2008-2012"){
            output$NY_map <- renderMapdeck({
                mapdeck(location = c(-73.985, 40.7588), zoom = 10, style = mapdeck_style("dark"), pitch = 45) %>%
                    add_screengrid(
                        data = plot_locations,
                        layer_id = "sg_layer",
                        cell_size = 6,
                        opacity = 0.4,
                        colour_range = colourvalues::colour_values(1:6, palette = "plasma"))
            })
        }
        else if (input$YearSelect == "2013-2016"){
            output$NY_map <- renderMapdeck({
                mapdeck(location = c(-73.985, 40.7588), zoom = 10, style = mapdeck_style("dark"), pitch = 45) %>%
                    add_screengrid(
                        data = plot_locations,
                        layer_id = "sg_layer",
                        cell_size = 6,
                        opacity = 0.4,
                        colour_range = colourvalues::colour_values(1:6, palette = "plasma"))
            })
        }
        else if (input$YearSelect == "2017-2018"){
            output$NY_map <- renderMapdeck({
                mapdeck(location = c(-73.985, 40.7588), zoom = 10, style = mapdeck_style("dark"), pitch = 45) %>%
                    add_screengrid(
                        data = plot_locations,
                        layer_id = "sg_layer",
                        cell_size = 6,
                        opacity = 0.4,
                        colour_range = colourvalues::colour_values(1:6, palette = "plasma"))
            })
        }
    })
    
    observeEvent(input$sf,{
        plot_locations <- get_data_plot()
        if(input$sf == TRUE){
            mapdeck_update(map_id = "NY_map") %>%
                add_hexagon(data = plot_locations,
                            layer_id = "hex_layer",
                            radius = 6
                )
        }
        else{
            mapdeck_update(map_id = "NY_map") %>%
                clear_hexagon(layer_id = "hex_layer")
        }
    })
    
    # render table
    output$sqf_table <- renderDataTable(datatable({
        Table_SQF <- get_data_table()
        Table_SQF
    },
    options = list(lengthMenu = c(5, 10, 30)))
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
