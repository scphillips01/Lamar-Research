#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)          
library(tidyr)
library(dplyr)
library(leaflet)
library(zipcode)
library(crosstalk)

setwd("/Users/samantha/Desktop/Lamar Research/vahumane")
fac <- read_excel("vahumane_facilities.xlsx")
stat <- read_excel("vahumane_stats.xlsx")
fac_names <- select(fac, one_of(c("SYSFACNO", "FACILITY_NAME")))
fac_stat_orig <- full_join(stat, fac_names, by = c("SYSFACNO", "SYSFACNO"))
fac_stat <- select(fac_stat_orig, c("FACILITY_NAME", "CALENDAR_YEAR", "ON_HAND_JAN1", 
                                    "SURRENDERED", "RECLAIMED", "ADOPTED", "DIED", "EUTHANIZED"))
final_fac_stat <- full_join(fac_stat, latlong, by=c("FACILITY_NAME"))
fac_city <- select(fac, one_of(c("FACILITY_NAME", "CITY")))
fac_zip <- select(fac, one_of(c("FACILITY_NAME", "ZIPCODE")))
data("zipcode")
va_zipcode <- subset(zipcode, state=='VA')
va_zipcode2 <- select(va_zipcode, one_of(c("zip", "latitude", "longitude")))
latlong <- left_join(fac_zip, va_zipcode2, by=c("ZIPCODE"="zip"))
shared_fac_stat <- SharedData$new(final_fac_stat)

#add surrenders, transfers, euthanizations
#notes?
#visualize euthanize
#r_colors <- rgb(t(col2rgb(colors()) / 255))
#names(r_colors) <- colors()

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Virginia Humane Societies"),
   fluidPage(
     fluidRow(
       column(4,
              
              # Copy the line below to make a slider range 
              sliderInput("slider2", label = h3("Slider Range"), min = 2013, 
                          max = 2018, step = 1, value = c(2015, 2016))
       )
     ),
     
     hr(),
     
     
     fluidRow(
       column(4,
              selectInput("FACILITY_NAME",
                          "Name of Facility:",
                          c("All",
                            unique(as.character(fac_stat$FACILITY_NAME))))
       ),
       column(4,
              selectInput("CALENDAR_YEAR",
                          "Year:",
                          c("All",
                            unique(as.character(fac_stat$CALENDAR_YEAR))))
       )
     ),
     # Create a new row for the table.
     DT::dataTableOutput("table"),
   
   # Sidebar with a slider input for number of bins 
   #sidebarLayout(
      #sidebarPanel(
         #sliderInput("bins",
                     #"Number of bins:",
                     #min = 1,
                     #max = 50,
                     #value = 30)
      #),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("mymap", height = 600),
         p()
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   #points <- eventReactive({input$
    
      # draw the histogram with the specified number of bins
      output$mymap <- renderLeaflet({
        leaflet(latlong) %>% 
          setView(lng=-78, lat=37.3, zoom = 6.3) %>%
          addTiles() %>%
          addCircleMarkers(data = latlong, lat = ~latitude, lng = ~longitude,
                     popup = ~as.character(FACILITY_NAME))
      })
      output$table <- DT::renderDataTable(DT::datatable({
        datatable(shared_fac_stat) 
        #data <- select(shared_fac_stat, -c("ZIPCODE", "latitude", "longitude"))
        if (input$FACILITY_NAME != "All") {
          data <- data[data$FACILITY_NAME == input$FACILITY_NAME,]
        }
        if (input$CALENDAR_YEAR != "All") {
          data <- data[data$CALENDAR_YEAR == input$CALENDAR_YEAR,]
        }
        data
      }, server = FALSE))
}

# Run the application 
shinyApp(ui, server)

