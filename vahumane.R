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

setwd("/Users/samantha/Desktop/Lamar Research/vahumane")
fac <- read_excel("vahumane_facilities.xlsx")
stat <- read_excel("vahumane_stats.xlsx")
fac_city <- select(fac, one_of(c("FACILITY_NAME", "CITY")))
fac_zip <- select(fac, one_of(c("FACILITY_NAME", "ZIPCODE")))
data("zipcode")
va_zipcode <- subset(zipcode, state=='VA')
va_zipcode2 <- select(va_zipcode, one_of(c("zip", "latitude", "longitude")))
latlong <- left_join(fac_zip, va_zipcode2, by=c("ZIPCODE"="zip"))

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Virginia Humane Societies"),
   
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
#)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
   #points <- eventReactive({input$
      
      # draw the histogram with the specified number of bins
      output$mymap <- renderLeaflet({
        leaflet(latlong) %>% 
          setView(lng=-78, lat=37.3, zoom = 6.5) %>%
          addTiles() %>%
          addMarkers(data = latlong, lat = ~latitude, lng = ~longitude,
                     popup = ~as.character(FACILITY_NAME))
      })
}

# Run the application 
shinyApp(ui, server)

