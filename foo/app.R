#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(DT)
library(ggvis)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Data Visualization Using a Scatterplot"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         fileInput("file1", "Choose CSV File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
        tags$hr(),
        
        checkboxInput("header", "Header", TRUE),
        
        radioButtons("sep", "Separator", choices = c(Comma = ",",
                                                     Semicolon = ";",
                                                     Tab = "\t"),
                     selected = ","),
        radioButtons("quote", "Quote", choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                    selected = '"'),
        tags$hr(),
        
        selectInput('xcol', 'X Variable', ""),
        selectInput('ycol', 'Y Variable', "", selected = "")),
        
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel(
                    titlePanel("Uploading Files"),
                      tableOutput("contents")
                    ),
                  tabPanel(
                   titlePanel("Plot"),
                     plotOutput('MyPlot')
                    ),
                  tabPanel(
                   titlePanel("Summary Statistics"),
                      verbatimTextOutput("summary")
                  )))))
             
        
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  data <- reactive({
    req(input$file1)
    inFile <- input$file1
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote)
    
    updateSelectInput(session, inputId = "xcol", label = "X Variable",
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = "ycol", label = "Y Variable",
                      choices = names(df), selected = names(df)[2])
    
    return(df)
  })
  
  output$contents <- renderTable({
    data()
    
  })
  
  
#This does not work, I keep getting the error "argument "x" is missing, with no default"
  #so I'm trying to fix that
  #output$MyPlot <- renderPlot({
    #df() %>%
      #ggvis(~xcol(), ~ycol(), fill := "deepskyblue3") %>%
      #layer_points() %>%
      #layer_model_predictions(model = "lm", se = TRUE)
  #})
  
  #This works but is very basic  
  #output$MyPlot <- renderPlot({
    #x <- data()[, c(input$xcol, input$ycol)]
    #p <- plot(x, input$xcol,
                  #input$ycol,
                  #input$graph, col = "deepskyblue3")
    #p
    
  #})
   
  output$summary <- renderPrint({
    y <- data()
    summary(y)
  })
    
  
  
}
   



# Run the application 
shinyApp(ui = ui, server = server)

