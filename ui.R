library(shiny)
library(dygraphs)

shinyUI(fluidPage(
  titlePanel("Stock Market Prediction"),
 
  sidebarLayout(
    sidebarPanel(
      helpText("Select a stock to examine.
        Information will be collected from yahoo finance."),
    
      textInput("symb","StockSymbol", "DJIA"),
      dateRangeInput("dates", 
        "Date range",
        start = "2007-01-01", 
        end = as.character(Sys.Date())),
      
      br(),
      br(),
      
      checkboxInput("log", "Plot y axis on log scale", 
         value = FALSE)
     ),
    
    mainPanel(
      tabsetPanel(
        # Tab for Summary of the Stock
        tabPanel("Summary of Stock", verbatimTextOutput("summary"),br(),br(),
                 plotOutput("plot"),br(),br()),
        
        
        # Tab for General Evaluation of the Stock
        tabPanel("Interactive Stock Grpah",dygraphOutput("interactive")),
        
        
        # Intro To Stock
        tabPanel("Data and Plots",
                 dygraphOutput("logplot"),br(),br(),
                 plotOutput("predictplot"), br(), br(), 
                 plotOutput("decomposed")),
        
        
        # Tab for Predictions
        tabPanel("Predictions", helpText("The Next 4 Days Prediction for Closing Value are as below:"),
                 tableOutput("predicttable"), br(),
                 helpText("Prediction by Arima Model"), br(), 
                 tableOutput("ArimaForcast"), br(), 
                 helpText("Arima Models Predicion Graph"), br(), 
                 plotOutput("resultArima"),
                tableOutput("ArimaForcastAuto"),
                plotOutput("resultArima2"))
      )
      )
      )
    )
  )