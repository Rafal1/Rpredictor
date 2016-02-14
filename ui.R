library(shiny)

shinyUI(fluidPage(
  titlePanel("Rpredictor"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Wybierz symbol instrumentu finansowego:"),
    
      textInput("symb", "Symbol", "SPY"),
    
      dateRangeInput("dates", 
        "Date range",
        start = "2013-01-01", 
        end = as.character(Sys.Date())),
      
      br(),
      uiOutput('resetableFileInput'),
      actionButton("resetcsv", "Resetuj csv", icon = NULL, width = NULL),
      checkboxInput("rsi", label = "RSI", value = TRUE),
      fluidRow(column(4,checkboxInput("sma", label = "Średnia", value = TRUE)),
      column(8,numericInput("smasize", label="", value = 20))),
      checkboxInput("log", "Narysuj oś Y w skali logarytmicznej.", 
        value = FALSE)
      
    ),
    
    mainPanel(
      tabsetPanel(
                  tabPanel("Analiza", plotOutput("plot")),
                  tabPanel("Predykcja",
                           numericInput("forecastsize", label="Liczba okresów predykcji", value = 80),
                           plotOutput("forecast"))
                 
                )
              )

  )
))