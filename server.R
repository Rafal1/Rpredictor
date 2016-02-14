# server.R

library(quantmod)
source("helpers.R")
library(forecast)

fileflag <- TRUE
clickNumber <- 0
inputSymbol <- ""
inputData1 <- Sys.Date()
inputData2 <- Sys.Date()

shinyServer(function(input, output, session) {
  output$forecast <- renderPlot({
    cs <- input$resetcsv
    if(cs != clickNumber) {
      fileflag <<- FALSE
      clickNumber <<- cs
    } else if(input$dates[1] != inputData1 | input$dates[2] != inputData2) {
      inputData1 <<- input$dates[1]
      inputData2 <<- input$dates[2]
    } else {
      fileflag <<- TRUE
    }
    inFile <- input$file1
    if (!is.null(inFile) & fileflag==TRUE & inputSymbol==input$symb) {
      readcsv <- read.csv(inFile$datapath, header=TRUE, sep=",", quote='"', as.is=TRUE)
      zooobj <- read.zoo(readcsv[,-1], format="%Y%m%d", sep=",", tz="", dec=".", header = TRUE, index.column=1)
      data <- as.xts(zooobj)
    } else {
      data <- getSymbols(input$symb, src = "yahoo", 
                          from = input$dates[1],
                          to = input$dates[2],
                          auto.assign = FALSE)
      
      if(inputSymbol != input$symb) {
        inputSymbol <<- input$symb  
      }
      
    }
    range = paste(input$dates[1],'/',input$dates[2], sep = "")
    dcols <- data[,4]
    #datats <- as.ts(dcols)
    datats <- ts(dcols[range])
    #fit <- auto.arima(datats)
    fit <- nnetar(datats)
    grap <- forecast(fit, h=input$forecastsize)
    plot(grap, main="Przewidywany kurs")
  })
  
  output$plot <- renderPlot({
    cs <- input$resetcsv
    if(cs != clickNumber) {
      fileflag <<- FALSE
      clickNumber <<- cs
    } else if(input$dates[1] != inputData1 | input$dates[2] != inputData2) {
      inputData1 <<- input$dates[1]
      inputData2 <<- input$dates[2]
    } else {
      fileflag <<- TRUE
    }
    inFile <- input$file1
    if (!is.null(inFile) & fileflag==TRUE & inputSymbol==input$symb) {
      readcsv <- read.csv(inFile$datapath, header=TRUE, sep=",", quote='"', as.is=TRUE)
      zooobj <- read.zoo(readcsv[,-1], format="%Y%m%d", sep=",", tz="", dec=".", header = TRUE, index.column=1)
      myxtsTMP <- as.xts(zooobj)
      myxts <- align.time(myxtsTMP,1) 
    } else {
      myxts <- getSymbols(input$symb, src = "yahoo", 
             from = input$dates[1],
             to = input$dates[2],
             auto.assign = FALSE)
      
      if(inputSymbol != input$symb) {
        inputSymbol <<- input$symb  
      }
    }
    
    smasizev = 20
    if(input$smasize>0)
      smasizev = input$smasize
    
    TAstr = " "
    if(input$rsi==TRUE )
      TAstr = paste(TAstr, "addRSI(n = 14, maType = 'EMA', wilder = TRUE)")
    if(input$sma==TRUE) {
      if(input$rsi==TRUE) {
          TAstr = paste(TAstr, ";addSMA(n = ", smasizev, ", on = 1, with.col = Cl, overlay = TRUE, col = 'red')")
      } else {
          TAstr = paste(TAstr, "addSMA(n = ", smasizev, ", on = 1, with.col = Cl, overlay = TRUE, col = 'red')")
      }
    }
    range = paste(input$dates[1],'/',input$dates[2], sep = "")
    chartSeries(myxts[range], theme = chartTheme("black"), name = "Wykres ceny",
      type = "line", log.scale = input$log, TA = TAstr)
  })
  
  output$resetableFileInput <- renderUI({
    input$resetcsv
    fileInput('file1', 'Lub wybierz plik CSV:',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv'))
  })
  
})