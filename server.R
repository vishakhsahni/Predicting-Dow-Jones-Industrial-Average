library(quantmod)
library(AppliedPredictiveModeling)
library(TTR)
library(DMwR)
library(neuralnet)
library(dygraphs)
library(tseries)
library(forecast)

#source("neuralModel.R")

# for 3 graphs [predictplot]
getSymbols("DJIA")
dfdj<- as.data.frame(DJIA)
dfdj<-dfdj[-1:-5]
dfdj<- ts(dfdj, frequency = 12)

###
### Shiny Server

shinyServer(function(input, output) {
  ##
  output$plot <- renderPlot({
    TSData <- getSymbols(input$symb, 
                         from = input$dates[1],
                         to = input$dates[2],
                         auto.assign = FALSE)
    
    chartSeries(TSData, theme = chartTheme("black"), 
                type = "line", log.scale = input$log)
    })
  output$logplot <- renderDygraph({
    TSData <- getSymbols(input$symb, 
                         from = input$dates[1],
                         to = input$dates[2],
                         auto.assign = FALSE)
    dygraph(TSData) %>%
      dyOptions(colors = RColorBrewer::brewer.pal(8, "Set2"))%>%
      dyHighlight(highlightCircleSize = 5, 
                  highlightSeriesBackgroundAlpha = 2,
                  hideOnMouseOut = T)%>%
      dyAxis("y", label = "Stock Price") %>%
      dyCandlestick()
  })  
##
# ##  
#   output$predictplot <- renderPlot({
#     plot(predicts)
#   }) 
   output$predictplot <- renderPlot({
     lay= par(no.readonly = T)
     layout(matrix(c(1,1,2,3),2,2,byrow = T))
     plot.ts(dfdj);acf(dfdj, main = "Auto-Correlation");pacf(dfdj,main = "Partial Correalation")
     par(lay)
  })
   output$decomposed <- renderPlot({
     plot(decompose(dfdj))
   }) 
   output$predicttable <- renderTable({
    print(predicts)
  })
   output$ArimaForcast <- renderTable({
     Df <- as.data.frame(getSymbols(input$symb, env = NULL))
     Df$Date <- as.Date(rownames(Df))
     Df<-Df[-1:-5]
     Df<- ts(Df[-2], frequency = 12)
     boxtest<-Box.test(Df,lag = 200,type = "Ljung-Box")
     adftest<-adf.test(Df,alternative = "stationary")
     kpsstest<-kpss.test(Df)
     #fit2<-auto.arima(Df)
     fit = arima(Df, order = c(3,0,0))
     fit_resi = residuals(fit)
     boxtestFit<-Box.test(fit_resi,lag = 10,type = "Ljung-Box")
     forcast_Fit <- forecast(fit,h=4)
     Accuracy<-accuracy(forcast_Fit)
     Result<- plot(forecast(fit,h=40),include = 40)
     Result<- as.data.frame(Result)
     print(forcast_Fit)
   })
   output$resultArima <- renderPlot({
     Df <- as.data.frame(getSymbols(input$symb, env = NULL))
     Df$Date <- as.Date(rownames(Df))
     Df<-Df[-1:-5]
     Df<- ts(Df[-2], frequency = 12)
     #fit2<-auto.arima(Df)
     fit = arima(Df, order = c(3,0,0))
     #fit_resi = residuals(fit)
     #boxtestFit<-Box.test(fit_resi,lag = 10,type = "Ljung-Box")
     #forcast_Fit <- forecast(fit,h=4)
     #Accuracy<-accuracy(forcast_Fit)
     plot(forecast(fit,h=40),include = 40)
   })
   ##
   
   output$ArimaForcastAuto <- renderTable({
     Df <- as.data.frame(getSymbols(input$symb, env = NULL))
     Df$Date <- as.Date(rownames(Df))
     Df<-Df[-1:-5]
     Df<- ts(Df[-2], frequency = 12)
     fit2<-auto.arima(Df)
     fit_resi2 = residuals(fit2)
     forcast_Fit2 <- forecast(fit2,h=4)
     print(forcast_Fit2)
   })
   output$resultArima2 <- renderPlot({
     Df <- as.data.frame(getSymbols(input$symb, env = NULL))
     Df$Date <- as.Date(rownames(Df))
     Df<-Df[-1:-5]
     Df<- ts(Df[-2], frequency = 12)
     fit3<-auto.arima(Df)
     fit_resi3 = residuals(fit3)
     plot(forecast(fit3,h=40),include = 40)
   })
   
   ##
   output$summary <- renderPrint({
     TSD <- getSymbols(input$symb, 
                          from = input$dates[1],
                          to = input$dates[2],
                          auto.assign = FALSE)
     summary(TSD)
   })
   output$interactive <- renderDygraph({
     DDf <- as.data.frame(getSymbols(Symbols = input$symb, env = NULL), from = input$dates[1],
                          to = input$dates[2],
                          auto.assign = FALSE)
     DDf$Date <- as.Date(rownames(DDf))
     
     stocks <- xts(DDf[-7], order.by=as.Date(DDf[,7], "%m/%d/%Y"), frequency = 12)
     dygraph(stocks, main = "Interactive Stock Graph") %>%
       dyOptions(colors = RColorBrewer::brewer.pal(8, "Set2"))%>%
       dyHighlight(highlightCircleSize = 5, 
                   highlightSeriesBackgroundAlpha = 0.7,
                   hideOnMouseOut = FALSE)%>%
       dyAxis("y", label = "Stock Price") %>%
       dyRangeSelector()
     
   })
})

