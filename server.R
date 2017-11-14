
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(magclass)

shinyServer(function(input, output) {
  
  #loading data from csv
  names<-c("Toyota","General Motors","Tesla","Ford","Facebook","Honda","Nissan","Fiat Chrysler","S&P 500","Dow Jones")
  l<-list()
  l[[1]]<-read.csv("TM.csv",head=TRUE)
  l[[2]]<-read.csv("GM.csv",head=TRUE)
  l[[3]]<-read.csv("TSLA.csv",head=TRUE)
  l[[4]]<-read.csv("F.csv",head=TRUE)
  l[[5]]<-read.csv("FB.csv",head=TRUE)
  l[[6]]<-read.csv("HMC.csv",head=TRUE)
  l[[7]]<-read.csv("NSANY.csv",head=TRUE)
  l[[8]]<-read.csv("FCAU.csv",head=TRUE)
  l[[9]]<-read.csv("^GSPC.csv",head=TRUE) #sp500
  l[[10]]<-read.csv("^DJI.csv",head=TRUE) #dowjones
  
  #compute log-returns and store it in the list of dataframes l
  for (i in 1:10){
    l[[i]]['Return']<-lowpass(log(unlist(l[[i]]$Close)/unlist(l[[i]]$Open)))
    row.names(l[[i]])<-as.character(l[[i]]$Date)
  }
  
  
  #build ditribution histogram output
  output$distributionPlot <- renderPlot({
      chosenIndex<-which(names == input$stock)
      x    <- l[[chosenIndex]]$Return
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      titleHist=paste("histogram",input$stock,"with",input$bins,"bins")
      hist(x, main=titleHist , breaks = bins, col = 'darkgray', border = 'white')
  })
  
  #build log-return comparative graph:
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$comparePlot <- renderPlot({
      #determine index of chosen stock and compute intersect timeline and yRange for the plot
      
      chosenIndexes<-as.integer(input$comparedStocks)
      if(length(chosenIndexes)<1){chosenIndexes=c(3)}
      listDates<-list()
      for(i in 1:length(chosenIndexes)){listDates[[i]]<-row.names(l[[chosenIndexes[i]]])}
      timeline<-Reduce(intersect,listDates)
      listReturns<-list()
      for(i in 1:length(chosenIndexes)){listReturns[[i]]<-l[[chosenIndexes[i]]][timeline,]$Return}
      rangelist<-sapply(listReturns,range)
      rangePlot<-range(rangelist)
      if(is.null(ranges$y)){ranges$y=rangePlot}
      #create monthly xlabels for the plot
      dates<- strptime(as.character(timeline), "%Y-%m-%d")
      daterange=c(as.POSIXlt(min(timeline)),as.POSIXlt(max(timeline)))
      d1<-daterange[1]
      d2<-daterange[2]
      n<-d1$mday
      d1<-seq(d1, by = paste (1, "months"), length = 2)[2]
      d1<-as.Date(d1)-n+2
      d1<-strptime(as.character(d1),"%Y-%m-%d")
      sequence=seq(d1, d2, by="month")
      #plot for the chosen stock
      stocknames=paste(names[chosenIndexes],collapse=", ")
      titlePlot=paste('Comparative plot of',stocknames,"daily log-returns")
      title(titlePlot,xlab='Date',ylab='Return')  
      plot(dates,l[[chosenIndexes[1]]][timeline,]$Return, pch=16, cex=0.2,  ylim=ranges$y,xlim=ranges$x,ylab='',xlab='', xaxt="n",col=1)
      lines(dates,l[[chosenIndexes[1]]][timeline,]$Return, pch=16, cex=0.2,  ylim=ranges$y,xlim=ranges$x,ylab='',xlab='', xaxt="n",col=1)
      title(titlePlot,xlab='Date',ylab='Return') 
      if(length(chosenIndexes)>1){
          for(i in 2:length(chosenIndexes)){ lines(dates,l[[chosenIndexes[i]]][timeline,]$Return, pch=16, cex=0.2, ylim=ranges$y,xlim=ranges$x,ylab='',xlab='', xaxt="n",col=i) }
      }
      axis.POSIXct(1, at=sequence, format="%b", labels=format(sequence, "%Y/%m"))
      #legends and title
      
  })
  
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
})
