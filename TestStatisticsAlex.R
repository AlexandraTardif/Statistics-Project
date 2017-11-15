library(shiny)
library(magclass)
  
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
  
#Essai de démarrage au 30 janvier 2015
# Creation du masque 
L <- list()
for (i in 1:10) {
    l[[i]] = as.data.frame(l[[i]])
    l[[i]]$Date = as.Date(l[[i]]$Date)
    masque = l[[i]]$Date >= as.Date("2015-01-01") & l[[i]]$Date <= as.Date("2017-10-01")
    L[[i]]=l[[i]][masque,]
}


for (i in 4:8) {
  print(cov(L[[i]]$Return,L[[10]]$Return))
  
}
print(cov(L[[1]]$Return,L[[10]]$Return))



