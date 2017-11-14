
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
names<-c("Toyota","General Motors","Tesla","Ford","Facebook","Honda","Nissan","Fiat Chrysler","S&P 500","Dow Jones")
a=1

shinyUI(
  
  navbarPage("Les Probas du swag !",
      tabPanel("Distribution",         
          # Sidebar with a slider input for number of bins
          sidebarLayout(
                    sidebarPanel(
                          selectInput("stock", "Stock:",names),
                          sliderInput("bins","Number of bins:",min = 1,max = 50,value = 30)
                    ),
                  # Show a plot of the generated distribution
                    mainPanel(
                      plotOutput("distributionPlot")
                    )
          )
      ),
      #tab2
      tabPanel("Comparison",
          
          sidebarLayout(
                sidebarPanel(  checkboxGroupInput("comparedStocks", "Compared Stocks",choiceNames=names,choiceValues=1:10,selected=c(1,2))  ),
                mainPanel(  
                  plotOutput("comparePlot", height = 300,
                             dblclick = "plot1_dblclick",
                             brush = brushOpts(
                               id = "plot1_brush",
                               resetOnNew = TRUE
                             )
                  )
                )
          )
      )
  )
)
    
