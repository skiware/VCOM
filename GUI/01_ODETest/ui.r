###############################################################################
#LOAD LIBRARIES AND FILES
###############################################################################
library(shiny)
source("ODE.R")
###############################################################################
shinyUI(fluidPage(
  #############################################################################
  titlePanel("Testing Event Handlers"),
  #############################################################################
  sidebarLayout(
    sidebarPanel(
      headerPanel(h1("Inputs")),
      #actionButton("testButton","TESTING",width="100%"),
      actionButton("runModel","RUN",width="100%"),
      actionButton("plot","PLOT",width="100%"),
      sliderInput("maxTime","Number of days:",min=1,max=365,value=50)
    ),
    mainPanel(
      fluidRow(
        headerPanel(h1("Outputs")),
        textOutput("reactive"),
        textOutput("recalculate"),
        #plotOutput("replot"),
        plotOutput("barChart")
      )
    )
  )
  #############################################################################
))
###############################################################################