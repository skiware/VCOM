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
      actionButton("testButton","TESTING",width="100%"),
      actionButton("runModel","RUN",width="100%"),
      actionButton("plot","PLOT",width="100%")
    ),
    mainPanel(
      headerPanel(h1("Outputs")),
      textOutput("reactive"),
      textOutput("recalculate"),
      textOutput("replot")
    )
  )
  #############################################################################
))
###############################################################################