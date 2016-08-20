#############################################
######VCOM Model Output Web Application######
######Sean Wu 8/19/2016######################
#############################################

library(shiny)
library(htmlwidgets)
library(D3TableFilter)

shinyUI(fluidPage(
  title = 'VCOM Output',
  fluidRow(
    column(width = 12, d3tfOutput('vcom', height = "auto"))
  )
))