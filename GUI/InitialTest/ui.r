library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  #############################################################################
  titlePanel("Coolest GUI for Coolest Model Eveeeeeeer!!!!!!"),
  #############################################################################
  sidebarLayout(
    sidebarPanel(
      #########################################################################
      headerPanel("Parameters"),
      sliderInput("bins","Number of bins:",min=1,max=50,value=30),
      sliderInput("beta","Beta:",min=0,max=1,value=.5),
      sliderInput("muEL","muEL:",min=0,max=1,value=.5),
      submitButton(text="Run Model",icon=NULL,width="100%"),
      #########################################################################
      headerPanel("CM Coverage"),
      helpText("
          [IRS: Indoors Residual Spraying] 
          [ITN: Insecticide Treated Net]
          [IVM: Ivermectin]
          [HOU: House Modification]
          [OVI: Ovitrap]
          [FOG: Fogging]
          [LAR: Larvaciding]
          [ODO: Odor Traps]
          [BIO: Biological Control]
          [SRE: Source Reduction]
          [SPA: Spatial Spraying]
        "),
      fluidRow(
        column(4,
               textInput("inputId","IRS",value="",placeholder=.5,width="50px"),
               textInput("inputId","ITN",value="",placeholder=.5,width="50px"),
               textInput("inputId","IVM",value="",placeholder=.5,width="50px"),
               textInput("inputId","HOU",value="",placeholder=.5,width="50px"),
               textInput("inputId","OVI",value="",placeholder=.5,width="50px")
        ),
        column(4,
               textInput("inputId","FOG",value="",placeholder=.5,width="50px"),
               textInput("inputId","LAR",value="",placeholder=.5,width="50px"),
               textInput("inputId","ODO",value="",placeholder=.5,width="50px"),
               textInput("inputId","BIO",value="",placeholder=.5,width="50px"),
               textInput("inputId","SRE",value="",placeholder=.5,width="50px")
        ),
        column(4,
               textInput("inputId","SPA",value="",placeholder=.5,width="50px"),
               textInput("inputId","NA ",value="",placeholder=0,width="50px"),
               textInput("inputId","NA ",value="",placeholder=0,width="50px"),
               textInput("inputId","NA ",value="",placeholder=0,width="50px"),
               textInput("inputId","NA ",value="",placeholder=0,width="50px")
        )
      )
    ),
    #########################################################################
    mainPanel(plotOutput("distPlot"))
    #########################################################################
  )
))