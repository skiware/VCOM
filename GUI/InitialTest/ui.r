library(shiny)
library(ggplot2)

shinyUI(fluidPage(
  #############################################################################
  titlePanel("Coolest GUI for Coolest Model Eveeeeeeer!!!!!!"),
  #############################################################################
  sidebarLayout(
    sidebarPanel(
      headerPanel(h1("Inputs")),
      #########################################################################
      radioButtons("radio",label=h2("Species Selection"),
                   choices=list(
                     "An. gambiae"="GAM",
                     "An. arabiensis"="ARA",
                     "An. funestus"="FUN"
                  ),selected="ARA"),
      #########################################################################
      headerPanel(h2("Parameters")),
      sliderInput("bins","Number of bins:",min=1,max=50,value=30),
      sliderInput("maxTime","Number of days:",min=1,max=365,value=50),
      sliderInput("beta","Beta:",min=0,max=1,value=.5),
      sliderInput("muEL","muEL:",min=0,max=1,value=.5),
      sliderInput("hbi","Human Blood Index",min=0,max=1,value=.5),
      submitButton(text="Run Model",icon=NULL,width="100%"),
      #########################################################################
      headerPanel(h2("CM Coverage")),
      fluidRow(
        column(4,
               textInput("irsCov","IRS",value="",placeholder=0.5,width="50px"),
               textInput("itnCov","ITN",value="",placeholder=0.5,width="50px"),
               textInput("ivmCov","IVM",value="",placeholder=0.5,width="50px"),
               textInput("houCov","HOU",value="",placeholder=0.5,width="50px"),
               textInput("oviCov","OVI",value="",placeholder=0.5,width="50px")
        ),
        column(4,
               textInput("fogCov","FOG",value="",placeholder=0.5,width="50px"),
               textInput("larCov","LAR",value="",placeholder=0.5,width="50px"),
               textInput("odoCov","ODO",value="",placeholder=0.5,width="50px"),
               textInput("bioCov","BIO",value="",placeholder=0.5,width="50px"),
               textInput("sreCov","SRE",value="",placeholder=0.5,width="50px")
        ),
        column(4,
               textInput("spaCov","SPA",value="",placeholder=0.5,width="50px"),
               textInput("naaCov","NA ",value="",placeholder=0.0,width="50px"),
               textInput("naaCov","NA ",value="",placeholder=0.0,width="50px"),
               textInput("naaCov","NA ",value="",placeholder=0.0,width="50px"),
               textInput("naaCov","NA ",value="",placeholder=0.0,width="50px")
        )
      ),
      helpText("Authors: Samson, Sean, Hector, John")
    ),
    #########################################################################
  mainPanel(
    headerPanel(h1("Outputs")),
    
    fluidRow(
        plotOutput("distPlot"),
        headerPanel(h1("")),
        headerPanel(h1("")),
        headerPanel(h1("")),
        textOutput("text1")
      )
  )
  ###########################################################################
  )
))