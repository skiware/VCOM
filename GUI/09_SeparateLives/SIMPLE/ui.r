#------------------------------------------------------------------------------#
################################################################################
## Malaria vector ODE model GUI                                               ##
## Hector M. Sanchez C. (sanchez.hmsc@itesm.mx)                               ##
## 02/May/2016                                                                ##
################################################################################
#------------------------------------------------------------------------------#

################################################################################
#LOAD LIBRARIES AND FILES
################################################################################
library(shiny)
library(deSolve)
library(ggplot2)
source("ODEModel.R")
source("ODEMosquitoParameters.R")
source("ODEAuxiliaryFunctions.R")
source("ODEControlMeasuresParameters.R")
source("ODETransmissionParameters.R")
################################################################################
# GLOBAL GUI PARAMETERS ########################################################
importedFile = NULL
BOXES_WIDTH <<- "75px"
COVERAGE_STEP_SIZE = .025
COVERAGE_LABELS_SIZE = 3
COVERAGE_BAR_SIZE = 5
COVERAGE_INIT_SIZE = 4
INITIAL_TIME_VALUE = 80
################################################################################
# PRIME SYSTEM  ################################################################
MOSQUITO_PARAMETERS = getAnGambiaeParameters()
INTERVENTION_PARAMETERS = getInterventionsParameters()
theta <<- getTheta()
#initialParametersValues <<- importCSVParametersFromDirectory("SetupTemplates/SETUP_MosquitoLifeCycleParameters.csv")
#theta <<- parseImportedCSVParameters(initialParametersValues)
# MODEL -------------------------------------
initState <<- calculateInitialState(theta)
IVM_traj <<- runODE(INITIAL_TIME_VALUE,1,initState,theta,"lsoda") 
################################################################################
shinyUI(
  fluidPage(theme = "bootstrapSpace.css",
    titlePanel(h1("VCOM: Simple",align="center")),
    titlePanel(h4("Vector Control Optimization Model",align="center")),
    actionButton("buttonRun","Run Model",width="100%"),
    #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      sidebarLayout(  
        sidebarPanel(
          fluidRow(h3("Mosquito Selection")),
          radioButtons("radioSpecies",label=NULL,
          choices=list(
            "An. gambiae"="GAM",
            "An. arabiensis"="ARA",
            "An. funestus"="FUN"
          ),selected="GAM"),
          #####################################################################
          fluidRow(h3("Simulation Time")),
          sliderInput("sliderTime","Days to Simulate:",min=1,max=365,value=80),
          #####################################################################
          fluidRow(h3("Interventions")),
          fluidRow(
            column(COVERAGE_LABELS_SIZE,h4("",align="center")),
            column(COVERAGE_BAR_SIZE,h4("Coverage",align="center")),
            column(COVERAGE_INIT_SIZE,h4("Time",align="center"))
          ),
          fluidRow(
            column(COVERAGE_LABELS_SIZE,h5("OVI",align="left")),
            column(COVERAGE_BAR_SIZE,sliderInput("OVIcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
            column(COVERAGE_INIT_SIZE,numericInput("time_OVI_on",NULL,value=0,min=0,max=365))
          ),
          fluidRow(
            column(COVERAGE_LABELS_SIZE,h5("FOG",align="left")),
            column(COVERAGE_BAR_SIZE,sliderInput("FOGcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
            column(COVERAGE_INIT_SIZE,numericInput("time_FOG_on",NULL,value=0,min=0,max=365))
          ),
          fluidRow(
            column(COVERAGE_LABELS_SIZE,h5("LAR",align="left")),
            column(COVERAGE_BAR_SIZE,sliderInput("LARcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
            column(COVERAGE_INIT_SIZE,numericInput("time_LAR_on",NULL,value=0,min=0,max=365))
          ),
          fluidRow(
            column(COVERAGE_LABELS_SIZE,h5("BIO",align="left")),
            column(COVERAGE_BAR_SIZE,sliderInput("BIOcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
            column(COVERAGE_INIT_SIZE,numericInput("time_OVI_on",NULL,value=0,min=0,max=365))
          ),
          fluidRow(
            column(COVERAGE_LABELS_SIZE,h5("SRE",align="left")),
            column(COVERAGE_BAR_SIZE,sliderInput("SREcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
            column(COVERAGE_INIT_SIZE,numericInput("time_SRE_on",NULL,value=0,min=0,max=365))
          ),
          fluidRow(
            column(COVERAGE_LABELS_SIZE,h5("ITN",align="left")),
            column(COVERAGE_BAR_SIZE,sliderInput("ITNcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
            column(COVERAGE_INIT_SIZE,numericInput("time_ITN_on",NULL,value=0,min=0,max=365))
          ),
          fluidRow(
            column(COVERAGE_LABELS_SIZE,h5("IRS",align="left")),
            column(COVERAGE_BAR_SIZE,sliderInput("IRScov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
            column(COVERAGE_INIT_SIZE,numericInput("time_IRS_on",NULL,value=0,min=0,max=365))
          ),
          fluidRow(
            column(COVERAGE_LABELS_SIZE,h5("IVM",align="left")),
            column(COVERAGE_BAR_SIZE,sliderInput("IVMcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
            column(COVERAGE_INIT_SIZE,numericInput("time_IVM_on",NULL,value=0,min=0,max=365))
          ),
          fluidRow(
            column(COVERAGE_LABELS_SIZE,h5("HOU",align="left")),
            column(COVERAGE_BAR_SIZE,sliderInput("HOUcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
            column(COVERAGE_INIT_SIZE,numericInput("time_HOU_on",NULL,value=0,min=0,max=365))
          ),
          fluidRow(
            column(COVERAGE_LABELS_SIZE,h5("ODO",align="left")),
            column(COVERAGE_BAR_SIZE,sliderInput("ODOcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
            column(COVERAGE_INIT_SIZE,numericInput("time_ODO_on",NULL,value=0,min=0,max=365))
          ),
          fluidRow(
            column(COVERAGE_LABELS_SIZE,h5("SPA",align="left")),
            column(COVERAGE_BAR_SIZE,sliderInput("SPAcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
            column(COVERAGE_INIT_SIZE,numericInput("time_SPA_on",NULL,value=0,min=0,max=365))
          ),
          #####################################################################
          fluidRow(h3("Downloads")),
          helpText("Be sure to hit the 'Run Model' button at least once before downloading."),
          downloadButton("downloadParameters", 'Download CSV Parameters'),
          fluidRow(h4("")),
          downloadButton("downloadTrace", 'Download CSV Trace'),
          fluidRow(h4("")),
          downloadButton("downloadPlot", 'Download Trajectory Plot')
      ),
      mainPanel(
        plotOutput("plotTrajectory"),
        plotOutput("plotDemographics")
      )
    ),
    p("Cite as: VCOM!!! The coolest model ever!!!")
  )
)
#################################################################################