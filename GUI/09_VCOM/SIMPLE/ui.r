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
library(shinyjs)
library(shinythemes)
source("ODEModel.R")
source("ODEMosquitoParameters.R")
source("ODEAuxiliaryFunctions.R")
source("ODEControlMeasuresParameters.R")
source("ODETransmissionParameters.R")
source("ODEInterventions.R")
#source("ODEInterventions - FirstPass.R")
source("ODEModelOutput.R")
source("FeedingCycle.R")
################################################################################
# GLOBAL GUI PARAMETERS ########################################################
importedFile = NULL
BOXES_WIDTH <<- "75px"
COVERAGE_STEP_SIZE = .025
COVERAGE_LABELS_SIZE = 3
COVERAGE_BAR_SIZE = 5
COVERAGE_INIT_SIZE = 4
INITIAL_TIME_VALUE = 80
INITIAL_ITN_COVERAGE = .5
INITIAL_ITN_TIME = 50
###################################################################################
# PRIME SYSTEM  ###################################################################
MOSQUITO_PARAMETERS = getAnGambiaeParameters()
INTERVENTION_PARAMETERS = getInterventionsParameters(ITNcov=INITIAL_ITN_COVERAGE,time_ITN_on=INITIAL_ITN_TIME)
theta <<- getTheta(interventionParameters=INTERVENTION_PARAMETERS)
#initialParametersValues <<- importCSVParametersFromDirectory("SetupTemplates/SETUP_MosquitoLifeCycleParameters.csv")
#theta <<- parseImportedCSVParameters(initialParametersValues)
# MODEL -------------------------------------
initState <<- calculateInitialState(theta)
IVM_traj <<- runODE(INITIAL_TIME_VALUE,1,initState,theta,"lsoda")
###################################################################################
shinyUI(
  fluidPage(theme = shinytheme("cerulean"),
    titlePanel(h1("VCOM: Simple",align="center"),windowTitle="VCOM: Simple"),
    titlePanel(h4("Vector Control Optimization Model",align="center")),
    useShinyjs(),
    navbarPage("",id="nav",
        #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
        tabPanel("Main",
          sidebarLayout(
            sidebarPanel(
              titlePanel(h1("Instructions",align="left")),
              helpText("(1) Select the mosquito species."),
              helpText("(2) Select the number of days."),
              helpText("(3) Run the model!"),
              helpText("(4) Setup the desired interventions and repeat step 3 as required (ITN,IRS,HOU,ECS,ECT)."),
              helpText("(5) Additionally you can download results in the 'Files Output' tab."),
              fluidRow(h3("1. Mosquito Selection")),
              radioButtons("radioSpecies",label=NULL,
                choices=list(
                  "An. gambiae"="GAM",
                  "An. arabiensis"="ARA",
                  "An. funestus"="FUN"
                ),selected="GAM"),
              #####################################################################
              fluidRow(h3("2. Simulation Time")),
              sliderInput("sliderTime","Days to Simulate:",min=1,max=365,value=80),
              #####################################################################
              fluidRow(h3("3. Run Model")),
              actionButton("buttonRun","Run",width="100%"),
              #####################################################################
              fluidRow(h3("4. Interventions")),
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
                column(COVERAGE_INIT_SIZE,numericInput("time_BIO_on",NULL,value=0,min=0,max=365))
              ),
              fluidRow(
                column(COVERAGE_LABELS_SIZE,h5("SRE",align="left")),
                column(COVERAGE_BAR_SIZE,sliderInput("SREcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
                column(COVERAGE_INIT_SIZE,numericInput("time_SRE_on",NULL,value=0,min=0,max=365))
              ),
              fluidRow(
                column(COVERAGE_LABELS_SIZE,h5("ITN",align="left")),
                column(COVERAGE_BAR_SIZE,sliderInput("ITNcov",NULL,min=0,max=1,value=INITIAL_ITN_COVERAGE,step=COVERAGE_STEP_SIZE)),
                column(COVERAGE_INIT_SIZE,numericInput("time_ITN_on",NULL,value=INITIAL_ITN_TIME,min=0,max=365))
              ),
              fluidRow(
                column(COVERAGE_LABELS_SIZE,h5("IRS",align="left")),
                column(COVERAGE_BAR_SIZE,sliderInput("IRScov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
                column(COVERAGE_INIT_SIZE,numericInput("time_IRS_on",NULL,value=0,min=0,max=365))
              ),
              fluidRow(
                column(COVERAGE_LABELS_SIZE,h5("ECS",align="left")),
                column(COVERAGE_BAR_SIZE,sliderInput("ECScov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
                column(COVERAGE_INIT_SIZE,numericInput("time_ECS_on",NULL,value=0,min=0,max=365))
              ),
              fluidRow(
                column(COVERAGE_LABELS_SIZE,h5("ECT",align="left")),
                column(COVERAGE_BAR_SIZE,sliderInput("ECTcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
                column(COVERAGE_INIT_SIZE,numericInput("time_ECT_on",NULL,value=0,min=0,max=365))
              ),
              fluidRow(
                column(COVERAGE_LABELS_SIZE,h5("HOU",align="left")),
                column(COVERAGE_BAR_SIZE,sliderInput("HOUcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
                column(COVERAGE_INIT_SIZE,numericInput("time_HOU_on",NULL,value=0,min=0,max=365))
              ),
              fluidRow(
                column(COVERAGE_LABELS_SIZE,h5("OBT",align="left")),
                column(COVERAGE_BAR_SIZE,sliderInput("OBTcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
                column(COVERAGE_INIT_SIZE,numericInput("time_OBT_on",NULL,value=0,min=0,max=365))
              ),
              fluidRow(
                column(COVERAGE_LABELS_SIZE,h5("SPR",align="left")),
                column(COVERAGE_BAR_SIZE,sliderInput("SPRcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
                column(COVERAGE_INIT_SIZE,numericInput("time_SPR_on",NULL,value=0,min=0,max=365))
              ),
              fluidRow(
                column(COVERAGE_LABELS_SIZE,h5("PPM",align="left")),
                column(COVERAGE_BAR_SIZE,sliderInput("PPMcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
                column(COVERAGE_INIT_SIZE,numericInput("time_PPM_on",NULL,value=0,min=0,max=365))
              )
            ),mainPanel(
              plotOutput("plotTrajectory"),
              plotOutput("plotDemographics"),
              textOutput("results")
            )
          ),
          helpText("CSS theme downloaded from: http://bootswatch.com (MIT licence)"),
          helpText("Contacts: <Model: Samson.Kiware@ucsf.edu> <GUI: sanchez.hmsc@itesm.mx>")
        ),
        tabPanel("Files Output",
          titlePanel(h1("Export and Download",align="left")),
          fluidRow(
            column(4,align="center",
              titlePanel(h3("Data",align="center")),
              downloadButton("downloadTrace", 'Download CSV Trace')
            ),
            column(4,""),
            column(4,align="center",
              titlePanel(h3("Plots",align="center")),
              downloadButton("downloadPlot", 'Download Trajectory Plot')
            )
          )
        )#,
        #tags$script('$(document).on("keydown", function(e){Shiny.onInputChange("keypressed", e.which);});')
      )
    )
  )
