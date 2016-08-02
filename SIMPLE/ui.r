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
library(gridGraphics)
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
source("multiplot.R")
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
ENTER_DOWN_RUN <<- '
  $(document).ready(function(){
    $("body").keydown(function(e){
      if(e.which === 13){$("#buttonRun").click();}
    });
  });'
WORKING_MESSAGE_STYLE <<- "#loadmessage {
    position: fixed;top: 50%;left: 0px;line-height: 80px;height: 100px;
    width: 100%;padding: 5px 0px 5px 0px;text-align: center;font-weight: bold;
    font-size: 200%;color: #FFFFFF;background-color: rgba(100, 200, 255, .75);z-index: 105;
  }"
#
#Source reduction, coverage value, time it is on
INITIAL_SRE_COVERAGE = 0.0
INITIAL_SRE_TIME     = INITIAL_MODELRUNTIME_VALUE + 1
#LArvaciding, coverage value, time it is on
INITIAL_LAR_COVERAGE = .00
INITIAL_LAR_TIME     = INITIAL_MODELRUNTIME_VALUE + 1
#Biological, coverage value, time it is on
INITIAL_BIO_COVERAGE = .00
INITIAL_BIO_TIME     = INITIAL_MODELRUNTIME_VALUE + 1
#ATSB, coverage value, time it is on
INITIAL_ATSB_COVERAGE = .00
INITIAL_ATSB_TIME     = INITIAL_MODELRUNTIME_VALUE + 1
#Space Spraying, coverage value, time it is on
INITIAL_SSP_COVERAGE = .00
INITIAL_SSP_TIME     = INITIAL_MODELRUNTIME_VALUE + 1
#Odor Traps, coverage value, time it is on
INITIAL_OBT_COVERAGE = .00
INITIAL_OBT_TIME     = INITIAL_MODELRUNTIME_VALUE + 1
#LLINs, coverage value, time it is on
INITIAL_ITN_COVERAGE = .00
INITIAL_ITN_TIME     = INITIAL_MODELRUNTIME_VALUE + 1
#IRS
INITIAL_IRS_COVERAGE = 0.00
INITIAL_IRS_TIME     = INITIAL_MODELRUNTIME_VALUE + 1
#House modification
INITIAL_HOU_COVERAGE = 0.0
INITIAL_HOU_TIME     = INITIAL_MODELRUNTIME_VALUE + 1
#Cattle - Systemic
INITIAL_ECS_COVERAGE = 0.00
INITIAL_ECS_TIME     = INITIAL_MODELRUNTIME_VALUE + 1
#Cattle - Topical
INITIAL_ECT_COVERAGE = 0.00
INITIAL_ECT_TIME     = INITIAL_MODELRUNTIME_VALUE + 1

#Resting and Ovipositing - OviTraps -assuming same coverage for ATSB and SSP
INITIAL_OVI_COVERAGE = 0.00
INITIAL_OVI_TIME     = INITIAL_MODELRUNTIME_VALUE + 1

## Get intervetions parameters - LLINs for testing
#INTERVENTION_PARAMETERS = getInterventionsParameters(ITNcov=INITIAL_ITN_COVERAGE,time_ITN_on=INITIAL_ITN_TIME)

## Get intervetions parameters
INTERVENTION_PARAMETERS = getInterventionsParameters(
                                  #Source Reduction
                          SREcov=INITIAL_SRE_COVERAGE,time_SRE_on=INITIAL_SRE_TIME,
                                #Larvaciding
                          LARcov=INITIAL_LAR_COVERAGE,time_LAR_on=INITIAL_LAR_TIME,
                                #Biological Control
                          BIOcov=INITIAL_BIO_COVERAGE,time_BIO_on=INITIAL_BIO_TIME,
                                  #ATSB
                          ATSBcov=INITIAL_ATSB_COVERAGE,time_ATSB_on=INITIAL_ATSB_TIME,
                                #Space Spraying
                          SSPcov=INITIAL_SSP_COVERAGE,time_SSP_on=INITIAL_SSP_TIME,
                                  #Odor Traps
                          OBTcov=INITIAL_OBT_COVERAGE,time_OBT_on=INITIAL_OBT_TIME,
                                 #LLINs
                          ITNcov=INITIAL_ITN_COVERAGE,time_ITN_on=INITIAL_ITN_TIME,
                                  #IRS
                          IRScov=INITIAL_IRS_COVERAGE,time_IRS_on=INITIAL_IRS_TIME,
                                   #House Modification
                          HOUcov=INITIAL_HOU_COVERAGE,time_HOU_on=INITIAL_HOU_TIME,
                                  #Cattle - Systemic
                          ECScov=INITIAL_ECS_COVERAGE,time_ECS_on=INITIAL_ECS_TIME,
                                 #Cattle - topical
                          ECTcov=INITIAL_ECT_COVERAGE,time_ECT_on=INITIAL_ECT_TIME,
                                 #Resting & Ovipositing - Ovitraps --same for ATSB, SSP
                          OVIcov=INITIAL_OVI_COVERAGE,time_OVI_on=INITIAL_OVI_TIME)

###################################################################################
# PRIME SYSTEM  ###################################################################
MOSQUITO_PARAMETERS = getAnGambiaeParameters()
#INTERVENTION_PARAMETERS = getInterventionsParameters(time_ITN_on=INITIAL_ITN_TIME,ITNcov=INITIAL_ITN_COVERAGE)
theta <<- getTheta(interventionParameters=INTERVENTION_PARAMETERS)
#initialParametersValues <<- importCSVParametersFromDirectory("SetupTemplates/SETUP_MosquitoLifeCycleParameters.csv")
#theta <<- parseImportedCSVParameters(initialParametersValues)
# MODEL -------------------------------------
timing <<- seq(0,INITIAL_TIME_VALUE,by=1)
initState <<- calculateInitialState(theta)
IVM_traj <<- runODE(INITIAL_TIME_VALUE,1,initState,theta,"lsoda")
###################################################################################
shinyUI(
  fluidPage(theme = shinytheme("cerulean"),
    titlePanel(h1("VCOM: Simple",align="center"),windowTitle="VCOM: Simple"),
    titlePanel(h4("Vector Control Optimization Model",align="center")),
    ###############################################################################
    useShinyjs(),
    tags$script(ENTER_DOWN_RUN),
    tags$head(tags$style(type="text/css",WORKING_MESSAGE_STYLE)),
    conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div("Working...",id="loadmessage")),
    ###############################################################################
    navbarPage("",id="nav",
        #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
        tabPanel("Main",
          sidebarLayout(
            sidebarPanel(
              #####################################################################
              titlePanel(h1("Instructions",align="left")),
              helpText("(1) Select the mosquito species."),
              helpText("(2) Select the EIR interval."),
              helpText("(3) Set the Q0 level."),
              helpText("(4) Select the number of days."),
              helpText("(5) Run the model! (click the button or hit 'ENTER')"),
              helpText("(6) Setup the desired interventions and repeat step 3 as required."),
              helpText("(7) Additionally you can download results in the 'Files Output' tab."),
              fluidRow(h3("1. Mosquito Selection")),
              radioButtons("radioSpecies",label=NULL,
                choices=list(
                  "An. gambiae"="GAM",
                  "An. arabiensis"="ARA",
                  "An. funestus"="FUN"
                ),selected="GAM"),
              #####################################################################
              fluidRow(h3("2. EIR Level Selection")),
              radioButtons("EIR",label=NULL,
                choices=list(
                  "0<=EIR<10"="10",
                  "10<=EIR<100"="100",
                  "100<=EIR<1000"="1000"
                ),selected="100"),
              #####################################################################
              fluidRow(h3("3. Q0")),
              sliderInput("Q0",label=NULL,min=0,max=1,value=0,step=.01),
              #####################################################################
              fluidRow(h3("4. Simulation Time")),
              sliderInput("sliderTime","Days to Simulate:",min=1,max=365,value=80),
              #####################################################################
              fluidRow(h3("5. Run Model")),
              actionButton("buttonRun","Run",width="100%"),
              #####################################################################
              fluidRow(h3("6. Interventions")),
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
              ),
              fluidRow(
                column(COVERAGE_LABELS_SIZE,h5("ATSB",align="left")),
                column(COVERAGE_BAR_SIZE,sliderInput("ATSBcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
                column(COVERAGE_INIT_SIZE,numericInput("time_ATSB_on",NULL,value=0,min=0,max=365))
              )
            ),mainPanel(
              plotOutput("plotTrajectory"),
              plotOutput("plotDemographics"),
              plotOutput("plotEIR")
            )
          ),
          helpText("Cite as: "),
          helpText("Contacts: <Model: Samson.Kiware@ucsf.edu> <GUI: sanchez.hmsc@itesm.mx>"),
          helpText("CSS theme downloaded from: http://bootswatch.com (MIT licence)")
        ),
        tabPanel("Files Output",
          titlePanel(h1("Export and Download",align="left")),
          helpText("Run the model at least once for the buttons to activate."),
          fluidRow(
            column(4,align="center",
              titlePanel(h3("Data",align="center")),
              downloadButton("downloadCSVTrace", 'Download CSV Trace')
              #downloadButton("downloadCSVEIR", 'Download CSV EIR'),
              #downloadButton("downloadCSVDemographics", 'Download CSV Demographics')
            ),
            column(4,""),
            column(4,align="center",
              titlePanel(h3("Plots",align="center")),
              downloadButton("downloadPlotTrace", 'Download Trajectory Plot'),
              downloadButton("downloadPlotEIR", 'Download EIR Plot'),
              downloadButton("downloadPlotDemographics", 'Download Demographics Plot')
            )
          )
        )
      )
    )
  )
