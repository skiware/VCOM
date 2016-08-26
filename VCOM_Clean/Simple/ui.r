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
library(plotly)
library(BH)
source("ODEModel.R")
source("ODEMosquitoParameters.R")
source("Plots.R")
source("ODEControlMeasuresParameters.R")
source("ODETransmissionParameters.R")
source("ODEInterventions.R")
source("ODEModelOutput.R")
source("ODEFeedingCycle.R")
################################################################################
# GLOBAL GUI PARAMETERS ########################################################
importedFile = NULL
BOXES_WIDTH <<- "75px"
COVERAGE_STEP_SIZE = .025
COVERAGE_LABELS_SIZE = 3
COVERAGE_BAR_SIZE = 5
COVERAGE_INIT_SIZE = 4
INITIAL_TIME_VALUE = 365
INITIAL_ITN_COVERAGE = .5
INITIAL_EIR = 100
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
###################################################################################
# PRIME SYSTEM  ###################################################################
MOSQUITO_PARAMETERS = getAnGambiaeParameters()
TRANSMISSION_PARAMETERS = getAdditionalTransmissionParameters(epsilon0 = INITIAL_EIR/365)
INTERVENTION_PARAMETERS = getInterventionsParameters(time_ITN_on=INITIAL_ITN_TIME,ITNcov=INITIAL_ITN_COVERAGE)
theta <<- getTheta(interventionParameters=INTERVENTION_PARAMETERS)
#initialParametersValues <<- importCSVParametersFromDirectory("SetupTemplates/SETUP_MosquitoLifeCycleParameters.csv")
#theta <<- parseImportedCSVParameters(initialParametersValues)
# MODEL -------------------------------------
timing <<- seq(0,INITIAL_TIME_VALUE,by=1)
initState <<- calculateInitialState(theta)
IVM_traj <<- runODE(INITIAL_TIME_VALUE,1,initState,theta,"daspk")
###################################################################################
shinyUI(
  fluidPage(theme = shinytheme("cerulean"),
    titlePanel(h1("VCOM: Simple",align="center"),windowTitle="VCOM: Simple"),
    titlePanel(h4("Vector Control Optimization Model",align="center")),
    titlePanel(h4(tags$a(class="btn btn-default", href="http://chipdelmal.github.io/VCOM/", "Home", style="display: block; width: 100%;"),align="center")),
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
              helpText("(2) Select the EIR (entomological inoculation rate) level."),
              #helpText("(3) Set the Q0 level (proportion of bites taken on humans with respect to cattle)."),
              helpText("(3) Select the number of days to simulate."),
              helpText("(4) Run the model! (click the button or hit 'ENTER')"),
              helpText("(5) Setup the desired interventions and repeat step 3 as required."),
              helpText("(6) Additionally you can download results in the 'Files Output' tab."),
              helpText("(7) Eliminate malaria!"),
              fluidRow(h3("1. Mosquito Selection")),
              radioButtons("radioSpecies",label=NULL,
                choices=list(
                  "An. gambiae"="GAM",
                  "An. arabiensis"="ARA",
                  "An. funestus"="FUN"
                ),selected="GAM"),
              #####################################################################
              fluidRow(h3("2. EIR Level Selection")),
              sliderInput("radioEIR",label=NULL,min=0.1,max=1000,value=100,step=10),
              #radioButtons("radioEIR",label=NULL,
              #  choices=list(
              #    "10"=10,
              #    "100"=100,
              #    "1000"=1000
              #  ),selected=100),
              #####################################################################
              # fluidRow(h3("3. Q0")),
              # sliderInput("Q0",label=NULL,min=0,max=1,value=0,step=.01),
              #####################################################################
              fluidRow(h3("3. Simulation Time")),
              sliderInput("sliderTime","Days to Simulate:",min=1,max=365,value=INITIAL_TIME_VALUE),
              #####################################################################
              fluidRow(h3("4. Run Model")),
              actionButton("buttonRun","Run",width="100%"),
              #####################################################################
              fluidRow(h3("5. Interventions")),
              fluidRow(helpText(
                "Key: Ovitraps (OVI), Larviciding (LAR), Biological Control (BIO), Source Reduction (SRE)
                Insecticide-Treated Nets (ITN), Indoor Residual Spraying (IRS), Endocticide (ECS), Endocticide (ECT), House Modifications (HOU),
                Odor-Baited Traps (OBT), Spatial-Spraying (SPR), Personal Protection Measure (PPM)"
              )),
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
              # fluidRow(
              #   column(COVERAGE_LABELS_SIZE,h5("FOG",align="left")),
              #   column(COVERAGE_BAR_SIZE,sliderInput("FOGcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
              #   column(COVERAGE_INIT_SIZE,numericInput("time_FOG_on",NULL,value=0,min=0,max=365))
              # ),
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
              plotlyOutput("plotEIR"),
              fluidRow(h1("")),
              fluidRow(h1("")),
              plotlyOutput("plotTrajectory"),
              fluidRow(h1("")),
              fluidRow(h1("")),
              plotlyOutput("plotDemographics"),
              fluidRow(h1("")),
              fluidRow(h1("")),
              #plotlyOutput("plotVC"),
              #plotlyOutput("plotR0"),
              plotlyOutput("plotHuman")
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
              fluidRow(
              downloadButton("downloadPlotTrace", 'Download Trajectory Plot'),
              downloadButton("downloadPlotEIR", 'Download EIR Plot'),
              downloadButton("downloadPlotDemographics", 'Download Demographics Plot'),
              downloadButton("downloadPlotVC", 'Download VC Plot'),
              downloadButton("downloadPlotR0", 'Download R0 Plot'),
              downloadButton("downloadPlotHuman", 'Download Human Plot')
              )
            )
          )
        )
      )
    )
  )
