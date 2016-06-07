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
library(XLConnect)
#library(XML)
#library(kulife)
source("ODEModel.R")
source("ODEMosquitoParameters.R")
source("ODEAuxiliaryFunctions.R")
source("ODEControlMeasuresParameters.R")
source("ODETransmissionParameters.R")
################################################################################
importedFile = NULL
BOXES_WIDTH <<- "75px"
COVERAGE_STEP_SIZE = .025
COVERAGE_LABELS_SIZE = 4
COVERAGE_BAR_SIZE = 5
COVERAGE_INIT_SIZE = 3
# Theta from code----------------------------------
#initialTheta <<- getTheta()
# Theta from setup CSV----------------------------- 
initialParametersValues <<- importCSVParametersFromDirectory("SetupTemplates/SETUP_MosquitoLifeCycleParameters.csv")
#print(initialParametersValues)
theta <<- parseImportedCSVParameters(initialParametersValues)
initState <<- calculateInitialState(theta)
IVM_traj <<- runODE(80,1,initState,theta,"lsoda") 
#print(theta)
################################################################################
TEMPLATE_AN_ARABIENSIS<<-importCSVParametersFromDirectory("SetupTemplates/Template_AnArabiensis.csv")
TEMPLATE_AN_FUNESTUS<<-importCSVParametersFromDirectory("SetupTemplates/Template_AnFunestus.csv")
TEMPLATE_AN_GAMBIAE<<-importCSVParametersFromDirectory("SetupTemplates/Template_AnGambiae.csv")
################################################################################
shinyUI(
  fluidPage(theme = "bootstrapSpace.css",
    titlePanel(h1("VCOM: Simple",align="center")),
    titlePanel(h4("Vector Control Optimization Model",align="center")),
    actionButton("buttonRun","Run Model",width="100%"),
    navbarPage("",id="nav",
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Main",
        sidebarLayout(  
          sidebarPanel(
            fluidRow(h3("Mosquito Selection")),
            radioButtons("radioSpecies",label=NULL,
              choices=list(
                 "An. gambiae"="GAM",
                 "An. arabiensis"="ARA",
                 "An. funestus"="FUN"
            ),selected="ARA"),
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
              column(COVERAGE_LABELS_SIZE,h5("Ovitraps",align="left")),
              column(COVERAGE_BAR_SIZE,sliderInput("OVIcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
              column(COVERAGE_INIT_SIZE,textInput("time_OVI_on",NULL,value = 0))
            ),
            fluidRow(
              column(COVERAGE_LABELS_SIZE,h5("Fogging",align="left")),
              column(COVERAGE_BAR_SIZE,sliderInput("FOGcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
              column(COVERAGE_INIT_SIZE,textInput("time_FOG_on",NULL,value = 0))
            ),
            fluidRow(
              column(COVERAGE_LABELS_SIZE,h5("Larvaciding",align="left")),
              column(COVERAGE_BAR_SIZE,sliderInput("LARcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
              column(COVERAGE_INIT_SIZE,textInput("time_LAR_on",NULL,value = 0))
            ),
            fluidRow(
              column(COVERAGE_LABELS_SIZE,h5("Bio Control",align="left")),
              column(COVERAGE_BAR_SIZE,sliderInput("OVIcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
              column(COVERAGE_INIT_SIZE,textInput("time_OVI_on",NULL,value = 0))
            ),
            fluidRow(
              column(COVERAGE_LABELS_SIZE,h5("Source Reduction",align="left")),
              column(COVERAGE_BAR_SIZE,sliderInput("SREcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
              column(COVERAGE_INIT_SIZE,textInput("time_SRE_on",NULL,value = 0))
            ),
            fluidRow(
              column(COVERAGE_LABELS_SIZE,h5("Insecticide Nets",align="left")),
              column(COVERAGE_BAR_SIZE,sliderInput("IRScov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
              column(COVERAGE_INIT_SIZE,textInput("time_IRS_on",NULL,value = 0))
            ),
            fluidRow(
              column(COVERAGE_LABELS_SIZE,h5("Ivermectin",align="left")),
              column(COVERAGE_BAR_SIZE,sliderInput("IVMcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
              column(COVERAGE_INIT_SIZE,textInput("time_IVM_on",NULL,value = 0))
            ),
            fluidRow(
              column(COVERAGE_LABELS_SIZE,h5("House Modification",align="left")),
              column(COVERAGE_BAR_SIZE,sliderInput("HOUcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
              column(COVERAGE_INIT_SIZE,textInput("time_HOU_on",NULL,value = 0))
            ),
            fluidRow(
              column(COVERAGE_LABELS_SIZE,h5("Odor Traps",align="left")),
              column(COVERAGE_BAR_SIZE,sliderInput("ODOcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
              column(COVERAGE_INIT_SIZE,textInput("time_ODO_on",NULL,value = 0))
            ),
            fluidRow(
              column(COVERAGE_LABELS_SIZE,h5("Spatial Repellents",align="left")),
              column(COVERAGE_BAR_SIZE,sliderInput("SPAcov",NULL,min=0,max=1,value=0,step=COVERAGE_STEP_SIZE)),
              column(COVERAGE_INIT_SIZE,textInput("time_SPA_on",NULL,value = 0))
            ),
            #####################################################################
            helpText("If you want to modify the species or simulation's parameters go to the corresponding tab. If 
              you're just looking for fun hit the 'Run Model'  button"
            )
          ),
          mainPanel(
            plotOutput("plotTrajectory"),
            plotOutput("plotDemographics")
          )
        ),
        p("Cite as: VCOM!!! The coolest model ever!!!")
      ),
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Files Output",
        helpText("Select a CSV/XLS file to load the parameters set for the desired scenario. If it is the first time you are 
          using this option a good starting point is to download the 'CSV Parameters Template' and modify it."),
        titlePanel(h1("Export and Download",align="left")),
        fluidRow( 
          column(4,align="center",
           titlePanel(h3("Parameters Files",align="center")), 
           downloadButton("downloadTemplate", 'Download CSV Parameters Template'),
           downloadButton("downloadParameters", 'Download CSV Parameters'),
           downloadButton("downloadTrace", 'Download CSV Trace')
          ),
          column(4,""),
          column(4,align="center",
            titlePanel(h3("Plots",align="center")), 
            #radioButtons("radioFormat",label=h4("Plot Format"),choices=list("JPG"=".jpg","PNG"=".png"),selected="PNG"),
            downloadButton("downloadPlot", 'Download Trajectory Plot')
          )
        ),
        titlePanel(h1("Debugging and Other Super Fun Stuff",align="left")),
        tableOutput("contentsCSV"),
        tableOutput("contentsXLS"),
        tableOutput("contents")
      ),
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Runtime Data",
        titlePanel("Runtime data"),
        tableOutput("IVM_Runtime")
      )
      ###########################################################################
    )
  )
)
#################################################################################