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
library(shinyjs)
source("ODEModel.R")
source("ODEMosquitoParameters.R")
source("ODEAuxiliaryFunctions.R")
source("ODEControlMeasuresParameters.R")
source("ODETransmissionParameters.R")
################################################################################
importedFile = NULL
BOXES_WIDTH <<- "75px"
NAS_ALLOWED <<- 11
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
  fluidPage(theme = "bootstrapCerulean.css",
    useShinyjs(),
    titlePanel(h1("VCOM: Expert",align="center"),windowTitle="VCOM: Expert"),
    titlePanel(h4("Vector Control Optimization Model",align="center")),
    navbarPage("",id="nav",
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Main",
        sidebarLayout(
          sidebarPanel(
            titlePanel(h1("Instructions",align="left")),
            helpText("(1) Select the number of days for the simulation to run."),
            helpText("(2) Load the simulation setup file (CSV or XLS format). In case you currently do not have the template download it from the 'Downloads/'"),
            helpText("(3) Run the model!"),
            sliderInput("sliderTime","1. Days to Simulate:",min=1,max=365,value=80),
            fileInput('fileImport','2. Import CSV/XLS Parameters File',accept=c('.xls','.csv')),
            #textOutput("importedMessage"),
            actionButton("buttonRun","3. Run Model",width="100%"),
            titlePanel(h1("Messages",align="left")),
            textOutput("debugOutput")
          ),
          mainPanel(
            plotOutput("plotTrajectory")
            #plotOutput("plotDemographics")
          )
        ),
        helpText("CSS theme downloaded from: http://bootswatch.com (MIT licence)"),
        helpText("Contacts: <Model: Samson.Kiware@ucsf.edu> <GUI: sanchez.hmsc@itesm.mx>")
      ),
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Additional Output",
        mainPanel(
          plotOutput("plotDemographics")
        )
      ),
      tabPanel("Loaded Parameters",
        sidebarLayout(
          sidebarPanel(
            helpText("The table shows the parameters that are currently loaded into the model."),
            helpText("If there is no table shown the model should be running on the default parameters."),
            helpText("Go to 'Main/Import CSV/XLS Parameters File' to load your configuration."),
            helpText("If you do not have a configuration file go to 'Downloads/Download XLS Parameters Table'")
          ),
          mainPanel(
            tableOutput("fileContents")
          )
        )
      ),
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Downloads",
        #titlePanel(h1("Templates",align="left")),
        fluidRow(
          column(4,align="center",
            titlePanel(h3("Parameters Files",align="center")),
            titlePanel(h6("")),
            downloadButton("downloadTemplate", 'Download XLS Parameters Template'),
            #downloadButton("downloadParameters", 'Download Current CSV Parameters'),
            titlePanel(h6("")),
            downloadButton("downloadTrace", 'Download CSV Trace')
          ),
          column(4,""),
          column(4,align="center",
            titlePanel(h3("Plots",align="center")),
            titlePanel(h6("")),
            #radioButtons("radioFormat",label=h4("Plot Format"),choices=list("JPG"=".jpg","PNG"=".png"),selected="PNG"),
            downloadButton("downloadPlot", 'Download Trajectory Plot')
          )
        )
      )#,
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
#       tabPanel("Runtime Data",
#         titlePanel("Runtime data"),
#         tableOutput("IVM_Runtime")
#       )
      ###########################################################################
    )
  )
)
#################################################################################
