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
importedFile = NULL
BOXES_WIDTH <<- "75px"
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
    titlePanel(h1("VCOM: Expert",align="center")),
    titlePanel(h4("Vector Control Optimization Model",align="center")),
    navbarPage("",id="nav",
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Main",
        sidebarLayout(
          sidebarPanel(
            titlePanel(h4("Instructions",align="left")),
            helpText("(1) Select the number of days to simulate, (2) import the file that contains the simulation setup and (3) hit the 'Run Model' button."),
            sliderInput("sliderTime","1. Days to Simulate:",min=1,max=365,value=80),
            fileInput('fileImport','2. Import CSV/XLS Parameters File',accept=c('.xls','.csv')),
            textOutput("importedMessage"),
            actionButton("buttonRun","3. Run Model",width="100%")
          ),
          mainPanel(
            plotOutput("plotTrajectory")
            #plotOutput("plotDemographics")
          )
        ),
        p("Cite as: VCOM!!! The coolest model ever!!!"),
        helpText("CSS theme used with MIT licence (http://bootswatch.com)"),
        helpText("Contacts: <Model: Samson.Kiware@ucsf.edu> <GUI: sanchez.hmsc@itesm.mx>")
      ),
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("More Plots",
        mainPanel(
          plotOutput("plotDemographics")
        )
      ),
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Files Output",
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
