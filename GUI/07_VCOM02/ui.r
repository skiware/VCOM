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
BOXES_WIDTH<<-"75px"
# Theta from code----------------------------------
#initialTheta <<- getTheta()
# Theta from setup CSV----------------------------- 
initialParametersValues<<-importCSVParametersFromDirectory("SetupTemplates/SETUP_MosquitoLifeCycleParameters.csv")
#print(initialParametersValues)
theta <<- parseImportedCSVParameters(initialParametersValues)
initState <<- calculateInitialState(theta)
print(theta)
################################################################################
TEMPLATE_AN_ARABIENSIS<<-importCSVParametersFromDirectory("SetupTemplates/Template_AnArabiensis.csv")
TEMPLATE_AN_FUNESTUS<<-importCSVParametersFromDirectory("SetupTemplates/Template_AnFunestus.csv")
TEMPLATE_AN_GAMBIAE<<-importCSVParametersFromDirectory("SetupTemplates/Template_AnGambiae.csv")
################################################################################
shinyUI(
  fluidPage(theme = "bootstrapSpace.css",
    titlePanel(h1("VCOM",align="center")),
    titlePanel(h4("Vector Control Optimization Model",align="center")),
    actionButton("buttonRun","Run Model",width="100%"),
    navbarPage("",id="nav",
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Main",
        sidebarLayout(  
          sidebarPanel(
            radioButtons("radioSpecies",label=h4("Species Selection"),
                         choices=list(
                           "An. gambiae"="GAM",
                           "An. arabiensis"="ARA",
                           "An. funestus"="FUN"
                         ),selected="ARA"),
            sliderInput("sliderTime","Days to Simulate:",min=1,max=365,value=80),
            checkboxGroupInput("checkboxesControlMeasures", "Control Measures:",
              c(
                "Ovitraps"="OBIBool",
                "Fogging"="FOGBool",
                "Larvaciding"="LARBool",
                "Biological Control"="BIOBool",
                "Source Reduction"="SREBool",
                "Indoor Residual Spraying"="IRSBool",
                "Insecticide-Treated Nets"="ITNBool",
                "Ivermectin"="IVMBool",
                "House Modifications"="HOUBool",
                "Odor Traps"="ODOBool",
                "Spatial Repellents"="SREBool"
              )
            ),
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
      tabPanel("Mosquito Parameters",
        sidebarLayout(
          sidebarPanel(
            titlePanel(h2("Parameters Descriptions",align="center")), 
            dataTableOutput("mosquitoParametersTable")
          ),
          mainPanel(
            titlePanel(h2("Parameters Setup",align="center")),
            fluidRow( 
              column(3,align="center",
                textInput("muV","muV",value=theta[["muV"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("Q0","Q0",value=theta[["Q0"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("phiB","phiB",value=theta[["phiB"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("phiI","phiI",value=theta[["phiI"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("beta","beta",value=theta[["beta"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("muEL","muEL",value=theta[["muEL"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("muLL","muLL",value=theta[["muLL"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("durPL","durPL",value=theta[["durPL"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("durPL","durPL",value=theta[["durPL"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("durEV","durEV",value=theta[["durEV"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("gamma","gamma",value=theta[["gamma"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("tau1","tau1",value=theta[["tau1"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("tau2","tau2",value=theta[["tau2"]],placeholder=0.5,width=BOXES_WIDTH)
              ),
              column(3,align="center",
                textInput("rOVI","rOVI",value=theta[["rOVI"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rFOG","rFOG",value=theta[["rFOG"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rLAR","rLAR",value=theta[["rLAR"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rBIO","rBIO",value=theta[["rBIO"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rSRE","rSRE",value=theta[["rSRE"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rIRS","rIRS",value=theta[["rIRS"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rIVM","rIVM",value=theta[["rIVM"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rHOU","rHOU",value=theta[["rHOU"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rODO","rODO",value=theta[["rODO"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rSPA","rSPA",value=theta[["rSPA"]],placeholder=0.5,width=BOXES_WIDTH)
              ),
              column(3,align="center",
                textInput("sOVI","sOVI",value=theta[["sOVI"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sFOG","sFOG",value=theta[["sFOG"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sLAR","sLAR",value=theta[["sLAR"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sBIO","sBIO",value=theta[["sBIO"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sSRE","sSRE",value=theta[["sSRE"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sIRS","sIRS",value=theta[["sIRS"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sIVM","sIVM",value=theta[["sIVM"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sHOU","sHOU",value=theta[["sHOU"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sODO","sODO",value=theta[["sODO"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sSPA","sSPA",value=theta[["sSPA"]],placeholder=0.5,width=BOXES_WIDTH)
              )
            )
          )
        )
      ),
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Control Measures Parameters",
        sidebarLayout(
          sidebarPanel(
            headerPanel(h2("Parameters Descriptions",align="center")),
            dataTableOutput("controlMeasuresParametersTable")
          ),
          mainPanel(
            fluidRow( 
              column(3,align="center",
                titlePanel(h3("Coverage",align="center")),
                textInput("OVIcov","OVI",value=theta[["OVIcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("FOGcov","FOG",value=theta[["FOGcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("LARcov","LAR",value=theta[["LARcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("BIOcov","BIO",value=theta[["BIOcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("SREcov","SRE",value=theta[["SREcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("IRScov","IRS",value=theta[["IRScov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("ITNcov","ITN",value=theta[["ITNcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("IVMcov","IVM",value=theta[["IVMcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("HOUcov","HOU",value=theta[["HOUcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("ODOcov","ODO",value=theta[["ODOcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("SPAcov","SPA",value=theta[["SPAcov"]],placeholder=0.5,width=BOXES_WIDTH)
              ),
              column(3,align="center",
                titlePanel(h3("Init Day",align="center")),
                textInput("time_OVI_on","OVI",value=theta[["time_OVI_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_FOG_on","FOG",value=theta[["time_FOG_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_LAR_on","LAR",value=theta[["time_LAR_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_BIO_on","BIO",value=theta[["time_BIO_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_SRE_on","SRE",value=theta[["time_SRE_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_IRS_on","IRS",value=theta[["time_IRS_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_ITN_on","ITN",value=theta[["time_ITN_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_IVM_on","IVM",value=theta[["time_IVM_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_HOU_on","HOU",value=theta[["time_HOU_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_ODO_on","ODO",value=theta[["time_ODO_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_SPA_on","SPA",value=theta[["time_SPA_on"]],placeholder=0.5,width=BOXES_WIDTH)
              ),
              column(3,align="center",
                titlePanel(h3("Effectivity",align="center")),
                textInput("OVIeff","OVI",value=1,placeholder=0.5,width=BOXES_WIDTH),
                textInput("FOGeff","FOG",value=1,placeholder=0.5,width=BOXES_WIDTH),
                textInput("LAReff","LAR",value=1,placeholder=0.5,width=BOXES_WIDTH),
                textInput("BIOeff","BIO",value=1,placeholder=0.5,width=BOXES_WIDTH),
                textInput("SREeff","SRE",value=1,placeholder=0.5,width=BOXES_WIDTH),
                textInput("IRSeff","IRS",value=1,placeholder=0.5,width=BOXES_WIDTH),
                textInput("ITNeff","ITN",value=1,placeholder=0.5,width=BOXES_WIDTH),
                textInput("IVMeff","IVM",value=1,placeholder=0.5,width=BOXES_WIDTH),
                textInput("HOUeff","HOU",value=1,placeholder=0.5,width=BOXES_WIDTH),
                textInput("ODOeff","ODO",value=1,placeholder=0.5,width=BOXES_WIDTH),
                textInput("SPAeff","SPA",value=1,placeholder=0.5,width=BOXES_WIDTH)
              )
            )  
          )
        )
      ),
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Transmission Parameters",
        sidebarLayout(
         sidebarPanel(
            titlePanel(h2("Parameters Descriptions",align="center")),
            dataTableOutput("transmissionParametersTable")
          ),
          mainPanel(
            fluidRow( 
              column(3,align="center",
                textInput("f0","f0",value=theta[["f0"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("epsilon0","epsilon0",value=theta[["epsilon0"]],placeholder=0.5,width=BOXES_WIDTH)
              ),
              column(2,align="center",
               textInput("iH_eq","iH_eq",value=theta[["iH_eq"]],placeholder=0.5,width=BOXES_WIDTH),
               textInput("NH_eq","NH_eq",value=theta[["NH_eq"]],placeholder=0.5,width=BOXES_WIDTH)
              ),
              column(3,align="center",
                textInput("bv","bV",value=theta[["bV"]],placeholder=0.5,width=BOXES_WIDTH)
              )
            )
          )
        )
      ),
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Files I/O",
        titlePanel(h1("Import Parameters Files",align="left")),
        #fileInput('csvImport', 'Import CSV Parameters File', accept = c('.csv')),
        #fileInput('xlsImport', 'Import XLS Parameters File', accept = c('.xls')),
        fileInput('fileImport', 'Import CSV/XLS Parameters File', accept = c('.xls','.csv')),
        helpText("Select a CSV/XLS file to load the parameters set for the desired scenario. If it is the first time you are 
          using this option a good starting point is to download the 'CSV Parameters Template' and modify it."),
        titlePanel(h1("Export and Download",align="left")),
        downloadButton("downloadTemplate", 'Download CSV Parameters Template'),
        #downloadButton(NA, 'Download CSV Parameters'),
        downloadButton("downloadTrace", 'Download CSV Trace'),
        #downloadButton("downloadPlot", 'Download Plot'),
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