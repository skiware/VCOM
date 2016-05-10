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
#library(XML)
#library(kulife)
source("ODEModel.R")
source("ODEMosquitoParameters.R")
source("ODEAuxiliaryFunctions.R")
source("ODEControlMeasuresParameters.R")
source("ODETransmissionParameters.R")
################################################################################
BOXES_WIDTH <<- "75px"
# Theta from code----------------------------------
#initialTheta <<- getTheta()
# Theta from setup CSV----------------------------- 
initialParametersValues <<- importCSVParametersFromDirectory("SETUP_MosquitoLifeCycleParameters.csv")
initialTheta <<- parseImportedCSVParameters(initialParametersValues)
print(initialTheta)
################################################################################
shinyUI(
  fluidPage(theme = "bootstrapSpace.css",
    titlePanel(h1("VCOM",align="center")),
    titlePanel(h4("Vector Control Optimization Model",align="center")),
    actionButton("buttonRun","Run Model",width="100%"),
    #actionButton("buttonImport","Import XML Parameters File",width="100%"),
    #actionButton("buttonExport","Export XML Parameters File",width="100%"),
    navbarPage("",id="nav",
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Main",
        sidebarLayout(  
          sidebarPanel(
            #headerPanel(h2("IN")),
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
#             fluidRow( 
#               column(5,
#                 #titlePanel(h3("Effectivity",align="center")),
#                 checkboxInput("OVIBool","Ovitraps",value=FALSE),
#                 checkboxInput("FOGBool","Fogging",value=FALSE),
#                 checkboxInput("LARBool","Larvaciding",value=FALSE),
#                 checkboxInput("BIOBool","Biological Control",value=FALSE),
#                 checkboxInput("SREBool","Source Reduction",value=FALSE),
#                 checkboxInput("IRSBool","Indoor Residual Spraying",value=FALSE)
#               ),
#               column(5,
#                 checkboxInput("ITNBool","Insecticide Treated Nets",value=FALSE),
#                 checkboxInput("IVMBool","Ivermectin",value=FALSE),
#                 checkboxInput("HOUBool","House Modification",value=FALSE),
#                 checkboxInput("ODOBool","Odor Traps",value=FALSE),
#                 checkboxInput("SPABool","Spatial Repellants",value=FALSE)
#               )
#             ),
            helpText("If you want to modify the species or simulation's parameters go to the corresponding tab. If 
              you're just looking for fun hit the 'Run Model'  button"
            )
          ),
          mainPanel(
            #headerPanel(h2("OUT"))       
            #textOutput("demographics"),
            plotOutput("plotTrajectory"),
            plotOutput("plotDemographics")
          )
        ),
        actionButton("buttonTest","TEST!",width="100%"),
        br(),
        br(),
        p("Cite as: VCOM!!! The coolest model ever!!!")
      ),
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Mosquito Parameters",
        sidebarLayout(
          sidebarPanel(
            titlePanel(h2("Parameters Descriptions",align="center")),
            #img(src="DOC_parametersDescription.png",width = 375)  
            dataTableOutput("mosquitoParametersTable")
          ),
          mainPanel(
            titlePanel(h2("Parameters Setup",align="center")),
            fluidRow( 
              column(3,align="center",
                #titlePanel(h3("Effectivity",align="center")),
                textInput("muV","muV",value=initialTheta[["muV"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("Q0","Q0",value=initialTheta[["Q0"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("phiB","phiB",value=initialTheta[["phiB"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("phiI","phiI",value=initialTheta[["phiI"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("beta","beta",value=initialTheta[["beta"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("muEL","muEL",value=initialTheta[["muEL"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("muLL","muLL",value=initialTheta[["muLL"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("durPL","durPL",value=initialTheta[["durPL"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("durPL","durPL",value=initialTheta[["durPL"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("durEV","durEV",value=initialTheta[["durEV"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("gamma","gamma",value=initialTheta[["gamma"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("tau1","tau1",value=initialTheta[["tau1"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("tau2","tau2",value=initialTheta[["tau2"]],placeholder=0.5,width=BOXES_WIDTH)
              ),
              #headerPanel(h2("Parameters Matrix",align="center")),
              column(3,align="center",
                #titlePanel(h3("C",align="center")),
                textInput("rOVI","rOVI",value=initialTheta[["rOVI"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rFOG","rFOG",value=initialTheta[["rFOG"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rLAR","rLAR",value=initialTheta[["rLAR"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rBIO","rBIO",value=initialTheta[["rBIO"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rSRE","rSRE",value=initialTheta[["rSRE"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rIRS","rIRS",value=initialTheta[["rIRS"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rIVM","rIVM",value=initialTheta[["rIVM"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rHOU","rHOU",value=initialTheta[["rHOU"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rODO","rODO",value=initialTheta[["rODO"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("rSPA","rSPA",value=initialTheta[["rSPA"]],placeholder=0.5,width=BOXES_WIDTH)
              ),
              column(3,align="center",
                #titlePanel(h3("Activation Day",align="center")),
                textInput("sOVI","sOVI",value=initialTheta[["sOVI"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sFOG","sFOG",value=initialTheta[["sFOG"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sLAR","sLAR",value=initialTheta[["sLAR"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sBIO","sBIO",value=initialTheta[["sBIO"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sSRE","sSRE",value=initialTheta[["sSRE"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sIRS","sIRS",value=initialTheta[["sIRS"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sIVM","sIVM",value=initialTheta[["sIVM"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sHOU","sHOU",value=initialTheta[["sHOU"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sODO","sODO",value=initialTheta[["sODO"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("sSPA","sSPA",value=initialTheta[["sSPA"]],placeholder=0.5,width=BOXES_WIDTH)
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
              #headerPanel(h2("Parameters Matrix",align="center")),
              column(3,align="center",
                titlePanel(h3("Coverage",align="center")),
                textInput("OVIcov","OVI",value=initialTheta[["OVIcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("FOGcov","FOG",value=initialTheta[["FOGcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("LARcov","LAR",value=initialTheta[["LARcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("BIOcov","BIO",value=initialTheta[["BIOcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("SREcov","SRE",value=initialTheta[["SREcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("IRScov","IRS",value=initialTheta[["IRScov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("ITNcov","ITN",value=initialTheta[["ITNcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("IVMcov","IVM",value=initialTheta[["IVMcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("HOUcov","HOU",value=initialTheta[["HOUcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("ODOcov","ODO",value=initialTheta[["ODOcov"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("SPAcov","SPA",value=initialTheta[["SPAcov"]],placeholder=0.5,width=BOXES_WIDTH)
              ),
              column(3,align="center",
                titlePanel(h3("Init Day",align="center")),
                textInput("time_OVI_on","OVI",value=initialTheta[["time_OVI_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_FOG_on","FOG",value=initialTheta[["time_FOG_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_LAR_on","LAR",value=initialTheta[["time_LAR_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_BIO_on","BIO",value=initialTheta[["time_BIO_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_SRE_on","SRE",value=initialTheta[["time_SRE_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_IRS_on","IRS",value=initialTheta[["time_IRS_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_ITN_on","ITN",value=initialTheta[["time_ITN_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_IVM_on","IVM",value=initialTheta[["time_IVM_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_HOU_on","HOU",value=initialTheta[["time_HOU_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_ODO_on","ODO",value=initialTheta[["time_ODO_on"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("time_SPA_on","SPA",value=initialTheta[["time_SPA_on"]],placeholder=0.5,width=BOXES_WIDTH)
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
            #img(src="DOC_parametersDescription.png",width = 375)
          ),
          mainPanel(
            #titlePanel(h3("Parameters Setup",align="center")),
            fluidRow( 
              #headerPanel(h2("Parameters Matrix",align="center")),
              column(3,align="center",
                textInput("f0","f0",value=initialTheta[["f0"]],placeholder=0.5,width=BOXES_WIDTH),
                textInput("epsilon0","epsilon0",value=initialTheta[["epsilon0"]],placeholder=0.5,width=BOXES_WIDTH)
              ),
              column(2,align="center",
               textInput("iH_eq","iH_eq",value=initialTheta[["iH_eq"]],placeholder=0.5,width=BOXES_WIDTH),
               textInput("NH_eq","NH_eq",value=initialTheta[["NH_eq"]],placeholder=0.5,width=BOXES_WIDTH)
              ),
              column(3,align="center",
                textInput("bv","bV",value=initialTheta[["bV"]],placeholder=0.5,width=BOXES_WIDTH)
              )
            )
          )
        )
      ),
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Files I/O",
        #titlePanel(h3("Input",align="left")),
        fileInput('csvImport', 'Import CSV Parameters File', accept = c('.csv')),
        helpText("Select a CSV file to load the parameters set for the desired scenario. If it is the first time you are 
          using this option a good starting point is to download the 'CSV Parameters Template' and modify it."),
        titlePanel(h3("Output",align="left")),
        downloadButton(NA, 'Download CSV Parameters'),
        downloadButton(NA, 'Download CSV Output'),
        downloadButton("downloadPlot", 'Download Plot'),
        downloadButton("downloadTemplate", 'Download CSV Parameters Template')
      ),
      #-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.
      tabPanel("Runtime Data",
        titlePanel("Runtime data"),
        tableOutput("IVM_Runtime"),
        tableOutput("contents")
      )
      ###########################################################################
    )
  )
)
#################################################################################