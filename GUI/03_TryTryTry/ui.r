###############################################################################
#LOAD LIBRARIES AND FILES
###############################################################################
library(shiny)
source("ODE.R")
###############################################################################
shinyUI(
  fluidPage(theme = "bootstrapOrange.css",
    titlePanel(h1("VCOM",align="center")),
    titlePanel(h4("Vector Control Optimization Model",align="center")),
    #actionButton("buttonImport","Import XML Parameters File",width="100%"),
    #actionButton("buttonExport","Export XML Parameters File",width="100%"),
    navbarPage("",id="nav",
      ###########################################################################
      tabPanel("Main",
        sidebarLayout(  
          sidebarPanel(
            headerPanel(h2("IN")),
            sliderInput("maxTime","Days to Simulate:",min=1,max=365,value=80),
            helpText("If you want to modify the species or simulation's parameters go to the corresponding tab. If 
              you're just looking for fun hit the 'Run Model'  button"
            ),
            br(),
            br(),
            br(),
            br(),
            fileInput('xmlImport', 'Import XML Parameters File', accept = c('xml')),
            downloadButton(0, 'Download XML Parameters File')
          ),
          mainPanel(
            headerPanel(h2("OUT"))             
          )
        ),
        actionButton("buttonRun","Run Model",width="100%")
      ),
      ###########################################################################
      tabPanel("Mosquito Parameters",
        titlePanel(h2("Mosquito Parameters",align="center"))
      ),
      ###########################################################################
      tabPanel("Control Measures",
        fluidRow(
          column(2,align="center",
            titlePanel(h3("Coverage",align="center")),
            textInput("OVIcov","OVI",value="",placeholder=0.5,width="50px"),
            textInput("FOGCov","FOG",value="",placeholder=0.5,width="50px"),
            textInput("LARCov","LAR",value="",placeholder=0.5,width="50px"),
            textInput("BIOCov","BIO",value="",placeholder=0.5,width="50px"),
            textInput("SRECov","SRE",value="",placeholder=0.5,width="50px"),
            textInput("IRSCov","IRS",value="",placeholder=0.5,width="50px"),
            textInput("ITNCov","ITN",value="",placeholder=0.5,width="50px"),
            textInput("IVMCov","IVM",value="",placeholder=0.5,width="50px"),
            textInput("HOUCov","HOU",value="",placeholder=0.5,width="50px"),
            textInput("ODOCov","ODO",value="",placeholder=0.5,width="50px"),
            textInput("SPACov","SPA",value="",placeholder=0.5,width="50px")
          ),
          column(2,align="center",
            titlePanel(h3("Activation Day",align="center")),
            textInput("time_OVI_on","OVI",value="",placeholder=0.5,width="50px"),
            textInput("time_FOG_on","FOG",value="",placeholder=0.5,width="50px"),
            textInput("time_LAR_on","LAR",value="",placeholder=0.5,width="50px"),
            textInput("time_BIO_on","BIO",value="",placeholder=0.5,width="50px"),
            textInput("time_SRE_on","SRE",value="",placeholder=0.5,width="50px"),
            textInput("time_IRS_on","IRS",value="",placeholder=0.5,width="50px"),
            textInput("time_ITN_on","ITN",value="",placeholder=0.5,width="50px"),
            textInput("time_IVM_on","IVM",value="",placeholder=0.5,width="50px"),
            textInput("time_HOU_on","HOU",value="",placeholder=0.5,width="50px"),
            textInput("time_ODO_on","ODO",value="",placeholder=0.5,width="50px"),
            textInput("time_SPA_on","SPA",value="",placeholder=0.5,width="50px")
          ),
          column(2,align="center",
            titlePanel(h3("Effectivity",align="center")),
            textInput("OVIeff","OVI",value="",placeholder=0.5,width="50px"),
            textInput("FOGeff","FOG",value="",placeholder=0.5,width="50px"),
            textInput("LAReff","LAR",value="",placeholder=0.5,width="50px"),
            textInput("BIOeff","BIO",value="",placeholder=0.5,width="50px"),
            textInput("SREeff","SRE",value="",placeholder=0.5,width="50px"),
            textInput("IRSeff","IRS",value="",placeholder=0.5,width="50px"),
            textInput("ITNeff","ITN",value="",placeholder=0.5,width="50px"),
            textInput("IVMeff","IVM",value="",placeholder=0.5,width="50px"),
            textInput("HOUeff","HOU",value="",placeholder=0.5,width="50px"),
            textInput("ODOeff","ODO",value="",placeholder=0.5,width="50px"),
            textInput("SPAeff","SPA",value="",placeholder=0.5,width="50px")
          )
        )
      )
      ###########################################################################
    )
  )
)