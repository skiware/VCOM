#------------------------------------------------------------------------------#
################################################################################
## Malaria vector ODE model GUI                                               ##
## Hector M. Sanchez C. (sanchez.hmsc@itesm.mx)                               ##
## 02/May/2016                                                                ##
################################################################################
#------------------------------------------------------------------------------#

###############################################################################
#LOAD LIBRARIES AND FILES
###############################################################################
library(shiny)
###############################################################################
#---# RUN WHEN THE APP IS LAUNCHED ############################################
#BOXES_WIDTH <<- "100px"
initState <<- calculateInitialState(theta)
IVM_traj <<- runODE(80,1,initState,theta,"lsoda")  
mosquitoParametersTable=read.csv("www/ODEMosquitoParameters.csv",header=FALSE)
controlMeasuresParametersTable=read.csv("www/ODEControlMeasuresParameters.csv",header=FALSE)
mosquitoParametersTable = read.csv("www/ODEMosquitoParameters.csv",header=FALSE)
transmissionParametersTable = read.csv("www/ODETransmissionParameters.csv",header=FALSE)
#---# MAIN SHINY SERVER APPLICATION ###########################################
shinyServer(
  function(input,output,session){
  # VARIOUS TESTS #############################################################
  #   output$demographics = eventReactive(input$buttonRun,{
  #     theta <<- getTheta()
  #     initState <<- calculateInitialState(theta)
  #     IVM_traj <- runODE(input$maxTime,1,initState,theta,"lsoda")
  #     print(tail(IVM_traj))
  #     paste(runif(1,0,1), "::",input$maxTime)
  #     #paste("Model Was Run!!!",IVM_traj[[2]])
  #     #output$barChartPlot = renderPlot({barChartMosquitoDemographics(IVM_traj)})
  #   })
  #############################################################################
  # PARAMETER TABLES ##########################################################
  output$mosquitoParametersTable = renderDataTable(mosquitoParametersTable,options=list(searching=FALSE,paging = FALSE))
  output$controlMeasuresParametersTable = renderDataTable(controlMeasuresParametersTable,options=list(searching=FALSE,paging = FALSE))
  output$transmissionParametersTable = renderDataTable(transmissionParametersTable,options=list(searching=FALSE,paging = FALSE))
  #############################################################################
  # PLOTS #####################################################################
  output$barChartPlot = renderPlot({plotTrajectory(IVM_traj)})
  #############################################################################
  # DOWNLOAD TEMPLATE HANDLER #################################################
  output$downloadTemplate <- downloadHandler(
    filename <- function(){paste("VCOM_SimSetupFile","xls",sep=".")},
    content <- function(file){file.copy("SETUP_MosquitoLifeCycleParameters.xls",file)}
  )
  #############################################################################
  # CLICK EVENTS ##############################################################
  observeEvent(input$buttonTest,{
    cat("Button event!\n")
  })
  observeEvent(input$radioSpecies,{
    cat("Radio event!\n")
  })  
  observeEvent(input$checkboxesControlMeasures,{
    cat("Checkbox event!\n")
  })
  #############################################################################
  # DOWNLOAD PLOTS EVENT HANDLERS #############################################
  #   plotInput <- reactive({
  #     pdf("plot.pdf", width=100, height=100)
  #     plotTrajectory(IVM_traj)
  #     dev.off()
  #   })  
  #   output$myplot <- renderPlot({plotInput()})
  #   output$pdflink <- downloadHandler(
  #     filename <- "myplot.pdf",
  #     content <- function(file) {
  #       file.copy("plot.pdf", file)
  #     }
  #   )
  #############################################################################
  # IMPORT CSV EVENT HANDLERS #################################################
  output$contents <- renderTable({
    inFile <- input$csvImport
    if(is.null(inFile)){return(NULL)}
    #importedCSVParameters<<-read.csv(inFile$datapath,head=FALSE,sep=",",row.names=1,fill=FALSE,stringsAsFactors=FALSE)
    #validateCSVParameters(importedCSVParameters)
    importCSVParametersFromDirectory(inFile$datapath)
  })
  #############################################################################
})
#---###########################################################################