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
initState = calculateInitialState(theta)
IVM_traj = runODE(80,1,initState,theta,"lsoda")  
###############################################################################  
mosquitoParametersTable=read.csv("www/ODEMosquitoParameters.csv",header=FALSE)
controlMeasuresParametersTable=read.csv("www/ODEControlMeasuresParameters.csv",header=FALSE)
mosquitoParametersTable = read.csv("www/ODEMosquitoParameters.csv",header=FALSE)
transmissionParametersTable = read.csv("www/ODETransmissionParameters.csv",header=FALSE)
#---# MAIN SHINY SERVER APPLICATION ###########################################
shinyServer(
  function(input,output,session){
  #############################################################################
  # PARAMETER TABLES ##########################################################
  output$mosquitoParametersTable = renderDataTable(mosquitoParametersTable,options=list(searching=FALSE,paging = FALSE))
  output$controlMeasuresParametersTable = renderDataTable(controlMeasuresParametersTable,options=list(searching=FALSE,paging = FALSE))
  output$transmissionParametersTable = renderDataTable(transmissionParametersTable,options=list(searching=FALSE,paging = FALSE))
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
    #print(input$radioSpecies)
    #if(input$radioSpecies=="GAM"){theta<<-TEMPLATE_AN_GAMBIAE}
    #if(input$radioSpecies=="ARA"){theta<<-TEMPLATE_AN_ARABIENSIS}
    #if(input$radioSpecies=="FUN"){theta<<-TEMPLATE_AN_FUNESTUS}
    print(theta)
  })  
  observeEvent(input$checkboxesControlMeasures,{
    cat("Checkbox event!\n")
  })
  observeEvent(input$sliderTime,{
    cat("Slider event!\n")
  })
  observeEvent(input$buttonRun,{
    cat("Button event!\n")
    IVM_traj <<- runODE(input$sliderTime,1,initState,theta,"lsoda")
    output$IVM_Runtime = renderTable(IVM_traj)
    output$plotDemographics = renderPlot({barChartMosquitoDemographics(IVM_traj)})
    output$plotTrajectory = renderPlot({plotTrajectory(IVM_traj)})
    #print(IVM_traj)
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
  # IMPORT FILES EVENT HANDLERS ###############################################
  output$contentsCSV <- renderTable({
    inFile <- input$csvImport
    if(is.null(inFile)){return(NULL)}
    importCSVParametersFromDirectory(inFile$datapath)
  })
  output$contentsXLS <- renderTable({
    inFile <- input$xlsImport
    print(input$xlsImport[["type"]])
    if(is.null(inFile)){return(NULL)}
    importXLSParametersFromDirectory(inFile$datapath)
  })
  #############################################################################
})
#---###########################################################################

#"text/csv"
#"application/vnd.ms-excel"