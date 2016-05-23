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
###############################################################################  
mosquitoParametersTable=read.csv("Documentation/ODEMosquitoParameters.csv",header=FALSE)
controlMeasuresParametersTable=read.csv("Documentation/ODEControlMeasuresParameters.csv",header=FALSE)
mosquitoParametersTable = read.csv("Documentation/ODEMosquitoParameters.csv",header=FALSE)
transmissionParametersTable = read.csv("Documentation/ODETransmissionParameters.csv",header=FALSE)
#---# MAIN SHINY SERVER APPLICATION ###########################################
shinyServer(
  function(input,output,session){
  #############################################################################
  # PRIMING GUI ###############################################################  
  output$plotTrajectory = renderPlot({plotTrajectory(IVM_traj)})
  output$IVM_Runtime = renderTable(IVM_traj)
  #############################################################################
  # PARAMETER TABLES ##########################################################
  output$mosquitoParametersTable = renderDataTable(mosquitoParametersTable,options=list(searching=FALSE,paging = FALSE))
  output$controlMeasuresParametersTable = renderDataTable(controlMeasuresParametersTable,options=list(searching=FALSE,paging = FALSE))
  output$transmissionParametersTable = renderDataTable(transmissionParametersTable,options=list(searching=FALSE,paging = FALSE))
  #############################################################################
  # CLICK EVENTS ##############################################################
  observeEvent(input$buttonTest,{
    cat("Button event!\n")
  })
  observeEvent(input$radioSpecies,{
    cat("Radio event!\n")
    #print(theta[["time_IRS_on"]])
    theta <<- switch(input$radioSpecies,
      "GAM" = getTheta(speciesSpecificParameters=getAnGambiaeParameters()),#getTheta(parseImportedCSVParameters(TEMPLATE_AN_GAMBIAE)),
      "ARA" = getTheta(speciesSpecificParameters=getAnArabiensisParameters()),#getTheta(parseImportedCSVParameters(TEMPLATE_AN_ARABIENSIS)),
      "FUN" = getTheta(speciesSpecificParameters=getAnFunestusParameters()),#getTheta(parseImportedCSVParameters(TEMPLATE_AN_FUNESTUS))
      "USD" = getTheta()
    )
    print(theta)
    #print(initState)
  })  
  observeEvent(input$checkboxesControlMeasures,{
    cat("Checkbox event!\n")
  })
  observeEvent(input$sliderTime,{
    cat("Slider event!\n")
  })
  observeEvent(input$buttonRun,{
    cat("Button event!\n")
    print(theta)
    initState = calculateInitialState(theta)
    IVM_traj <<- runODE(input$sliderTime,1,initState,theta,"lsoda")
    output$IVM_Runtime = renderTable(IVM_traj)
    output$plotDemographics = renderPlot({barChartMosquitoDemographics(IVM_traj)})
    output$plotTrajectory = renderPlot({plotTrajectory(IVM_traj)})
    #print(IVM_traj)
  })
  #############################################################################
  # IMPORT FILES EVENT HANDLERS ###############################################
  #   output$contentsCSV <- renderTable({
  #     inFile <- input$csvImport
  #     if(is.null(inFile)){return(NULL)}
  #     importCSVParametersFromDirectory(inFile$datapath)
  #   })
  #   output$contentsXLS <- renderTable({
  #     inFile <- input$xlsImport
  #     if(is.null(inFile)){return(NULL)}
  #     importXLSParametersFromDirectory(inFile$datapath)
  #   })
  output$contents <- renderTable({
    inFile <- input$fileImport
    if(is.null(inFile)){return(NULL)}
    importedFile=importCSVXLSParametersFromDirectoryShiny(inFile$datapath,input$fileImport[["type"]])
    #theta<<-getTheta(parseImportedCSVParameters(importedFile))
    #print(theta)
    #importedFile
  })
  #############################################################################
  # DOWNLOADS HANDLERS ########################################################
  output$downloadParameters <- downloadHandler(
    filename <- function(){paste("VCOM_Parameters","csv",sep=".")},
    content <- function(file){
      df=data.frame(theta)
      df$Description = 0
      names(df) = c("Value","Description")
      write.csv(df,file)
      #write.csv(cbind(theta,NewColumn=0),file)
    }
  )
  output$downloadTemplate <- downloadHandler(
    filename <- function(){paste("VCOM_SimSetupFile","xls",sep=".")},
    content <- function(file){file.copy("SETUP_MosquitoLifeCycleParameters.xls",file)}
  )
  output$downloadTrace <- downloadHandler(
    filename <- function(){paste("VCOM_Trace","csv",sep=".")},
    content <- function(file){
      write.csv(IVM_traj,file)
    }
  )
  #############################################################################
})
#---###########################################################################
