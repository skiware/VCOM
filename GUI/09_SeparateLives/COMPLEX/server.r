#------------------------------------------------------------------------------##
#################################################################################
## Malaria vector ODE model GUI                                               ###
## Hector M. Sanchez C. (sanchez.hmsc@itesm.mx)                               ###
## 02/May/2016                                                                ###
#################################################################################
#------------------------------------------------------------------------------##

#################################################################################
#LOAD LIBRARIES AND FILES
#################################################################################
library(shiny)
#################################################################################
#---# RUN WHEN THE APP IS LAUNCHED ##############################################
#BOXES_WIDTH <<- "100px"
#################################################################################
mosquitoParametersTable=read.csv("Documentation/ODEMosquitoParameters.csv",header=FALSE)
controlMeasuresParametersTable=read.csv("Documentation/ODEControlMeasuresParameters.csv",header=FALSE)
mosquitoParametersTable=read.csv("Documentation/ODEMosquitoParameters.csv",header=FALSE)
transmissionParametersTable=read.csv("Documentation/ODETransmissionParameters.csv",header=FALSE)
#################################################################################
#---# MAIN SHINY SERVER APPLICATION #############################################
shinyServer(
  function(input,output,session){
    #############################################################################
    # PRIMING GUI ###############################################################
    shinyjs::disable("buttonRun")
    shinyjs::disable("downloadTrace")
    shinyjs::disable("downloadPlot")
    output$plotTrajectory=renderPlot({plotTrajectory(IVM_traj)})
    output$IVM_Runtime=renderTable(IVM_traj)
    output$plotDemographics=renderPlot({barChartMosquitoDemographics(IVM_traj)})
    output$debugOutput=renderText("Load setup file for the 'Run Model' button to be activated.")
    #############################################################################
    # PARAMETER TABLES ##########################################################
    output$mosquitoParametersTable=renderDataTable(mosquitoParametersTable,options=list(searching=FALSE,paging = FALSE))
    output$controlMeasuresParametersTable=renderDataTable(controlMeasuresParametersTable,options=list(searching=FALSE,paging = FALSE))
    output$transmissionParametersTable=renderDataTable(transmissionParametersTable,options=list(searching=FALSE,paging = FALSE))
    #############################################################################
    # CLICK EVENTS ##############################################################
    observeEvent(input$sliderTime,{cat("Slider event!\n")})
    observeEvent(input$buttonRun,{
      cat("Button event!\n")
      print(theta)
      initState = calculateInitialState(theta)
      IVM_traj <<- runODE(input$sliderTime,1,initState,theta,"lsoda")
      output$IVM_Runtime = renderTable(IVM_traj)
      output$plotDemographics = renderPlot({barChartMosquitoDemographics(IVM_traj)})
      output$plotTrajectory = renderPlot({plotTrajectory(IVM_traj)})
      shinyjs::enable("downloadTrace")
      shinyjs::enable("downloadPlot")
      #print(IVM_traj)
    })
    #############################################################################
    # IMPORT FILES EVENT HANDLERS ###############################################
    observeEvent(input$fileImport,{
      inFile <- input$fileImport
      if(is.null(inFile)){return(NULL)}
      importedFile<<-importCSVXLSParametersFromDirectoryShiny(inFile$datapath,input$fileImport[["type"]])
      theta<<-parseImportedCSVParameters(importedFile)
      print(theta)
      output$contents=renderTable({importedFile})
      output$importedMessage=renderText({"Imported File"})
      df=importedFile
      names(df) <- NULL
      nasNumber=sum(is.na(theta))
      #print(nasNumber)
      debugLoadText="SETUP FILE LOADED CORRECTLY!"
      shinyjs::enable("buttonRun")
      if(nasNumber > NAS_ALLOWED){
        shinyjs::disable("buttonRun")
        shinyjs::disable("downloadTrace")
        shinyjs::disable("downloadPlot")
        debugLoadText="ERROR IN SETUP FILE. Please see 'Loaded Parameters' tab for more information."
      }
      output$debugOutput=renderText(debugLoadText)
      output$fileContents<-renderTable({df})
    })
    #############################################################################
    # DOWNLOADS HANDLERS ########################################################
    output$downloadTemplate <- downloadHandler(
      filename <- function(){paste("VCOM_SimSetupFile","xls",sep=".")},
      content <- function(file){file.copy("SetupTemplates/SETUP_VCOM.xls",file)}
    )
    output$downloadTrace <- downloadHandler(
      filename <- function(){paste("VCOM_Trace","csv",sep=".")},
      content <- function(file){
        write.csv(IVM_traj,file)
      }
    )
    output$downloadPlot <- downloadHandler(
      filename = function(){paste(input$dataset, 'TrajectoryPlot.png', sep='')},
      #filename = function(){paste(input$dataset, input$radioFormat, sep='')},
      content = function(file) {
        device <- function(...,width,height){grDevices::png(...,width=width,height=height,res=300,units="in")}
        ggsave(file, plot = plotTrajectory(IVM_traj), device = device)
      }
    )
    #############################################################################
  })
#---#############################################################################
