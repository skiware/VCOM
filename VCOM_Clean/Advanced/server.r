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
#mosquitoParametersTable=read.csv("Documentation/ODEMosquitoParameters.csv",header=FALSE)
#controlMeasuresParametersTable=read.csv("Documentation/ODEControlMeasuresParameters.csv",header=FALSE)
#mosquitoParametersTable=read.csv("Documentation/ODEMosquitoParameters.csv",header=FALSE)
#transmissionParametersTable=read.csv("Documentation/ODETransmissionParameters.csv",header=FALSE)
#################################################################################
#---# MAIN SHINY SERVER APPLICATION #############################################
shinyServer(
  function(input,output,session){
    #############################################################################
    # PRIMING GUI ###############################################################
    shinyjs::disable("buttonRun")
    shinyjs::disable("downloadCSVTrace"); shinyjs::disable("downloadPlotTrace")
    shinyjs::disable("downloadCSVEIR"); shinyjs::disable("downloadPlotEIR")
    shinyjs::disable("downloadPlotDemographics"); shinyjs::disable("downloadPlotVC")
    shinyjs::disable("downloadPlotHuman"); shinyjs::disable("downloadPlotR0")
    output$debugOutput=renderText("Load a Setup File!")
    output$plotTrajectory=renderPlotly({plotTrajectoryPlotLy(IVM_traj)})
    output$IVM_Runtime=renderTable(IVM_traj)
    output$plotDemographics = renderPlotly({barChartMosquitoDemographicsPlotLy(IVM_traj)})
    #output$plotVC = renderPlotly({plotVCPlotLy(IVM_traj)})
    #output$plotR0 = renderPlotly({plotR0PlotLy(IVM_traj)})
    output$plotEIR = renderPlotly({plotEIRVCR0PlotLy(IVM_traj)})
    output$plotHuman = renderPlotly({plotTrajectoryHumansPlotLy(IVM_traj)})
    #############################################################################
    # PARAMETER TABLES ##########################################################
    output$mosquitoParametersTable=renderDataTable(mosquitoParametersTable,options=list(searching=FALSE,paging = FALSE))
    output$controlMeasuresParametersTable=renderDataTable(controlMeasuresParametersTable,options=list(searching=FALSE,paging = FALSE))
    output$transmissionParametersTable=renderDataTable(transmissionParametersTable,options=list(searching=FALSE,paging = FALSE))
    #############################################################################
    # CLICK EVENTS ##############################################################
    observeEvent(input$sliderTime,{
      cat("Slider event!\n")
      initState = calculateInitialState(theta)
      IVM_traj <<- runODE(input$sliderTime,1,initState,theta,"daspk")
      output$plotTrajectory=renderPlotly({plotTrajectoryPlotLy(IVM_traj)})
      output$IVM_Runtime=renderTable(IVM_traj)
      output$plotDemographics = renderPlotly({barChartMosquitoDemographicsPlotLy(IVM_traj)})
      #output$plotVC = renderPlotly({plotVCPlotLy(IVM_traj)})
      #output$plotR0 = renderPlotly({plotR0PlotLy(IVM_traj)})
      output$plotEIR = renderPlotly({plotEIRVCR0PlotLy(IVM_traj)})
      output$plotHuman = renderPlotly({plotTrajectoryHumansPlotLy(IVM_traj)})
      shinyjs::enable("downloadCSVTrace")
      shinyjs::enable("downloadPlotTrace")
      shinyjs::enable("downloadCSVTrace"); shinyjs::enable("downloadPlotTrace")
      shinyjs::enable("downloadCSVEIR"); shinyjs::enable("downloadPlotEIR")
      shinyjs::enable("downloadPlotDemographics"); shinyjs::enable("downloadPlotVC")
      shinyjs::enable("downloadPlotHuman"); shinyjs::enable("downloadPlotR0")
    })
    # observeEvent(input$buttonRun,{
    #   cat("Button event!\n")
    #   print(theta)
    #   initState = calculateInitialState(theta)
    #   IVM_traj <<- runODE(input$sliderTime,1,initState,theta,"daspk")
    #   output$plotTrajectory=renderPlotly({plotTrajectoryPlotLy(IVM_traj)})
    #   output$IVM_Runtime=renderTable(IVM_traj)
    #   output$plotDemographics = renderPlotly({barChartMosquitoDemographicsPlotLy(IVM_traj)})
    #   #output$plotVC = renderPlotly({plotVCPlotLy(IVM_traj)})
    #   #output$plotR0 = renderPlotly({plotR0PlotLy(IVM_traj)})
    #   output$plotEIR = renderPlotly({plotEIRVCR0PlotLy(IVM_traj)})
    #   output$plotHuman = renderPlotly({plotTrajectoryHumansPlotLy(IVM_traj)})
    #   shinyjs::enable("downloadCSVTrace")
    #   shinyjs::enable("downloadPlotTrace")
    #   shinyjs::enable("downloadCSVTrace"); shinyjs::enable("downloadPlotTrace")
    #   shinyjs::enable("downloadCSVEIR"); shinyjs::enable("downloadPlotEIR")
    #   shinyjs::enable("downloadPlotDemographics"); shinyjs::enable("downloadPlotVC")
    #   shinyjs::enable("downloadPlotHuman"); shinyjs::enable("downloadPlotR0")
    #   #print(IVM_traj)
    # })
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
        shinyjs::disable("downloadCSVTrace"); shinyjs::disable("downloadPlotTrace")
        shinyjs::disable("downloadCSVEIR"); shinyjs::disable("downloadPlotEIR")
        shinyjs::disable("downloadPlotDemographics"); shinyjs::disable("downloadPlotVC")
        shinyjs::disable("downloadPlotHuman"); shinyjs::disable("downloadPlotR0")
        debugLoadText="ERROR IN SETUP FILE. Please see 'Loaded Parameters' tab for a list of the parameters with error."
      }else{
        print(theta)
        initState = calculateInitialState(theta)
        IVM_traj <<- runODE(input$sliderTime,1,initState,theta,"daspk")
        output$plotTrajectory=renderPlotly({plotTrajectoryPlotLy(IVM_traj)})
        output$IVM_Runtime=renderTable(IVM_traj)
        output$plotDemographics = renderPlotly({barChartMosquitoDemographicsPlotLy(IVM_traj)})
        #output$plotVC = renderPlotly({plotVCPlotLy(IVM_traj)})
        #output$plotR0 = renderPlotly({plotR0PlotLy(IVM_traj)})
        output$plotEIR = renderPlotly({plotEIRVCR0PlotLy(IVM_traj)})
        output$plotHuman = renderPlotly({plotTrajectoryHumansPlotLy(IVM_traj)})
        shinyjs::enable("downloadCSVTrace")
        shinyjs::enable("downloadPlotTrace")
        shinyjs::enable("downloadCSVTrace"); shinyjs::enable("downloadPlotTrace")
        shinyjs::enable("downloadCSVEIR"); shinyjs::enable("downloadPlotEIR")
        shinyjs::enable("downloadPlotDemographics"); shinyjs::enable("downloadPlotVC")
        shinyjs::enable("downloadPlotHuman"); shinyjs::enable("downloadPlotR0")
      }
      output$debugOutput=renderText(debugLoadText)
      printDFFiltered=df[which(!rownames(df) %in% DISALLOWED_HEADS),]
      output$fileContents<-renderTable({printDFFiltered},include.headnames=FALSE)
    })
    #############################################################################
    # DOWNLOADS HANDLERS ########################################################
    output$downloadTemplate <- downloadHandler(
      filename <- function(){paste("VCOM_SimSetupFile","xls",sep=".")},
      content <- function(file){file.copy("SetupTemplates/SETUP_MosquitoLifeCycleParameters.xls",file)}
    )
    output$downloadCSVTrace <- downloadHandler(
      filename <- function(){paste("VCOM_Trace","csv",sep=".")},
      content <- function(file){
        write.csv(IVM_traj,file)
      }
    )
    output$downloadPlotTrace <- downloadHandler(
      filename = function(){paste(input$dataset,'TrajectoryPlot.png',sep='')},
      content = function(file){
        device <- function(...,width,height){grDevices::png(...,width=2*width,height=height,res=300,units="in")}
        ggsave(file,plot=plotTrajectory(IVM_traj),device=device)
      }
    )
    output$downloadPlotEIR <- downloadHandler(
      filename = function(){paste(input$dataset,'EIRPlot.png',sep='')},
      content = function(file){
        device <- function(...,width,height){grDevices::png(...,width=2*width,height=height,res=300,units="in")}
        ggsave(file,plot=plotEIR(IVM_traj),device=device)
      }
    )
    output$downloadPlotVC <- downloadHandler(
      filename = function(){paste(input$dataset,'VC.png',sep='')},
      content = function(file){
        device <- function(...,width,height){grDevices::png(...,width=2*width,height=height,res=300,units="in")}
        ggsave(file,plot=plotVC(IVM_traj),device=device)
      }
    )
    output$downloadPlotHuman <- downloadHandler(
      filename = function(){paste(input$dataset,'Human.png',sep='')},
      content = function(file){
        device <- function(...,width,height){grDevices::png(...,width=2*width,height=height,res=300,units="in")}
        ggsave(file,plot=plotTrajectoryHumans(IVM_traj),device=device)
      }
    )
    output$downloadPlotR0 <- downloadHandler(
      filename = function(){paste(input$dataset,'R0.png',sep='')},
      content = function(file){
        device <- function(...,width,height){grDevices::png(...,width=2*width,height=height,res=300,units="in")}
        ggsave(file,plot=plotR0(IVM_traj),device=device)
      }
    )
    output$downloadPlotDemographics <- downloadHandler(
      filename = function(){paste(input$dataset,'Demographics.png',sep='')},
      content = function(file){
        device <- function(...,width,height){grDevices::png(...,width=2*width,height=height,res=300,units="in")}
        ggsave(file,plot=barChartMosquitoDemographics(IVM_traj),device=device)
      }
    )
    #############################################################################
  })
#---#############################################################################
