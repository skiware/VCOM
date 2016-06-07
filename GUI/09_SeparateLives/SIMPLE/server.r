#------------------------------------------------------------------------------##
#################################################################################
## Hector M. Sanchez C. (sanchez.hmsc@itesm.mx)                               ###
## Malaria vector ODE model GUI                                               ###
## 02/May/2016                                                                ###
#################################################################################
#------------------------------------------------------------------------------##

#################################################################################
#---# MAIN SHINY SERVER APPLICATION #############################################
shinyServer(
  function(input,output,session){
    #############################################################################
    # PRIMING GUI ###############################################################  
    output$plotTrajectory=renderPlot({plotTrajectory(IVM_traj)})
    output$IVM_Runtime=renderTable(IVM_traj)
    #############################################################################
    # PARAMETER TABLES ##########################################################
    output$mosquitoParametersTable=renderDataTable(mosquitoParametersTable,options=list(searching=FALSE,paging = FALSE))
    output$controlMeasuresParametersTable=renderDataTable(controlMeasuresParametersTable,options=list(searching=FALSE,paging = FALSE))
    output$transmissionParametersTable=renderDataTable(transmissionParametersTable,options=list(searching=FALSE,paging = FALSE))
    #############################################################################
    # CLICK EVENTS ##############################################################
    observeEvent(input$buttonTest,{
      cat("Button event!\n")
    })
    observeEvent(input$radioSpecies,{
      cat("Radio event!\n")
      theta <<- switch(input$radioSpecies,
        "GAM"=getTheta(speciesSpecificParameters=getAnGambiaeParameters()),
        "ARA"=getTheta(speciesSpecificParameters=getAnArabiensisParameters()),
        "FUN"=getTheta(speciesSpecificParameters=getAnFunestusParameters())
      )
      print(theta)
    })  
    observeEvent(input$sliderTime,{
      cat("Slider event!\n")
    })
    #############################################################################
    # INTERVENTIONS_EVENTS ######################################################
    observeEvent(input$OVIcov|input$FOGcov|input$LARcov|input$BIOcov|input$SREcov|input$ITNcov|input$IRScov|input$IVMcov|input$HOUcov|input$ODOcov|input$SREcov
      #input$time_OVI_on|input$time_FOG_on|input$time_LAR_on|input$time_BIO_on|input$time_SRE_on|input$time_ITN_on|input$time_IRS_on|input$time_IVM_on|input$time_HOU_on|input$time_ODO_on|input$time_SRE_on
      ,{
        cat("Coverage Slider Event!\n")
    })
    #observeEvent(input$time_OVI_on|input$time_LAR_on#|input$time_LAR_on|input$time_BIO_on|input$time_SRE_on|input$time_ITN_on|input$time_IRS_on|input$time_IVM_on|input$time_HOU_on|input$time_ODO_on|input$time_SRE_on
    #  ,{
    #    cat("Coverage Text Event!\n")
    #})
    #############################################################################
    # RUN MODEL #################################################################
    observeEvent(input$buttonRun,{
      cat("Button event!\n")
      print(theta)
      initState = calculateInitialState(theta)
      IVM_traj <<- runODE(input$sliderTime,1,initState,theta,"lsoda")
      output$IVM_Runtime = renderTable(IVM_traj)
      output$plotDemographics = renderPlot({barChartMosquitoDemographics(IVM_traj)})
      output$plotTrajectory = renderPlot({plotTrajectory(IVM_traj)})
    })
    #############################################################################
    # DOWNLOADS HANDLERS ########################################################
    output$downloadParameters <- downloadHandler(
      filename <- function(){paste("VCOM_Parameters","csv",sep=".")},
      content <- function(file){
        print(theta)
        df=data.frame(theta)
        df$Description = 0
        names(df) = c("Value","Description")
        write.csv(df,file)
      }
    )
    output$downloadTemplate <- downloadHandler(
      filename <- function(){paste("VCOM_SimSetupFile","xls",sep=".")},
      content <- function(file){file.copy("SetupTemplates/SETUP_MosquitoLifeCycleParameters.xls",file)}
    )
    output$downloadTrace <- downloadHandler(
      filename <- function(){paste("VCOM_Trace","csv",sep=".")},
      content <- function(file){
        write.csv(IVM_traj,file)
      }
    )
    output$downloadPlot <- downloadHandler(
      filename = function(){paste(input$dataset, 'TrajectoryPlot', sep='')},
      content = function(file){
        device <- function(...,width,height){grDevices::png(...,width=width,height=height,res=300,units="in")}
        ggsave(file, plot = plotTrajectory(IVM_traj), device = device)
      }
    )
    #############################################################################
  })
#---#############################################################################