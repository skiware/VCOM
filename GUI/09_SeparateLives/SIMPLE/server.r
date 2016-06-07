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
    # CLICK EVENTS ##############################################################
    observeEvent(input$buttonTest,{
      cat("Button event!\n")
    })
    observeEvent(input$radioSpecies,{
      cat("Radio event!\n")
      MOSQUITO_PARAMETERS <<- switch(input$radioSpecies,
        "GAM"=getAnGambiaeParameters(),
        "ARA"=getAnArabiensisParameters(),
        "FUN"=getAnFunestusParameters()
      )
    })  
    observeEvent(input$sliderTime,{cat("Slider event!\n")})
    #############################################################################
    # INTERVENTIONS_EVENTS ######################################################
    observeEvent(input$OVIcov|input$FOGcov|input$LARcov|input$BIOcov|input$SREcov|input$ITNcov|input$IRScov|input$IVMcov|input$HOUcov|input$ODOcov|input$SREcov,{
        cat("Coverage Slider Event!\n")
    })
    #############################################################################
    # RUN MODEL #################################################################
    observeEvent(input$buttonRun,{
      cat("Button event!\n")
      theta <<- getTheta(speciesSpecificParameters=MOSQUITO_PARAMETERS)
      initState = calculateInitialState(theta)
      IVM_traj <<- runODE(input$sliderTime,1,initState,theta,"lsoda")
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