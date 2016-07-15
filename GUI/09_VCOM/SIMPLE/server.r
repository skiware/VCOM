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
    shinyjs::disable("downloadTrace")
    shinyjs::disable("downloadPlot")
    output$plotTrajectory=renderPlot({plotTrajectory(IVM_traj)})
    output$IVM_Runtime=renderTable(IVM_traj)
    output$plotDemographics = renderPlot({barChartMosquitoDemographics(IVM_traj)})
    #############################################################################
    # CLICK EVENTS ##############################################################
    observeEvent(input$buttonTest,{cat("Button event!\n")})
    observeEvent(input$radioSpecies,{cat("Radio event!\n")})
    observeEvent(input$sliderTime,{cat("Slider event!\n")})
    #############################################################################
    # INTERVENTIONS_EVENTS ######################################################
    #observeEvent(input$OVIcov|input$FOGcov|input$LARcov|input$BIOcov|input$SREcov|input$ITNcov|input$IRScov|input$IVMcov|input$HOUcov|input$ODOcov|input$SPAcov,{
    #    cat("Coverage Slider Event!\n")
    #})
    #############################################################################
    # RUN MODEL #################################################################
    observeEvent(input$buttonRun,{
      cat("Run button event!\n")
      # CALCULATE THETA ---------------------------------------------------------
      MOSQUITO_PARAMETERS <<- switch(input$radioSpecies,
        "GAM"=getAnGambiaeParameters(),
        "ARA"=getAnArabiensisParameters(),
        "FUN"=getAnFunestusParameters()
      )
      INTERVENTION_PARAMETERS <<- getInterventionsParameters(
        OVIcov=input$OVIcov,FOGcov=input$FOGcov,LARcov=input$LARcov,BIOcov=input$BIOcov,SREcov=input$SREcov,ITNcov=input$ITNcov,
        IRScov=input$IRScov,ECScov=input$ECScov,ECTcov=input$ECTcov,HOUcov=input$HOUcov,ODOcov=input$ODOcov,SPRcov=input$SPRcov,PPMcov=input$PPMcov,
        time_OVI_on=input$time_OVI_on,time_FOG_on=input$time_FOG_on,time_LAR_on=input$time_LAR_on,time_BIO_on=input$time_BIO_on,
        time_SRE_on=input$time_SRE_on,time_ITN_on=input$time_ITN_on,time_IRS_on=input$time_IRS_on,time_ECS_on=input$time_ECS_on,
        time_ECT_on=input$time_ECT_on,time_HOU_on=input$time_HOU_on,time_ODO_on=input$time_ODO_on,time_SPR_on=input$time_SPR_on,
        time_PPM_on=input$time_PPM_on
      )
      theta <<- getTheta(
        speciesSpecificParameters=MOSQUITO_PARAMETERS,
        interventionParameters=INTERVENTION_PARAMETERS
      )
      #--------------------------------------------------------------------------
      print(INTERVENTION_PARAMETERS)
      shinyjs::enable("downloadTrace")
      shinyjs::enable("downloadPlot")
      initState=calculateInitialState(theta)
      IVM_traj<<-runODE(input$sliderTime,1,initState,theta,"lsoda")
      output$plotDemographics=renderPlot({barChartMosquitoDemographics(IVM_traj)})
      output$plotTrajectory=renderPlot({plotTrajectory(IVM_traj)})
    })
    #############################################################################
    # DOWNLOADS HANDLERS ########################################################
    output$downloadParameters <- downloadHandler(
      filename <- function(){paste("VCOM_Parameters","csv",sep=".")},
      content <- function(file){
        df=data.frame(theta)
        df$Description=0
        names(df)=c("Value","Description")
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
      filename = function(){paste(input$dataset,'TrajectoryPlot.png',sep='')},
      content = function(file){
        device <- function(...,width,height){grDevices::png(...,width=width,height=height,res=300,units="in")}
        ggsave(file,plot=plotTrajectory(IVM_traj),device=device)
      }
    )
    #############################################################################
  })
#---#############################################################################
