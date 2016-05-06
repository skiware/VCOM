###############################################################################
#LOAD LIBRARIES AND FILES
###############################################################################
library(shiny)
###############################################################################

##### RUN WHEN THE APP IS LAUNCHED ############################################
theta <<- getTheta()
initState <<- calculateInitialState(theta)
IVM_traj <<- runODE(80,1,initState,theta,"lsoda")  

shinyServer(
  function(input,output,session){
#   output$demographics = eventReactive(input$buttonRun,{
#     theta <<- getTheta()
#     initState <<- calculateInitialState(theta)
#     IVM_traj <- runODE(input$maxTime,1,initState,theta,"lsoda")
#     print(tail(IVM_traj))
#     paste(runif(1,0,1), "::",input$maxTime)
#     #paste("Model Was Run!!!",IVM_traj[[2]])
#     #output$barChartPlot = renderPlot({barChartMosquitoDemographics(IVM_traj)})
#   })
    
  output$barChartPlot = renderPlot({
    plotTrajectory(IVM_traj)
  })
  
  # DOWNLOAD TEMPLATE HANDLER #################################################
  output$downloadData <- downloadHandler(
    filename <- function(){paste("VCOM_SimSetupFile", "xls", sep=".")},
    content <- function(file){file.copy("SETUP_MosquitoLifeCycleParameters.xls", file)}
  )
})
###############################################################################