###############################################################################
#LOAD LIBRARIES AND FILES
###############################################################################
library(shiny)
###############################################################################

##### RUN WHEN THE APP IS LAUNCHED ############################################
theta <<- getTheta()
initState <<- calculateInitialState(theta)
IVM_traj <<- runODE(80,1,initState,theta,"lsoda")  

##### MAIN SHINY SERVER APPLICATION ###########################################
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
  output$downloadTemplate <- downloadHandler(
    filename <- function(){paste("VCOM_SimSetupFile", "xls", sep=".")},
    content <- function(file){file.copy("SETUP_MosquitoLifeCycleParameters.xls", file)}
  )
  
  # DOWNLOAD PLOTS EVENT HANDLERS ############################################
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
  
  # IMPORT CSV EVENT HANDLERS ################################################
  output$contents <- renderTable({
    inFile <- input$csvImport
    if (is.null(inFile))
      return(NULL)
    read.csv(inFile$datapath,header=FALSE,sep=",")
  })
})
###############################################################################