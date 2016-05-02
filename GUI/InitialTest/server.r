library(shiny)

shinyServer(function(input,output,session){
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')    
  })
  output$reactiveText = eventReactive(input$runModel,{input$radio})
  output$text1 <- renderText({paste("You have selected", input$radio)})
  output$text2 = renderText(callFunction())
  output$odeOutput = renderText(NumMosq)
  output$barChart = renderPlot({barplot(NumMosq , main='An. Gambiae ITN & IRS & Cattle = 0.8', xlab='Time (days)',ylab='Number of Mosquitoes',
                            names.arg=c('EL','LL','PL','SV','EV','IV'))})
  eventReactive(input$testButton,{
    #callFunction()
    #print("RUUUUUN!!!!!!!!!!")
    #data.frame(dede(y = initState, times = times, parms = theta, func = IVM_ode, method = "lsoda"))
  })
})