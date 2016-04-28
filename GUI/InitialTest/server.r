library(shiny)

shinyServer(function(input,output,session){
  output$distPlot <- renderPlot({
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'darkgray', border = 'white')    
  })
  output$reactiveText = eventReactive(input$runModel,{input$radio})
  output$text1 <- renderText({
    paste("You have selected", input$radio)
  })
})