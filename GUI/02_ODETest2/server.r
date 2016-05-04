# server.R

shinyServer(
  function(input, output) {
    #ACTIONS TO BE PERFORMED WHEN RANGE CHANGES AND IMPACTS text1
    output$text1 <- renderText({
      paste("You have selected", input$var)
    })
    #ACTIONS TO BE PERFORMED WHEN RANGE CHANGES AND IMPACTS text2
    output$text2 <- renderText({paste("You have chosen a range that goes from", input$range[1], "to", input$range[2])})
    #ACTIONS TO BE PERFORMED WHEN BUTTON IS ACTIVATED
    observeEvent(input$testButton,{
      cat("Showing: ", runif(1,0,1), "\n")
    })
#     observe({
#       #REACTIVE DEPENDENCY
#       isolate({
#         #NON REACTIVE DEPENDENCY
#       })
#     })
  }
)

#outputOptions(output)