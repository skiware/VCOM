###############################################################################
#LOAD LIBRARIES AND FILES
###############################################################################
library(shiny)
###############################################################################
shinyServer(function(input,output,session){
  output$reactive = eventReactive(input$testButton,{
    NumMosq
  })
  output$recalculate = eventReactive(input$runModel,{
    IVM_traj = data.frame(dede(y=initState,times=times,parms=theta,func=IVM_ode,method="lsoda"))
    print(IVM_traj)
  })
  output$replot = eventReactive(input$plot,{
    1
  })
})
###############################################################################