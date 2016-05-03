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
    IVM_traj <<- data.frame(dede(
      y=initState,
      times=seq(0,input$maxTime),
      parms=theta,
      func=IVM_ode,
      method="lsoda"
    ))
    #print(IVM_traj)
    NumMosq <<- c(
      sum(IVM_traj[2]),
      sum(IVM_traj[3]),
      sum(IVM_traj[4]),
      sum(IVM_traj[5]),
      sum(IVM_traj[6]),
      sum(IVM_traj[7])
    )
    #print(NumMosq)
  })
  output$barChart = eventReactive(input$plot,{
    output$barChart = renderPlot({barplot(NumMosq , 
      main='An. Gambiae ITN & IRS & Cattle = 0.8', 
      xlab='Time (days)',
      ylab='Number of Mosquitoes',
      names.arg=c('EL','LL','PL','SV','EV','IV'))
    })
  })
})
###############################################################################