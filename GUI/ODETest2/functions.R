source("ODE.R")

callFunction = function(){
  print("Ran function")
  #print(data.frame(dede(y = initState, times = times, parms = theta, func = IVM_ode, method = "lsoda")))
  IVM_traj <- data.frame(dede(y = initState, times = times, parms = theta, func = IVM_ode, method = "lsoda"))
  runif(1, 5.0, 7.5)
}