########################################################################
#=======================================================================
# ODEAuxiliaryFunctions.R
# Contains the functions that deal with plotting and performing other
# generic kind of actions
#=======================================================================
########################################################################
REQUIRED_PARAMETERS_LIST_GLOBAL = c("beta","muEL","muLL","muPL","durEL","durLL","durPL","durEV",
                                    "gamma","tau1","tau2","muV","Q0","phiB","phiI","rOVI","sOVI","rFOG","sFOG","rLAR","sLAR",
                                    "rBIO","sBIO","rSRE","sSRE","rIRS","sIRS","rITN","sITN","rIVM","sIVM","rHOU","sHOU","rODO",
                                    "sODO","rSPA","sSPA")

plotTrajectory = function(IVM_traj){
  #. plotTrajectory: Plots the evolution of the dynamics of the system
  ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
    geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) + 
    geom_line(aes(y = SV, col = "SV"), size = 1.2) + 
    geom_line(aes(y = EV, col = "EV"), size = 1.2) +
    geom_line(aes(y = IV, col = "IV"), size = 1.2) +
    labs(x = "Time (days)", y = "Number of mosquitoes")
}
barChartMosquitoDemographics = function(IVM_traj){
  #. barChartMosquitoDemographics: Generates a bar chart with the amount of mosquitos in each life stage
  NumMosq <- c(sum(IVM_traj[2]),sum(IVM_traj[3]),sum(IVM_traj[4]),sum(IVM_traj[5]),sum(IVM_traj[6]),sum(IVM_traj[7]))
  barplot(NumMosq , main='', xlab='Time (days)',ylab='Number of Mosquitoes',names.arg=c('EL','LL','PL','SV','EV','IV'))
}
parseImportedCSVParameters = function(inputDataFrame){
  #. parseImportedCSVParameters: Once a csv file has been imported this function converts the data into a theta object
  c(
    beta = inputDataFrame["beta",1],
    muEL = inputDataFrame["muEL",1],
    muLL = inputDataFrame["muLL",1],
    muPL = inputDataFrame["muPL",1],
    durEL = inputDataFrame["durEL",1],
    durLL = inputDataFrame["durLL",1],
    durPL = inputDataFrame["durPL",1],
    durEV = inputDataFrame["durEV",1],
    gamma = inputDataFrame["gamma",1],
    tau1 = inputDataFrame["tau1",1],
    tau2 = inputDataFrame["tau2",1],
    ITNcov = inputDataFrame["ITNcov",1],
    IRScov = inputDataFrame["IRScov",1],
    time_ITN_on = inputDataFrame["time_ITN_on",1],
    time_IRS_on = inputDataFrame["time_IRS_on",1],
    muV = inputDataFrame["mV",1],
    Q0 = inputDataFrame["Q0",1],
    phiB = inputDataFrame["phiB",1],
    phiI = inputDataFrame["phiI",1],
    rITN = inputDataFrame["rITN",1],
    sITN = inputDataFrame["sITN",1],
    rIRS = inputDataFrame["rIRS",1],
    sIRS = inputDataFrame["sIRS",1],
    f0 = inputDataFrame["f0",1],
    epsilon0 = inputDataFrame["epsilon0",1],
    iH_eq = inputDataFrame["iH_eq",1],
    NH_eq = inputDataFrame["NH_eq",1],
    bV = inputDataFrame["bV",1]
  )
}
importCSVParametersFromDirectory = function(fileName){
  #. importCSVParametersFromDirectory: Loads the parameters from a CSV file into the workflow
  inputDataFrame=read.csv(file=fileName,head=FALSE,sep=",",row.names=1,fill=FALSE,stringsAsFactors=FALSE)
  inputDataFrame[,1]=as.numeric(inputDataFrame[,1])
  return(validateCSVParameters(inputDataFrame))
}
validateCSVParameters = function(inputDataFrame){
  #. validateCSVParameters: Verifies that the user has imported a valid CSV file with all the parameters defined
  containsNA=anyNA(inputDataFrame[REQUIRED_PARAMETERS_LIST_GLOBAL,1])
  if(containsNA==FALSE){
    print("FILE IS CORRECT")
    return(inputDataFrame)
  }else{
    print("ERROR IN FILE")
    n = c("YOU MADE A MISTAKE AND SHOULD FEEL ASHAMED!!!") 
    s = c("")
    df = data.frame(n, s)
    return(data.frame(df))
  }
}