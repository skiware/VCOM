########################################################################
#=======================================================================
# ODEAuxiliaryFunctions.R
#    Contains the functions that deal with calling functions to compute model out and plotting different scenarios + performing other actions
#
#

#=======================================================================
########################################################################
REQUIRED_PARAMETERS_LIST_GLOBAL = c("beta","muEL","muLL","muPL","durEL","durLL","durPL","durEV",
                                    "gamma","tau1","tau2","muV","Q0","phiB","phiI","rOVI","sOVI","rFOG","sFOG","rLAR","sLAR",                                     "rBIO","sBIO","rSRE","sSRE","rIRS","sIRS","rITN","sITN","rECS","rECT","dHOU","dIRS","rHOU","sHOU","rODO",
                                    "sODO","rSPA","sSPA")



plotTrajectory = function(IVM_traj){
  #. plotTrajectory: Plots the evolution of the dynamics of the system
  ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
    #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) +
    theme_grey(base_size = 18, base_family = "") +
    geom_line(aes(y = IV, col = "Infected"), size = 1.75) +
    geom_line(aes(y = SV, col = "Suceptible"), size = 1.75) +
    geom_line(aes(y = EV, col = "Exposed"), size = 1.75) +
    ylim(0, NA) +
    labs(x = "Time (days)", y = "Number of mosquitoes")
}

plotTrajectoryMalesMosq = function(IVM_traj){
  #. plotTrajectory: Plots the evolution of the dynamics of the system
  ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
    #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) +
    geom_line(aes(y = Mm, col = "Mm"), size = 1.2) +
    labs(x = "Time (days)", y = "Number of Male Mosquitoes")
}
plotTrajectoryALL = function(IVM_traj){
  #. plotTrajectory: Plots the evolution of the dynamics of the system
  ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
    #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) +
    geom_line(aes(y = EL, col = "Early Instar"), size = 1.2) +
    geom_line(aes(y = LL, col = "Late Instar"), size = 1.2) +
    geom_line(aes(y = PL, col = "Pupae"), size = 1.2) +
    geom_line(aes(y = IV, col = "Infected"), size = 1.2) +
    geom_line(aes(y = SV, col = "Suceptible"), size = 1.2) +
    geom_line(aes(y = EV, col = "Eexposed"), size = 1.2) +
    geom_line(aes(y = EL_LAR, col = "EL_LAR"), size = 1.2) +
    geom_line(aes(y = EL_BIO, col = "EL_BIO"), size = 1.2) +
    geom_line(aes(y = EL_LAR_BIO, col = "EL_LAR_BIO"), size = 1.2) +
    geom_line(aes(y = LL_LAR, col = "LL_LAR"), size = 1.2) +
    geom_line(aes(y = LL_BIO, col = "LL_BIO"), size = 1.2) +
    geom_line(aes(y = LL_LAR_BIO, col = "LL_LAR_BIO"), size = 1.2) +
    labs(x = "Time (days)", y = "Number of mosquitoes") +
    scale_y_log10() +
    theme_bw()
}

plotTrajectoryDEBUG <- function(traj,log=FALSE){
  #. plotTrajectoryDEBUG: Used only to debug
  require(reshape2)
  IVM_melt <- melt(traj,id.vars="time")
  if(log){
    ggplot(data=IVM_melt,aes(x=time,y=value,group=variable,colour=variable,linetype=variable)) +
      geom_line(size=1.15) +
      theme_bw() +
      scale_y_log10()
  } else {
    ggplot(data=IVM_melt,aes(x=time,y=value,group=variable,colour=variable,linetype=variable)) +
      geom_line(size=1.15) +
      theme_bw()
  }
}

plotTrajectoryHumans = function(IVM_traj){
  #. plotTrajectory: Plots the evolution of the dynamics of the system
  ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
    theme_grey(base_size = 18, base_family = "") +
    #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) +
    geom_line(aes(y = IH, col = "Infected"), size = 1.75) +
    geom_line(aes(y = SH, col = "Suceptible"), size = 1.75) +
    ylim(0, NA) +
    labs(x = "Time (days)", y = "Number of humans")
}

plotEIR_1 = function(IVM_traj,EIR_1){
  #. plotEIR_VC_R0: Plots EIR, VC and R0 dynamics of the system
  ggplot(IVM_traj, aes(x = time, y = EIR, color = State)) +
    geom_line(aes(y = EIR_1, col = "EIR"), size = 1.75) +
    labs(x = "Time (days)", y = " EIR")

}

plot_MS_Figures = function(IVM_traj_Control,IVM_traj_LLIN_50,IVM_traj_LLIN_80,IVM_traj_LLIN_90,theta,INITIAL_MODELRUNTIME_VALUE){
  #. plot_MS_Figures - produce some figures for the manuscript


  #Mosquito population
  NV_Control = IVM_traj_Control[["SV"]]+IVM_traj_Control[["EV"]]+IVM_traj_Control[["IV"]]

  NV_LLIN_80 = IVM_traj_LLIN_80[["SV"]]+IVM_traj_LLIN_80[["EV"]]+IVM_traj_LLIN_80[["IV"]]

  NV_LLIN_50 = IVM_traj_LLIN_50[["SV"]]+IVM_traj_LLIN_50[["EV"]]+IVM_traj_LLIN_50[["IV"]]
  NV_LLIN_90 = IVM_traj_LLIN_90[["SV"]]+IVM_traj_LLIN_90[["EV"]]+IVM_traj_LLIN_90[["IV"]]

  #browser()
  #Just Mosquito density
  p <- ggplot(IVM_traj_Control, aes(x = time, y = IVM_traj_Control, color = State)) +
    geom_line(aes(y = NV_Control, col = "Mosquito-Control"), size = 1.2) +
    geom_line(aes(y = NV_LLIN_50, col = "Mosquito-LLIN-50"), size = 1.2) +
    geom_line(aes(y = NV_LLIN_80, col = "Mosquito-LLIN-80"), size = 1.2) +
    geom_line(aes(y = NV_LLIN_90, col = "Mosquito-LLIN-80"), size = 1.2) +
    labs(title = "An. Arabiensis", x = "Time (days)", y = "Number of mosquitoes")
  p <- p + expand_limits(x = 0, y = 0)
  multiplot(p, cols=1)

  #
  p2 <- ggplot(IVM_traj_Control, aes(x = 1:length(IVM_traj_Control[,1]), y = EIR, color = key)) +
    geom_line(aes(y = IVM_traj_Control[["EIR"]] , col = "EIR - Control "), size = 1.2) +
    geom_line(aes(y = IVM_traj_LLIN_50[["EIR"]], col = "EIR - LLIN - 50 "), size = 1.2) +
    geom_line(aes(y = IVM_traj_LLIN_80[["EIR"]], col = "EIR - LLIN - 80"), size = 1.2) +
    geom_line(aes(y = IVM_traj_LLIN_90[["EIR"]], col = "EIR - LLIN - 90"), size = 1.2) +
    labs(title = "An. Arabiensis", x = "Time (days)", y = "EIR")

  multiplot(p2, cols=1)



}
barChart_MS <- function(IVM_traj_Control,IVM_traj_ITN_80,IVM_traj_IRS_80,IVM_traj_ITN_ECS_5080,
                        IVM_traj_ITN_ATSB_5080,IVM_traj_ITN_LAR_5080,IVM_traj_ITN_PPM_LAR_505050
                        ,IVM_traj_LLIN_ECT_PPM_ATSB_50505050){
  
  traj_dat <- data.frame(stage=c("ITN 80%","IRS 80%","ITN 50%, ECT 80%", "ITN 50%, ATSB 80%","ITN 50%, LAR 80%",
                                 "ITN 50%, PPM 50% LAR 50%","ITN 50%, ECT 50%, PPM 50%, ATSB 50%"),
                         value=c(tail(IVM_traj_ITN_80[["EIR"]],1),tail(IVM_traj_IRS_80[["EIR"]],1),tail(IVM_traj_ITN_ECS_5080[["EIR"]],1),
                                 tail(IVM_traj_ITN_ATSB_5080[["EIR"]],1),tail(IVM_traj_ITN_LAR_5080[["EIR"]],1),
                                 tail(IVM_traj_ITN_PPM_LAR_505050[["EIR"]],1), tail(IVM_traj_LLIN_ECT_PPM_ATSB_50505050[["EIR"]],1)))
  traj_dat$stage <- factor(traj_dat$stage,levels=traj_dat$stage)
  
  traj_dat$newx = str_wrap(traj_dat$stage, width = 10)
  
  ggplot(data=traj_dat,aes(newx,value,fill=stage)) +
    geom_bar(stat="identity",colour="black") +
    #scale_y_log10() +
    theme_grey(base_size = 18, base_family = "") +
    #theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
    guides(fill=FALSE) +
    labs(x="Tools",y="EIR")
  #scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
  
  #theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  
}

plotEIR = function(IVM_traj){
  #. plotEIR_VC_R0: Plots EIR, VC and R0 dynamics of the system
   ggplot(IVM_traj, aes(x = time, y = EIR, color = State)) +
    theme_grey(base_size = 18, base_family = "") +
    geom_line(aes(y = IVM_traj[["EIR"]], col = "EIR"), size = 1.75, colour = 'magenta') +
    ylim(0, NA) +
    labs(x = "Time (days)", y = "Entomological Inoculation Rate (EIR)")
}

plotVC = function(IVM_traj){
  #. plotVC
  ggplot(IVM_traj, aes(x = time, y = VC, color = State)) +
    theme_grey(base_size = 18, base_family = "") +
    geom_line(aes(y = IVM_traj[["VC"]], col = "VC"), size = 1.75, colour = 'purple') +
    ylim(0, NA) +
    labs(x = "Time (days)", y = "Vectorial Capacity (VC)")
}

plotR0 = function(IVM_traj){
  #. plotR0: Plots EIR, VC and R0 dynamics of the system
  ggplot(IVM_traj, aes(x = time, y = EIR, color = State)) +
    theme_grey(base_size = 18, base_family = "") +
    geom_line(aes(y = IVM_traj[["R0"]], col = "R0"), size = 1.75, colour = "blue") +
    ylim(0, NA) +
    labs(x = "Time (days)", y = "Basic Reproduction Rate (R0)")
 }

plotEIR_VC_R0 = function(IVM_traj){
  #. plotEIR_VC_R0: Plots EIR, VC and R0 dynamics of the system

  #Mosquito density
  p1 <- ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
    #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) +
    geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) +
    labs(x = "Time (days)", y = "Number of total mosquitoes")

   #Latent Vector
  p2 <- ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
    #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) +
    geom_line(aes(y = IV, col = "IV"), size = 1.2) +
    labs(x = "Time (days)", y = "Number of infectious mosquitoes")
  #IV
  p3 <- ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
    geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) +
    geom_line(aes(y = IV, col = "IV"), size = 1.2) +
    labs(x = "Time (days)", y = "Number of infections, mosquitoes")

 #browser()
  p4 <- ggplot(IVM_traj, aes(x = time, y = EIR, color = State)) +
    geom_line(aes(y = IVM_traj[["EIR"]], col = "EIR"), size = 1.2) +
    labs(x = "Time (days)", y = " EIR")


  p5 <- ggplot(IVM_traj, aes(x = time, y = EIR, color = State)) +
    geom_line(aes(y = IVM_traj[["R0"]], col = "R0"), size = 1.2) +
    labs(x = "Time (days)", y = " R0")


  p6 <-  ggplot(IVM_traj, aes(x = time, y = EIR, color = State)) +
    geom_line(aes(y = IVM_traj[["VC"]], col = "VC"), size = 1.2) +
    labs(x = "Time (days)", y = " VC")


  multiplot(p1, p2, p3, p4,p5,p6, cols=2)
 }




barChartMosquitoDemographics = function(IVM_traj){
  #. barChartMosquitoDemographics: Generates a bar chart with the amount of mosquitos in each life stage
  #NumMosq <- c(sum(IVM_traj[2]),sum(IVM_traj[3]),sum(IVM_traj[4]),sum(IVM_traj[5]),sum(IVM_traj[6]),sum(IVM_traj[7]))
  #Exctract using names
  NumMosq <- c(sum(IVM_traj["EL"]),sum(IVM_traj["LL"]),sum(IVM_traj["PL"]),sum(IVM_traj["SV"]),sum(IVM_traj["EV"]),sum(IVM_traj["IV"]))
  barplot(NumMosq , main='', xlab='Time (days)',ylab='Number of Mosquitoes',names.arg=c('EL','LL','PL','SV','EV','IV'))
}
barChartMosquitoDemographics_slwu <- function(IVM_traj){

  traj_dat <- data.frame(stage=c("Early Instar","Late Instar","Pupae","Suceptible","Exposed","Infected"),
                         sum=c(sum(IVM_traj["EL"]),sum(IVM_traj["LL"]),sum(IVM_traj["PL"]),sum(IVM_traj["SV"]),sum(IVM_traj["EV"]),sum(IVM_traj["IV"])))
  traj_dat$stage <- factor(traj_dat$stage,levels=traj_dat$stage)

  ggplot(data=traj_dat,aes(stage,sum,fill=stage)) +
    geom_bar(stat="identity",colour="black") +
    scale_y_log10() +
    theme_grey(base_size = 18, base_family = "") +
    guides(fill=FALSE) +
    labs(x="Mosquito Stage",y="Number of Mosquitoes")

}
#SK - CHECK WITH HECTOR - Need to rename some of the parameters in the following function
parseImportedCSVParameters = function(inputDataFrame){
  #. parseImportedCSVParameters: Once a csv file has been imported this function converts the data into a theta object
  c(
    beta = inputDataFrame["beta",1],
    muEL = inputDataFrame["muEL",1],muLL = inputDataFrame["muLL",1],muPL = inputDataFrame["muPL",1],
    durEL = inputDataFrame["durEL",1],durLL = inputDataFrame["durLL",1],durPL = inputDataFrame["durPL",1],durEV = inputDataFrame["durEV",1],
    gamma = inputDataFrame["gamma",1],
    tau1 = inputDataFrame["tau1",1],tau2 = inputDataFrame["tau2",1],
    ITNcov = inputDataFrame["ITNcov",1],
    IRScov = inputDataFrame["IRScov",1],
    time_ITN_on = inputDataFrame["time_ITN_on",1],
    time_IRS_on = inputDataFrame["time_IRS_on",1],
    muV = inputDataFrame["muV",1],Q0 = inputDataFrame["Q0",1],
    phiB = inputDataFrame["phiB",1], phiI = inputDataFrame["phiI",1],
    rOVI = inputDataFrame["rOVI",1], sOVI = inputDataFrame["sOVI",1],
    rFOG = inputDataFrame["rFOG",1], sFOG = inputDataFrame["sFOG",1],
    rLAR = inputDataFrame["rLAR",1], sLAR = inputDataFrame["sLAR",1],
    rBIO = inputDataFrame["rBIO",1], sBIO = inputDataFrame["sBIO",1],
    rSRE = inputDataFrame["rSRE",1], sSRE = inputDataFrame["sSRE",1],
    rIRS = inputDataFrame["rIRS",1], sIRS = inputDataFrame["sIRS",1],
    rITN = inputDataFrame["rITN",1], sITN = inputDataFrame["sITN",1],
    rECT = inputDataFrame["rECT",1], sECT = inputDataFrame["sECT",1],
    rEST = inputDataFrame["rEST",1], sEST = inputDataFrame["sEST",1],
    rHOU = inputDataFrame["rHOU",1], sHOU = inputDataFrame["sHOU",1],
    rODO = inputDataFrame["rODO",1], sODO = inputDataFrame["sODO",1],
    rSPA = inputDataFrame["rSPA",1], sSPA = inputDataFrame["sSPA",1],
    f0 = inputDataFrame["f0",1], epsilon0 = inputDataFrame["epsilon0",1],
    iH_eq = inputDataFrame["iH_eq",1], NH_eq = inputDataFrame["NH_eq",1],
    bV = inputDataFrame["bV",1],
    OVIcov=inputDataFrame["OVIcov",1], time_OVI_on=inputDataFrame["time_OVI_on",1],OVIeff=inputDataFrame["OVIeff",1],
    FOGcov=inputDataFrame["FOGcov",1], time_FOG_on=inputDataFrame["time_FOG_on",1],FOGeff=inputDataFrame["FOGeff",1],
    LARcov=inputDataFrame["LARcov",1], time_LAR_on=inputDataFrame["time_LAR_on",1],LAReff=inputDataFrame["LAReff",1],
    BIOcov=inputDataFrame["BIOcov",1], time_BIO_on=inputDataFrame["time_BIO_on",1],BIOeff=inputDataFrame["BIOeff",1],
    SREcov=inputDataFrame["SREcov",1], time_SRE_on=inputDataFrame["time_SRE_on",1],SREeff=inputDataFrame["SREeff",1],
    IRScov=inputDataFrame["IRScov",1], time_IRS_on=inputDataFrame["time_IRS_on",1],IRSeff=inputDataFrame["IRSeff",1],
    ITNcov=inputDataFrame["ITNcov",1], time_ITN_on=inputDataFrame["time_ITN_on",1],ITNeff=inputDataFrame["ITNeff",1],
    ECScov=inputDataFrame["ECScov",1], time_ECS_on=inputDataFrame["time_ECS_on",1],ECSeff=inputDataFrame["ECSeff",1],
    ECTcov=inputDataFrame["ECTcov",1], time_ECT_on=inputDataFrame["time_ECT_on",1],ECTeff=inputDataFrame["ECTeff",1],
    HOUcov=inputDataFrame["HOUcov",1], time_HOU_on=inputDataFrame["time_HOU_on",1],HOUeff=inputDataFrame["HOUeff",1],
    ODOcov=inputDataFrame["ODOcov",1], time_ODO_on=inputDataFrame["time_ODO_on",1],ODOeff=inputDataFrame["ODOeff",1],
    SPAcov=inputDataFrame["SPAcov",1], time_SPA_on=inputDataFrame["time_SPA_on",1],SPAeff=inputDataFrame["SPAeff",1]
  )
}
importCSVParametersFromDirectory = function(fileName){
  #. importCSVParametersFromDirectory: Loads the verified parameters of the simulation from a CSV file into the workflow
  inputDataFrame=read.csv(file=fileName,head=FALSE,sep=",",row.names=1,col.names=c("ID","Value","Description"),fill=FALSE,stringsAsFactors=FALSE)
  inputDataFrame[,1]=as.numeric(inputDataFrame[,1])
  return(validateCSVParameters(inputDataFrame))
}
validateCSVParameters = function(inputDataFrame){
  #. validateCSVParameters: Verifies that the user has imported a valid CSV file with all the parameters defined
  containsNA=anyNA(inputDataFrame[REQUIRED_PARAMETERS_LIST_GLOBAL,1])
  if(containsNA==FALSE){
    print("FILE IS VALID")
    return(inputDataFrame)
  }else{
    print("ERROR IN FILE")
    sub=subset(inputDataFrame,is.na(inputDataFrame))
    return(sub)
  }
}
importXLSParametersFromDirectory = function(fileName){
  #. importXLSParametersFromDirectory: Loads the verified parameters of the simulation from an XLS file into the workflow
  xlsRaw = loadWorkbook(fileName)
  inputDataFrame = readWorksheet(xlsRaw,sheet=1,head=FALSE,startCol=0,rownames=1)
  inputDataFrame[,1]=as.numeric(inputDataFrame[,1])
  return(validateCSVParameters(inputDataFrame))
}
importCSVXLSParametersFromDirectoryShiny = function(fileName,typeString){
  #. importCSVXLSParametersFromDirectoryShiny: Wrapper function that works only within shiny to select the proper import function to use on a given file
  print(typeString)
  if(typeString == "text/csv"){return(importCSVParametersFromDirectory(fileName))}
  if(typeString == "application/vnd.ms-excel"){return(importXLSParametersFromDirectory(fileName))}
}
