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
    geom_line(aes(y = IV, col = "IV"), size = 1.2) + 
    geom_line(aes(y = SV, col = "SV"), size = 1.2) +
    geom_line(aes(y = EV, col = "EV"), size = 1.2) +
    labs(x = "Time (days)", y = "Number of mosquitoes")
}

plotTrajectoryMalesMosq = function(IVM_traj){
  #. plotTrajectory: Plots the evolution of the dynamics of the system
  ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
    #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) + 
    geom_line(aes(y = Mm, col = "Mm"), size = 1.2) + 
    labs(x = "Time (days)", y = "Number of Male Mosquitoes")
}

plotTrajectoryHumans = function(IVM_traj){
  #. plotTrajectory: Plots the evolution of the dynamics of the system
  ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
    #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) + 
    geom_line(aes(y = IH, col = "IH"), size = 1.2) + 
    geom_line(aes(y = SH, col = "SH"), size = 1.2) +
    labs(x = "Time (days)", y = "Number of humans")
}


plotEIR_VC_R0 = function(IVM_traj,theta,INITIAL_MODELRUNTIME_VALUE){
  #. plotEIR_VC_R0: Plots EIR, VC and R0 dynamics of the system
  #browser()
  #IS IT OK TO RE-COMPUTE THE parameter values required to compute EIR, VC, R0? This time done at after the ODE run
  # INITIAL_MODELRUNTIME_VALUE - this value will be compared by intervention turn on time (less than INITIAL_MODELRUNTIME_VALUE)
  # This values are re-computed at equilbrium subjected to turned - on intervention
  impactFeedingCycle = impactFeedingCycleParameters(INITIAL_MODELRUNTIME_VALUE, theta[["beta"]], theta[["tau1"]],theta[["tau2"]],theta[["e_ov"]],theta[["time_ATSB_on"]],theta[["ATSBcov"]],theta[["time_SSP_on"]],
                               theta[["SSPcov"]],theta[["fSSP"]],theta[["fATSB"]],theta[["muV"]],theta[["Q0"]],theta[["aOBT"]],theta[["OBTcov"]],theta[["time_OBT_on"]],theta[["time_ITN_on"]],
                               theta[["ITNcov"]],theta[["time_IRS_on"]],theta[["IRScov"]],theta[["HOUcov"]],theta[["time_HOU_on"]],theta[["rITN"]],theta[["sITN"]],theta[["rIRS"]],theta[["rHOU"]],
                               theta[["sIRS"]],  theta[["sHOU"]],theta[["phiB"]], theta[["phiI"]],theta[["dHOU"]],theta[["dIRS"]],theta[["time_SPR_on"]],theta[["SPRcov"]],theta[["time_PPM_on"]],
                               theta[["PPMcov"]],theta[["rSPR"]],theta[["rPPM"]],theta[["sSPR"]],theta[["sPPM"]],theta[["c0"]],theta[["time_ECS_on"]],theta[["ECScov"]],theta[["time_ECT_on"]],
                               theta[["ECTcov"]],theta[["rECT"]],theta[["sECS"]],theta[["sECT"]],theta[["time_OVI_on"]],theta[["OVIcov"]],theta[["fOVI"]])
  # I can also just pass theta for this function
  
  
 

    #mosquito mortality
  muVCom  = impactFeedingCycle[1]
  #Biting rate
  a_theta = impactFeedingCycle[3]
  
  
  
  # #EIR
  transmissionValues = getAdditionalTransmissionParameters()
  bV = transmissionValues[["bV"]]
  NH = transmissionValues[["NH_eq"]]
  bh = transmissionValues[["bh"]]
  bv = transmissionValues[["bV"]]
  IV = IVM_traj[["IV"]]
  
  #Mosquito population
  NV = IVM_traj[["SV"]]+IVM_traj[["EV"]]+IVM_traj[["IV"]]
  #Mosquito density
  Mdensity = NV/NH
  
  
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
  
# browser()
  
  #EIR
  time_on = 20                 #Manually set it to the earliest time an intervention is on - to be automated
  EIR <-computeEIR(time_on,a_theta, IV, NH)
  #EIR <-computeEIR(a_theta, IV, NH)
  
  p4 <- ggplot(IVM_traj, aes(x = 1:length(IVM_traj[,1]), y = EIR, color = State)) +
    geom_line(aes(y = EIR, col = "EIR"), size = 1.2) + 
    #geom_line(aes(y = IVM_traj["EV"], col = "EV"), size = 1.2) +
    labs(x = "Time (days)", y = " EIR")
  
  #VC
 
  
  #R0
  R0 = computeRO(time_on,a_theta,muVCom, NV,bv,bh,NH)
  p5 <- ggplot(IVM_traj, aes(x = 1:length(IVM_traj[,1]), y = EIR, color = State)) +
    geom_line(aes(y = R0, col = "R0"), size = 1.2) + 
    #geom_line(aes(y = IVM_traj["EV"], col = "EV"), size = 1.2) +
    labs(x = "Time (days)", y = " R0")
  
  VC = computeVC(time_on,a_theta, NV,NH,muVCom)
  p6 <-  ggplot(IVM_traj, aes(x = 1:length(IVM_traj[,1]), y = EIR, color = State)) +
    geom_line(aes(y = VC, col = "VC"), size = 1.2) + 
    #geom_line(aes(y = IVM_traj["EV"], col = "EV"), size = 1.2) +
    labs(x = "Time (days)", y = " VC")
  
  
 
  
  #All at once
  
  # #IVM_traj - just space holder
  # # We might need to plot them separately - s
  # p4 <- ggplot(IVM_traj, aes(x = 1:length(IVM_traj[,1]), y = EIR, color = State)) +
  #   #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) + 
  #   geom_line(aes(y = EIR, col = "EIR"), size = 1.2) + 
  #   geom_line(aes(y = VC, col = "VC"), size = 1.2) + 
  #   geom_line(aes(y = R0, col = "R0"), size = 1.2) + 
  #   #geom_line(aes(y = IVM_traj["EV"], col = "EV"), size = 1.2) +
  #   labs(x = "Time (days)", y = "values for EIR, VC, or R0")
  # 
  # 
  multiplot(p1, p2, p3, p4,p5,p6, cols=2)
  
  
  
 }

plot_SA_Figures = function(IVM_traj_Control,IVM_traj_LLIN_60,IVM_traj_LLIN_30,theta,INITIAL_MODELRUNTIME_VALUE){
  #. plot_SA_Figures - produce some figures for presentations
  impactFeedingCycle = impactFeedingCycleParameters(INITIAL_MODELRUNTIME_VALUE, theta[["beta"]], theta[["tau1"]],theta[["tau2"]],theta[["e_ov"]],theta[["time_ATSB_on"]],theta[["ATSBcov"]],theta[["time_SSP_on"]],
                                                    theta[["SSPcov"]],theta[["fSSP"]],theta[["fATSB"]],theta[["muV"]],theta[["Q0"]],theta[["aOBT"]],theta[["OBTcov"]],theta[["time_OBT_on"]],theta[["time_ITN_on"]],
                                                    theta[["ITNcov"]],theta[["time_IRS_on"]],theta[["IRScov"]],theta[["HOUcov"]],theta[["time_HOU_on"]],theta[["rITN"]],theta[["sITN"]],theta[["rIRS"]],theta[["rHOU"]],
                                                    theta[["sIRS"]],  theta[["sHOU"]],theta[["phiB"]], theta[["phiI"]],theta[["dHOU"]],theta[["dIRS"]],theta[["time_SPR_on"]],theta[["SPRcov"]],theta[["time_PPM_on"]],
                                                    theta[["PPMcov"]],theta[["rSPR"]],theta[["rPPM"]],theta[["sSPR"]],theta[["sPPM"]],theta[["c0"]],theta[["time_ECS_on"]],theta[["ECScov"]],theta[["time_ECT_on"]],
                                                    theta[["ECTcov"]],theta[["rECT"]],theta[["sECS"]],theta[["sECT"]],theta[["time_OVI_on"]],theta[["OVIcov"]],theta[["fOVI"]])
  # I can also just pass theta for this function
  
  
  #browser()
  
  #mosquito mortality
  muVCom  = impactFeedingCycle[1]
  #Biting rate
  a_theta = impactFeedingCycle[3]
  
  
  

  transmissionValues = getAdditionalTransmissionParameters()
  bV = transmissionValues[["bV"]]
  NH = transmissionValues[["NH_eq"]]
  bh = transmissionValues[["bh"]]
  bv = transmissionValues[["bV"]]
  #IV = IVM_traj[["IV"]]
  
  #Mosquito population
  NV_Control = IVM_traj_Control[["SV"]]+IVM_traj_Control[["EV"]]+IVM_traj_Control[["IV"]]
  
  NV_LLIN_80 = IVM_traj_LLIN_80[["SV"]]+IVM_traj_LLIN_80[["EV"]]+IVM_traj_LLIN_80[["IV"]]
  
  NV_LLIN_50 = IVM_traj_LLIN_50[["SV"]]+IVM_traj_LLIN_50[["EV"]]+IVM_traj_LLIN_50[["IV"]]
 
  #browser()
  #Just Mosquito density
  p <- ggplot(IVM_traj_Control, aes(x = time, y = IVM_traj_Control, color = State)) +
    geom_line(aes(y = NV_Control, col = "Mosquito-Control"), size = 1.2) + 
    geom_line(aes(y = NV_LLIN_80, col = "Mosquito-LLIN-80"), size = 1.2) +
    geom_line(aes(y = NV_LLIN_50, col = "Mosquito-LLIN-50"), size = 1.2) +
    labs(title = "An. Arabiensis", x = "Time (days)", y = "Number of mosquitoes")
  p <- p + expand_limits(x = 0, y = 0)
  multiplot(p, cols=1)
  
  #IV
  IV_Control = IVM_traj_Control[["IV"]]
  IV_LLIN_80 = IVM_traj_LLIN_80[["IV"]]
  IV_LLIN_50 = IVM_traj_LLIN_50[["IV"]]
  
  
  
   #EIR
   time_on = 20                 #Manually set it to the earliest time an intervention is on - to be automated
   EIR_control <-computeEIR(time_on,a_theta, IV_Control, NH)
   EIR_LLIN_80 <-computeEIR(time_on,a_theta, IV_LLIN_80, NH)
   EIR_LLIN_50 <-computeEIR(time_on,a_theta, IV_LLIN_50, NH)
   #EIR <-computeEIR(a_theta, IV, NH)
  # 
   p2 <- ggplot(IVM_traj_Control, aes(x = 1:length(IVM_traj_Control[,1]), y = EIR, color = key)) +
     geom_line(aes(y = EIR_control , col = "EIR - Control "), size = 1.2) + 
     geom_line(aes(y = EIR_LLIN_80, col = "EIR - LLIN - 80 "), size = 1.2) + 
     geom_line(aes(y = EIR_LLIN_50, col = "EIR - LLIN - 50"), size = 1.2) + 
     labs(title = "An. Arabiensis", x = "Time (days)", y = "EIR")
   
   multiplot(p2, cols=1)
  
  #Mosquito density
  # p1 <- ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
  #   geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) + 
  #   #geom_line(aes(y = IV, col = "IV"), size = 1.2) +
  #   labs(x = "Time (days)", y = "Number of total mosquitoes")
  # 
  # #Mosquito density
  # p2 <- ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
  #   #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) + 
  #   geom_line(aes(y = IV, col = "IV"), size = 1.2) +
  #   labs(x = "Time (days)", y = "Number ofinfectious mosquitoes")
  # 
  # #EIR
  # time_on = 20                 #Manually set it to the earliest time an intervention is on - to be automated
  # EIR <-computeEIR(time_on,a_theta, IV, NH)
  # #EIR <-computeEIR(a_theta, IV, NH)
  # 
  # p3 <- ggplot(IVM_traj, aes(x = 1:length(IVM_traj[,1]), y = EIR, color = State)) +
  #   geom_line(aes(y = EIR, col = "EIR"), size = 1.2) + 
  #   #geom_line(aes(y = IVM_traj["EV"], col = "EV"), size = 1.2) +
  #   labs(x = "Time (days)", y = " EIR")
  # 
  # #VC
  # 
  # 
  # 
  # 
  # 
  # 
  # #All at once
  # 
  # # #IVM_traj - just space holder
  # # # We might need to plot them separately - s
  # # p4 <- ggplot(IVM_traj, aes(x = 1:length(IVM_traj[,1]), y = EIR, color = State)) +
  # #   #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) + 
  # #   geom_line(aes(y = EIR, col = "EIR"), size = 1.2) + 
  # #   geom_line(aes(y = VC, col = "VC"), size = 1.2) + 
  # #   geom_line(aes(y = R0, col = "R0"), size = 1.2) + 
  # #   #geom_line(aes(y = IVM_traj["EV"], col = "EV"), size = 1.2) +
  # #   labs(x = "Time (days)", y = "values for EIR, VC, or R0")
  # # 
  # # 
  # multiplot(p1, p2,p3, cols=1)
  # 
  
  
}

plot_SA_Figures_v2 = function(IVM_traj_Control,IVM_traj_LLIN_80,IVM_traj_LLIN_50_IRS_50,IVM_traj_LLIN_50_LAR_50,IVM_traj_LLIN_50_ECT_50,
                   IVM_traj_LLIN_50_HOU_50,IVM_traj_LLIN_50_PPM_50,IVM_traj_LLIN_50_ATSB_50,theta,INITIAL_MODELRUNTIME_VALUE){

   #. plot_SA_Figures - produce some figures for presentations
  impactFeedingCycle = impactFeedingCycleParameters(INITIAL_MODELRUNTIME_VALUE, theta[["beta"]], theta[["tau1"]],theta[["tau2"]],theta[["e_ov"]],theta[["time_ATSB_on"]],theta[["ATSBcov"]],theta[["time_SSP_on"]],
                                                    theta[["SSPcov"]],theta[["fSSP"]],theta[["fATSB"]],theta[["muV"]],theta[["Q0"]],theta[["aOBT"]],theta[["OBTcov"]],theta[["time_OBT_on"]],theta[["time_ITN_on"]],
                                                    theta[["ITNcov"]],theta[["time_IRS_on"]],theta[["IRScov"]],theta[["HOUcov"]],theta[["time_HOU_on"]],theta[["rITN"]],theta[["sITN"]],theta[["rIRS"]],theta[["rHOU"]],
                                                    theta[["sIRS"]],  theta[["sHOU"]],theta[["phiB"]], theta[["phiI"]],theta[["dHOU"]],theta[["dIRS"]],theta[["time_SPR_on"]],theta[["SPRcov"]],theta[["time_PPM_on"]],
                                                    theta[["PPMcov"]],theta[["rSPR"]],theta[["rPPM"]],theta[["sSPR"]],theta[["sPPM"]],theta[["c0"]],theta[["time_ECS_on"]],theta[["ECScov"]],theta[["time_ECT_on"]],
                                                    theta[["ECTcov"]],theta[["rECT"]],theta[["sECS"]],theta[["sECT"]],theta[["time_OVI_on"]],theta[["OVIcov"]],theta[["fOVI"]])

  

  #mosquito mortality
  muVCom  = impactFeedingCycle[1]
  #Biting rate
  a_theta = impactFeedingCycle[3]
  
  transmissionValues = getAdditionalTransmissionParameters()
  bV = transmissionValues[["bV"]]
  NH = transmissionValues[["NH_eq"]]
  bh = transmissionValues[["bh"]]
  bv = transmissionValues[["bV"]]
  
 
  #Mosquito population
  NV_Control        = IVM_traj_Control[["SV"]]+IVM_traj_Control[["EV"]]+IVM_traj_Control[["IV"]]
  
  NV_LLIN_80        = IVM_traj_LLIN_80[["SV"]]+IVM_traj_LLIN_80[["EV"]]+IVM_traj_LLIN_80[["IV"]]
  
  NV_LLIN_50_IRS_50 = IVM_traj_LLIN_50_IRS_50[["SV"]]+IVM_traj_LLIN_50_IRS_50[["EV"]]+IVM_traj_LLIN_50_IRS_50[["IV"]]
  
  NV_LLIN_50_LAR_50 = IVM_traj_LLIN_50_LAR_50[["SV"]]+IVM_traj_LLIN_50_LAR_50[["EV"]]+IVM_traj_LLIN_50_LAR_50[["IV"]]
  
  NV_LLIN_50_ECT_50 = IVM_traj_LLIN_50_ECT_50[["SV"]]+IVM_traj_LLIN_50_ECT_50[["EV"]]+IVM_traj_LLIN_50_ECT_50[["IV"]]
  
  NV_LLIN_50_HOU_50 = IVM_traj_LLIN_50_HOU_50[["SV"]]+IVM_traj_LLIN_50_HOU_50[["EV"]]+IVM_traj_LLIN_50_HOU_50[["IV"]]
  
  NV_LLIN_50_PPM_50 = IVM_traj_LLIN_50_PPM_50[["SV"]]+IVM_traj_LLIN_50_PPM_50[["EV"]]+IVM_traj_LLIN_50_PPM_50[["IV"]]
  
  NV_LLIN_50_ATSB_50 = IVM_traj_LLIN_50_ATSB_50[["SV"]]+IVM_traj_LLIN_50_ATSB_50[["EV"]]+IVM_traj_LLIN_50_ATSB_50[["IV"]]
  
  
  #browser()
  #Just Mosquito density
  p <- ggplot(IVM_traj_Control, aes(x = time, y = IVM_traj_Control, color = Key)) +
    geom_line(aes(y = NV_Control,        col = "Control"), size = 1.2) + 
    geom_line(aes(y = NV_LLIN_80 ,       col = "LLIN=0.80"), size = 1.2) +
    geom_line(aes(y = NV_LLIN_50_IRS_50, col = "LLIN=0.50 & IRS=0.50"), size = 1.2) +
    geom_line(aes(y = NV_LLIN_50_LAR_50, col = "LLIN=0.50 & LAR=0.50"), size = 1.2) +
    geom_line(aes(y = NV_LLIN_50_ECT_50, col = "LLIN=0.50 & ECT=0.50"), size = 1.2) +
    geom_line(aes(y = NV_LLIN_50_HOU_50, col = "LLIN=0.50 & HOU=0.50"), size = 1.2) +
    geom_line(aes(y = NV_LLIN_50_PPM_50, col = "LLIN=0.50 & PPM=0.50"), size = 1.2) +
    geom_line(aes(y = NV_LLIN_50_ATSB_50,col = "LLIN=0.50 & ATSB=0.50"), size = 1.2) +
    labs(title = "An Funestus", x = "Time (days)", y = "Number of mosquitoes")
  p <- p + expand_limits(x = 0, y = 0)
  multiplot(p, cols=1)
  
  #Bar plots??
  #NumMosq <- c(sum(NV_Control),sum(NV_LLIN_80),sum(NV_LLIN_50_IRS_50),sum(NV_LLIN_50_LAR_50),sum(NV_LLIN_50_ECT_50),sum(NV_LLIN_50_HOU_50),sum(NV_LLIN_50_PPM_50),sum(NV_LLIN_50_ATSB_50))
  
  NumMosq_eq <- c(tail(NV_Control,1),tail(NV_LLIN_80,1),tail(NV_LLIN_50_IRS_50,1),tail(NV_LLIN_50_LAR_50,1),tail(NV_LLIN_50_ECT_50,1),tail(NV_LLIN_50_HOU_50,1),tail(NV_LLIN_50_PPM_50,1),tail(NV_LLIN_50_ATSB_50,1))
  
#  divVal = length(NV_Control)
 # NumMosqAvg <- c(sum(NV_Control)/divVal,sum(NV_LLIN_50)/divVal,sum(NV_cattle_50)/divVal,sum(NV_cc)/divVal,sum(NV_LLIN_HOUSE_50)/divVal,sum(NV_LLIN_OBT_50)/divVal)
  #barplot(NumMosq , main='An. Arabiensis: Mosquito Density', xlab='Time (days)',ylab='Number of Mosquitoes',names.arg=c('Control','BB','CC','DD','EE','FF'))
  
  
  #IV
  
  IV_Control        = IVM_traj_Control[["IV"]]
  
  IV_LLIN_80        = IVM_traj_LLIN_80[["IV"]]
  
  IV_LLIN_50_IRS_50 = IVM_traj_LLIN_50_IRS_50[["IV"]]
  
  IV_LLIN_50_LAR_50 = IVM_traj_LLIN_50_LAR_50[["IV"]]
  
  IV_LLIN_50_ECT_50 = IVM_traj_LLIN_50_ECT_50[["IV"]]
  
  IV_LLIN_50_HOU_50 = IVM_traj_LLIN_50_HOU_50[["IV"]]
  
  IV_LLIN_50_PPM_50 = IVM_traj_LLIN_50_PPM_50[["IV"]]
  
  IV_LLIN_50_ATSB_50 =IVM_traj_LLIN_50_ATSB_50[["IV"]]
  
  
   #EIR
  time_on = 20                 #Manually set it to the earliest time an intervention is on - to be automated
  EIR_control       <-computeEIR(time_on,a_theta, IV_Control, NH)
  EIR_LLIN_80            <-computeEIR(time_on,a_theta, IV_LLIN_80, NH)
  EIR_LLIN_50_IRS_50       <-computeEIR(time_on,a_theta, IV_LLIN_50_IRS_50, NH)
  EIR_LLIN_50_LAR_50     <-computeEIR(time_on,a_theta, IV_LLIN_50_LAR_50, NH)
  EIR_LLIN_50_ECT_50 <-computeEIR(time_on,a_theta, IV_LLIN_50_ECT_50, NH)
  EIR_LLIN_50_HOU_50   <-computeEIR(time_on,a_theta, IV_LLIN_50_HOU_50, NH)
  EIR_LLIN_50_PPM_50   <-computeEIR(time_on,a_theta, IV_LLIN_50_PPM_50, NH)
  EIR_LLIN_50_ATSB_50   <-computeEIR(time_on,a_theta, IV_LLIN_50_ATSB_50, NH)
  
  
  #EIR <-computeEIR(a_theta, IV, NH)
  # 
  p2 <- ggplot(IVM_traj_Control, aes(x = 1:length(IVM_traj_Control[,1]), y = EIR, color = Key)) +
    geom_line(aes(y = EIR_control,        col = "Control"), size = 1.2) + 
    geom_line(aes(y = EIR_LLIN_80 ,       col = "LLIN=0.80"), size = 1.2) +
    geom_line(aes(y = EIR_LLIN_50_IRS_50, col = "LLIN=0.50 & IRS=0.50"), size = 1.2) +
    geom_line(aes(y = EIR_LLIN_50_LAR_50, col = "LLIN=0.50 & LAR=0.50"), size = 1.2) +
    geom_line(aes(y = EIR_LLIN_50_ECT_50, col = "LLIN=0.50 & ECT=0.50"), size = 1.2) +
    geom_line(aes(y = EIR_LLIN_50_HOU_50, col = "LLIN=0.50 & HOU=0.50"), size = 1.2) +
    geom_line(aes(y = EIR_LLIN_50_PPM_50, col = "LLIN=0.50 & PPM=0.50"), size = 1.2) +
    geom_line(aes(y = EIR_LLIN_50_ATSB_50,col = "LLIN=0.50 & ATSB=0.50"), size = 1.2) +
    labs(title = "An Funestus", x = "Time (days)", y = "EIR")
  p2 <- p2 + expand_limits(x = 0, y = 0)
  multiplot(p2, cols=1)
  
  EIR_eq <-c(tail(EIR_control,1),tail(EIR_LLIN_80,1),tail(EIR_LLIN_50_IRS_50,1),tail(EIR_LLIN_50_LAR_50,1)
             ,tail(EIR_LLIN_50_ECT_50,1),tail(EIR_LLIN_50_HOU_50,1),tail(EIR_LLIN_50_PPM_50,1),tail(EIR_LLIN_50_ATSB_50,1))
  
  ###Computing R0

  
  time_on = 20                 #Manually set it to the earliest time an intervention is on - to be automated
  R0_control       <-computeRO(time_on,a_theta,muVCom, NV_Control,bv,bh,NH)
  R0_LLIN_80            <-computeRO(time_on,a_theta,muVCom, NV_LLIN_80,bv,bh,NH)
  R0_LLIN_50_IRS_50       <-computeRO(time_on,a_theta,muVCom, NV_LLIN_50_IRS_50,bv,bh,NH)
  R0_LLIN_50_LAR_50     <-computeRO(time_on,a_theta,muVCom, NV_LLIN_50_LAR_50,bv,bh,NH)
  R0_LLIN_50_ECT_50 <-computeRO(time_on,a_theta,muVCom, NV_LLIN_50_ECT_50,bv,bh,NH)
  R0_LLIN_50_HOU_50   <-computeRO(time_on,a_theta,muVCom, NV_LLIN_50_HOU_50,bv,bh,NH)
  R0_LLIN_50_PPM_50   <-computeRO(time_on,a_theta,muVCom, NV_LLIN_50_PPM_50,bv,bh,NH)
  R0_LLIN_50_ATSB_50   <-computeRO(time_on,a_theta,muVCom, NV_LLIN_50_ATSB_50,bv,bh,NH)
  
  R0_eq <-c(tail(R0_control,1),tail(R0_LLIN_80,1),tail(R0_LLIN_50_IRS_50,1),tail(R0_LLIN_50_LAR_50,1)
             ,tail(R0_LLIN_50_ECT_50,1),tail(R0_LLIN_50_HOU_50,1),tail(R0_LLIN_50_PPM_50,1),tail(R0_LLIN_50_ATSB_50,1))
  browser()
  
  p3 <- ggplot(IVM_traj_Control, aes(x = 1:length(IVM_traj_Control[,1]), y = EIR, color = Key)) +
    geom_line(aes(y = R0_control,        col = "Control"), size = 1.2) + 
    geom_line(aes(y = R0_LLIN_80 ,       col = "LLIN=0.80"), size = 1.2) +
    geom_line(aes(y = R0_LLIN_50_IRS_50, col = "LLIN=0.50 & IRS=0.50"), size = 1.2) +
    geom_line(aes(y = R0_LLIN_50_LAR_50, col = "LLIN=0.50 & LAR=0.50"), size = 1.2) +
    geom_line(aes(y = R0_LLIN_50_ECT_50, col = "LLIN=0.50 & ECT=0.50"), size = 1.2) +
    geom_line(aes(y = R0_LLIN_50_HOU_50, col = "LLIN=0.50 & HOU=0.50"), size = 1.2) +
    geom_line(aes(y = R0_LLIN_50_PPM_50, col = "LLIN=0.50 & PPM=0.50"), size = 1.2) +
    geom_line(aes(y = R0_LLIN_50_ATSB_50,col = "LLIN=0.50 & ATSB=0.50"), size = 1.2) +
    labs(title = "An Funestus", x = "Time (days)", y = "R0")
  
  multiplot(p3, cols=1)
  
  NumMosq_eq
  EIR_eq
  R0_eq
  
  #Average EIR
  #divValue = length(EIR_control)
  #EIR_AVERAGE <- c(sum(EIR_control)/divValue,sum(EIR_LLIN_50)/divValue,sum(EIR_Cattle_50)/divValue,sum(EIR_CC)/divValue,sum(EIR_LLIN_HOUSE_50)/divValue,sum(EIR_LLIN_OBT_50)/divValue)
  #barplot(EIR_AVERAGE , main='An. Arabiensis EIR = 100', xlab='Tools',ylab='Average EIR',names.arg=c('Baseline','LLIN-50%','ITC - 50%','ITC & LLIN - 50%','LLIN & MPH - 50','LLIN & OBT - 50'))
  
#  write.csv(EIR_AVERAGE, file = "foo.csv")
  
 # myData = list(c('Baseline','ITC & LLIN - 50%','LLIN-50%','ITC - 50%','LLIN & MPH - 50','LLIN & OBT - 50'),EIR_AVERAGE)
  
  #browser()
  breakPoint = 1
  #Mosquito density
  # p1 <- ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
  #   geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) + 
  #   #geom_line(aes(y = IV, col = "IV"), size = 1.2) +
  #   labs(x = "Time (days)", y = "Number of total mosquitoes")
  # 
  # #Mosquito density
  # p2 <- ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
  #   #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) + 
  #   geom_line(aes(y = IV, col = "IV"), size = 1.2) +
  #   labs(x = "Time (days)", y = "Number ofinfectious mosquitoes")
  # 
  # #EIR
  # time_on = 20                 #Manually set it to the earliest time an intervention is on - to be automated
  # EIR <-computeEIR(time_on,a_theta, IV, NH)
  # #EIR <-computeEIR(a_theta, IV, NH)
  # 
  # p3 <- ggplot(IVM_traj, aes(x = 1:length(IVM_traj[,1]), y = EIR, color = State)) +
  #   geom_line(aes(y = EIR, col = "EIR"), size = 1.2) + 
  #   #geom_line(aes(y = IVM_traj["EV"], col = "EV"), size = 1.2) +
  #   labs(x = "Time (days)", y = " EIR")
  # 
  # #VC
  # 
  # 
  # 
  # 
  # 
  # 
  # #All at once
  # 
  # # #IVM_traj - just space holder
  # # # We might need to plot them separately - s
  # # p4 <- ggplot(IVM_traj, aes(x = 1:length(IVM_traj[,1]), y = EIR, color = State)) +
  # #   #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) + 
  # #   geom_line(aes(y = EIR, col = "EIR"), size = 1.2) + 
  # #   geom_line(aes(y = VC, col = "VC"), size = 1.2) + 
  # #   geom_line(aes(y = R0, col = "R0"), size = 1.2) + 
  # #   #geom_line(aes(y = IVM_traj["EV"], col = "EV"), size = 1.2) +
  # #   labs(x = "Time (days)", y = "values for EIR, VC, or R0")
  # # 
  # # 
  # multiplot(p1, p2,p3, cols=1)
  # 
  
  
}




barChartMosquitoDemographics = function(IVM_traj){
  #. barChartMosquitoDemographics: Generates a bar chart with the amount of mosquitos in each life stage
  #NumMosq <- c(sum(IVM_traj[2]),sum(IVM_traj[3]),sum(IVM_traj[4]),sum(IVM_traj[5]),sum(IVM_traj[6]),sum(IVM_traj[7]))
  #Exctract using names
  NumMosq <- c(sum(IVM_traj["EL"]),sum(IVM_traj["LL"]),sum(IVM_traj["PL"]),sum(IVM_traj["SV"]),sum(IVM_traj["EV"]),sum(IVM_traj["IV"]))
  barplot(NumMosq , main='', xlab='Time (days)',ylab='Number of Mosquitoes',names.arg=c('EL','LL','PL','SV','EV','IV'))
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