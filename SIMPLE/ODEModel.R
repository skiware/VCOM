########################################################################
#=======================================================================
# ODEModel.R
# Contains the functions that deal with the main ODE mosquitos model
#
#=======================================================================
########################################################################

######################################################################################
runODE = function(tMax, tIncrement, initState, theta, method){
  #. runODE: Main ODE wrapper for simulating the mosquito population
  simPeriod <- tMax # Simulation runs up to 365 days

  times <- seq(0, simPeriod, by = tIncrement)
  IVM_traj <- data.frame(dede(y = initState, times = times, parms = theta, func = IVM_ode, method = "lsoda"))
  #print(IVM_traj)
  return(IVM_traj)
}
######################################################################################
calculateInitialState = function(theta){
  #. calculateInitialState: Calculates the initial conditions of a system given the theta parameters
  beta <- theta[["beta"]]; muEL <- theta[["muEL"]]; muLL <- theta[["muLL"]]
  muPL <- theta[["muPL"]]; muV <- theta[["muV"]]; durEL <- theta[["durEL"]]
  durLL <- theta[["durLL"]]; durPL <- theta[["durPL"]]; durEV <- theta[["durEV"]]
  gamma <- theta[["gamma"]]; Q0 <- theta[["Q0"]]; f0 <- theta[["f0"]]
  epsilon0 <- theta[["epsilon0"]]; iH_eq <- theta[["iH_eq"]]
  NH_eq <- theta[["NH_eq"]]; bV <- theta[["bV"]];bh <- theta[["bh"]]

  b_omega <- gamma*muLL/muEL - durEL/durLL + (gamma-1)*muLL*durEL
  omega <- -0.5*b_omega + sqrt(0.25*b_omega^2 + gamma*beta*muLL*durEL/(2*muEL*muV*durLL*(1+durPL*muPL)))
  a0 <- Q0*f0 # Human biting rate at equilibrium

  lambdaV <- a0*iH_eq*bV # Force of infection in mosquitoes at equilibrium
  theta["lambdaV"] <<- lambdaV # Include vector force of infection in vector of parameters (theta)

  iV_eq <- lambdaV*exp(-muV*durEV)/(lambdaV + muV)
  sV_eq <- iV_eq*muV/(lambdaV*exp(-muV*durEV))
  eV_eq <- 1 - sV_eq - iV_eq

  NV_eq <- epsilon0*NH_eq/(iV_eq*a0)
  theta["NV_eq"] <<- NV_eq # Include equilibrium vector population size in vector of parameters (theta)

  EL_eq <- 2*omega*muV*durLL*(1 + muPL*durPL)*NV_eq
  LL_eq <- 2*muV*durLL*(1 + muPL*durPL)*NV_eq
  PL_eq <- 2*muV*durPL*NV_eq
  SV_eq <- sV_eq*NV_eq
  EV_eq <- eV_eq*NV_eq
  IV_eq <- iV_eq*NV_eq

  #initState <- c(EL = EL_eq,LL = LL_eq,PL = PL_eq,SV = SV_eq,EV = EV_eq,IV = IV_eq)
  # Initiated EL_LAR, etc as EL_eq etc, IS THIS OK? Initialized to 0

  #initState <- c(EL = EL_eq,EL_LAR = EL_eq,EL_BIO = EL_eq,EL_LAR_BIO = EL_eq,LL = LL_eq,LL_LAR = LL_eq,LL_BIO = LL_eq,LL_LAR_BIO = LL_eq,PL = PL_eq,SV = SV_eq,EV = EV_eq,IV = IV_eq)

  initState <- c(EL = EL_eq,EL_LAR = 0,EL_BIO = 0,EL_LAR_BIO = 0,LL = LL_eq,LL_LAR = 0,LL_BIO = 0,LL_LAR_BIO = 0,PL = PL_eq,SV = SV_eq,EV = EV_eq,IV = IV_eq)

  return(initState)
}
######################################################################################
IVM_ode <- function(time, state, theta){
  #. IVM_ode: ODE Model definition
  ## Parameters (mosquito life cycle):
  beta <- theta[["beta"]] # Eggs laid per day by female mosquito
  muEL <- theta[["muEL"]] # Early instar stage daily mortality
  muLL <- theta[["muLL"]] # Late instar stage daily mortality
  muPL <- theta[["muPL"]] # Pupal stage daily mortality
  muV <- theta[["muV"]] # Adult mosquito daily mortality
  Q0 <- theta[["Q0"]] # Human blood index
  phiB <- theta[["phiB"]] # Proportion of bites on a person while they are in bed
  phiI <- theta[["phiI"]] # Proportion of bites on a person while they are indoors
  durEL <- theta[["durEL"]] # Duration of early instar stage (days)
  durLL <- theta[["durLL"]] # Duration of late instar stage (days)
  durPL <- theta[["durPL"]] # Duration of pupal stage (days)
  durEV <- theta[["durEV"]] # Duration of latent period in mosquito (days)
  gamma <- theta[["gamma"]] # Effect of density-dependence on late instarts relative to early instars
  tau1 <- theta[["tau1"]] # Time spent foraging for a blood meal (no ITNs) (days)
  tau2 <- theta[["tau1"]] # Time spent resting and ovipositing (days)
  NV_eq <- theta[["NV_eq"]] # Number of female mosquitoes at equilibrium
  #e_ov  <- theta[["e_ov"]] # # Number of eggs per oviposition per mosquito - SK wanted to add it in theta but recomputed in FC
  lambdaV <- theta[["lambdaV"]] # Force of infection in vectors at equilibrium
  ## Parameters (interventions):
  ITNcov <- theta[["ITNcov"]] # ITN coverage
  IRScov <- theta[["IRScov"]] # IRS coverave
  time_ITN_on <- theta[["time_ITN_on"]] # When ITNs are applied (days)
  time_IRS_on <- theta[["time_IRS_on"]] # When IRS is applied (days)
  rITN <- theta[["rITN"]] # Probability of mosquito repeating a feeding attempt due to IRS
  sITN <- theta[["sITN"]] # Probability of mosquito feeding and surviving in presence of ITNs
  rIRS <- theta[["rIRS"]] # Probability of mosquito repeating a feeding attempt due to IRS
  sIRS <- theta[["sIRS"]] # Probability of mosquito feeding and surviving in presence of IRS
  dIRS <- theta[["dIRS"]] # death rate after encountering IRS
  dHOU <- theta[["dHOU"]] # death rate after encountering mosquito proofed housing
  ## Add other interventions - SK

  ## Mosquito proofed housing
  HOUcov <- theta[["HOUcov"]] # proportion of houses that are mosquito proofed housing
  time_HOU_on <- theta[["time_HOU_on"]] # When HM is applied (days)
  rHOU <- theta[["rHOU"]] # Probability of mosquito repeating a feeding attempt due to a mosquito proofed housing
  sHOU <- theta[["sHOU"]] # Probability of mosquito feeding and surviving in presence of mosquito proofed housing

  ## endocticide-treated cattle (either topical or systemic)
  ECScov <- theta[["ECScov"]] # endocticide (e.g., ivermectin) coverage - systemic coverage
  time_ECS_on <- theta[["time_ECS_on"]] # When cattle are treated (systemic) (days)
  #time_ENDO_of <- theta[["time_ENDO_of"]] # When ivermectin is no longer effective (days)
  sECS <- theta[["sECS"]] # Probability of mosquito feeding and surviving in presence of systemic-treated cattle

  ECTcov <- theta[["ECTcov"]] # endocticide (e.g., ivermectin) coverage - topical coverage
  time_ECT_on <- theta[["time_ECT_on"]] # When cattle are treated (topical) (days)
  sECT <- theta[["sECT"]] # Probability of mosquito feeding and surviving in presence of topical-treated cattle
  rECT <- theta[["rECT"]] # Probability of mosquito repeating a feeding attempt due to topical-treated cattle

  ## Protecting human outdoor - Spatial repellents and Personal protection Measures***************
  SPRcov <- theta[["SPRcov"]] # proportion of a defined space with Spatial repelents
  time_SPR_on <- theta[["time_SPR_on"]] # When SPR is on)
  rSPR <- theta[["rSPR"]] # Probability of mosquito repeating a feeding attempt due personal protection measure (PPM)
  sSPR <- theta[["sSPR"]] # Probability of mosquito feeding in presence of PPM
  PPMcov <- theta[["PPMcov"]] # proportion of people outside with PPM
  time_PPM_on <- theta[["time_PPM_on"]] # When PPM is on)
  rPPM <- theta[["rPPM"]] # Probability of mosquito repeating a feeding attempt due personal protection measures
  sPPM <- theta[["sPPM"]] # Probability of mosquito feeding in presence of PPM


  ##***********Odor baited traps ********************
  time_OBT_on <- theta[["time_OBT_on"]] # When OBT is on)
  aOBT <- theta[["aOBT"]] # availability of one trap in relation to one human
  OBTcov <- theta[["OBTcov"]] # ratio of traps to human (i.e., coverage)

  ##***********Source Reduction ********************
  time_SRE_on <- theta[["time_SRE_on"]] # When source reduction is on)
  eSRE <- theta[["eSRE"]] # effectiveness of source reduction
  SREcov <- theta[["SREcov"]] # prop of aquatic habitats covered by source reduction

  ##***********Host Seeking - ATSB and Space spraying ********************
  time_ATSB_on <- theta[["time_ATSB_on"]] # When ATSB is on)
  fATSB <- theta[["fATSB"]] # factor allowing for increased death rate due to ATSB
  ATSBcov <- theta[["ATSBcov"]] # prop of vegation sprayed with ATSB

  time_SSP_on <- theta[["time_SSP_on"]] # When space spraying is on)
  fSSP <- theta[["fSSP"]] # factor allowing for increased death rate due to space spraying
  SSPcov <- theta[["SSPcov"]] # prop mosquito habitats in a defined area sprayed with insecticide


  ATSBcov <- theta[["ATSBcov"]] # prop of vegation sprayed with

  time_SSP_on <- theta[["time_SSP_on"]] # When space spraying is on)
  fSSP <- theta[["fSSP"]] # factor allowing for increased death rate due to space spraying
  SSPcov <- theta[["SSPcov"]] # prop of a defined area sprayed with insecticide

  ##***********Resting and Ovipositing: ATSB, Space spraying, ovitraps ********************
  time_OVI_on <- theta[["time_OVI_on"]] # When ATSB is on)
  fOVI <- theta[["fOVI"]] # factor allowing for increased death rate due to ATSB
  OVIcov <- theta[["OVIcov"]] # prop of ovitraps
  #ATSB and SSP - already defined - might have two differentiate the two

  ##**********************Larviciding and Biologica Control *********************

  time_BIO_on <- theta[["time_BIO_on"]] # When biological control is on)
  fBIO <- theta[["fOVI"]] # factor allowing for increased death rate due to biological control
  BIOcov <- theta[["BIOcov"]] # prop of aquatic habitats covered by biological control
  time_LAR_on <- theta[["time_LAR_on"]] # When larvicide is on)
  fLAR <- theta[["fOVI"]] # factor allowing for increased death rate due to larvacide
  LARcov <- theta[["LARcov"]] # prop of aquatic habitats covered by larvicide


  ## States:  - Defn added by SK
  EL         <- state[["EL"]]  # Early Instar stage
  EL_LAR     <- state[["EL_LAR"]]  # Early Instar stage in presence of larvaciding
  EL_BIO     <- state[["EL_BIO"]]  # Early Instar stage in presence of biological control
  EL_LAR_BIO <- state[["EL_LAR_BIO"]]  # Early Instar stage in presence of both
  LL         <- state[["LL"]]  # Late Instar stage
  LL_LAR     <- state[["LL_LAR"]]  # Late Instar stage in presence of larvaciding
  LL_BIO     <- state[["LL_BIO"]]  # Late Instar stage in presence of biological control
  LL_LAR_BIO <- state[["LL_LAR_BIO"]]  # Late Instar stage in presence of both
  PL         <- state[["PL"]]  # Pupal stage
  SV         <- state[["SV"]]  # Susceptitable Vectors
  EV         <- state[["EV"]]  # Latent period
  IV         <- state[["IV"]]  # Infectious Vectors


  ## Derived parameters: (SK - IMPoRANT add to the write up)
  NV <- SV + EV + IV # Total mosquito population size
  #Equation 1
  delta <- 1/(tau1+tau2) # Inverse of gonotrophic cycle without ITNs/IRS (DONE)
  #Equation 3
  e_ov <- beta*(exp(muV/delta)-1)/muV # Number of eggs per oviposition per mosquito (DONE)
  b_omega <- gamma*muLL/muEL - durEL/durLL + (gamma-1)*muLL*durEL
  omega <- -0.5*b_omega + sqrt(0.25*b_omega^2 + gamma*beta*muLL*durEL/(2*muEL*muV*durLL*(1+durPL*muPL)))
  K <- 2*NV_eq*muV*durLL*(1 + muPL*durPL)*gamma*(omega+1)/(omega/(muLL*durEL) - 1/(muLL*durLL) - 1) # Larval carrying capacity
  ## Derived parameters which depend on intervention status:

 #browser()   #break point


  ##**********Aquatic habitats - impact of source management ****##############################
  K_sr    <<- impactSourceReduction(time,eSRE,SREcov,time_SRE_on,K)

   #Update K
  K = K_sr



  ##**************Feeding cycle in presence of interventions ****************************
  feedingCycleImpact= impactFeedingCycleParameters(time,beta,tau1,tau2,e_ov,time_ATSB_on,ATSBcov,time_SSP_on,SSPcov,fSSP,fATSB,muV,
                     Q0,aOBT,OBTcov,time_OBT_on,time_ITN_on,ITNcov,time_IRS_on,IRScov,HOUcov,
                     time_HOU_on,rITN,sITN,rIRS,rHOU,sIRS,sHOU, phiB, phiI,dHOU,dIRS,
                     time_SPR_on,SPRcov,time_PPM_on,PPMcov,rSPR,rPPM,sSPR,sPPM,
                     c0,time_ECS_on,ECScov,time_ECT_on,ECTcov,rECT,sECS,sECT,
                     time_OVI_on,OVIcov,fOVI)

  muVCom  = feedingCycleImpact[1]
  betaCom = feedingCycleImpact[2]

  ##****************Other Outputs####################

  #Extract transmission parameter values
  #browser()
  transmissionValues = getAdditionalTransmissionParameters()
  bV = transmissionValues[["bV"]]
  NH = transmissionValues[["NH_eq"]]
  bh = transmissionValues[["bh"]]



  # #Mosquito density
  # Mdensity = computeMosqDensity(NV,NH)
  #
  # #Length of gonotrophic cycle
  #  f_theta <- computeLengthGonotrophicycle(deltaCom)
  # # #HBI
  #  wCom_human = wCom_Human_Indoor + wCom_Human_Outdoor
  #  HBI_com <- computeHBI(wCom_human,wCom)
  # # #Human biting rate
  #  a_theta <- computeHumanBitingRate(f_theta,HBI_com)
  # # #VC
  #  VC<- computeVC(a_theta, Mdensity,muVCom,durEV)
  # # #Sam get transmission parameters and proceed...
  # # #EIR
  #  EIR <-computeEIR(a_theta, IV, NH)
  # # #RO
  #  R0 <- computeRO(a_theta,muVCom, NV,bV,bh,NH)

  ##*********************************************************************

  ##******Add Larviciding and biological control -move to a function
  #I ABSOLUTELLY HATE THIS PATCH BUT IT FIXES THE ERROR ON SETTING COVERAGES TO ZERO!!!! I REALLY HATE IT... A LOT!!!!!! IT MAKES ME CRY!!!!!
  AD_HOC_HORRIBLE_PATCH = .0000000001
  if (time > time_LAR_on) { LARcov_t <- LARcov + AD_HOC_HORRIBLE_PATCH } else { LARcov_t <- 0 }
  if (time > time_BIO_on) { BIOcov_t <- BIOcov + AD_HOC_HORRIBLE_PATCH } else { BIOcov_t <- 0 }

  #Coverage larvaciding only
  cLAR <- LARcov_t - LARcov_t*BIOcov_t
  # Coverage with biological control  only
  cBIO <- BIOcov_t -  LARcov_t*BIOcov_t
  #both applied
  c_LAR_BIO <-  LARcov_t*BIOcov_t
  # neither applied
  c0_LAR_BIO <- 1 - LARcov_t - BIOcov_t +  LARcov_t*BIOcov_t

  f_LAR_BIO =fLAR*fBIO

  #browser()

  #################**********************************
    ##********************************* ODEs:
  if(time < durEV){
    SVLag <- SV
  }else{
    lagStates <- lagvalue(time-durEV)
    SVLag <- lagStates[4]
  }


  if( time > 7){
   # browser()
  }



  #if( time > 20){
  #  browser()
  #}

    ###

  #dEL   <- betaCom*NV - muEL*(1 + ((EL+LL)/K))*EL - EL/durEL
  #dLL   <-EL/durEL - muLL*(1 + gamma*((EL+LL)/K))*LL - LL/durLL

   dEL   <- c0_LAR_BIO*betaCom*NV - muEL*(1 + ((EL+LL)/K*c0_LAR_BIO))*EL - EL/durEL
   dLL     <-c0_LAR_BIO*EL/durEL - muLL*(1 + gamma*((EL+LL)/K*c0_LAR_BIO))*LL - LL/durLL
   dPL <- LL/durLL - muPL*PL - PL/durPL

   ###*************Enable Larvaciding and biological control************
   if( time > time_LAR_on && cLAR >0 ){dEL_LAR <- cLAR*betaCom*NV - fLAR*muEL*(1 + ((EL_LAR+LL_LAR)/cLAR*K))*EL_LAR - EL_LAR/durEL}else{dEL_LAR<-0}
   if( time > time_BIO_on && cBIO >0){dEL_BIO <- cBIO*betaCom*NV - fBIO*muEL*(1 + ((EL_BIO+LL_BIO)/cBIO*K))*EL_BIO - EL_BIO/durEL}else{dEL_BIO <-0}
   if( time > time_LAR_on && cLAR >0 && time > time_BIO_on && cBIO >0){dEL_LAR_BIO <- c_LAR_BIO*betaCom*NV - f_LAR_BIO*muEL*(1 + ((EL_LAR_BIO+LL_LAR_BIO)/c_LAR_BIO*K))*EL_LAR_BIO - EL_LAR_BIO/durEL}else{dEL_LAR_BIO<-0}

   if( time > time_LAR_on ){dEL_LAR <- cLAR*betaCom*NV - fLAR*muEL*(1 + ((EL_LAR+LL_LAR)/cLAR*K))*EL_LAR - EL_LAR/durEL}else{dEL_LAR<-0}
   if( time > time_BIO_on ){dEL_BIO <- cBIO*betaCom*NV - fBIO*muEL*(1 + ((EL_BIO+LL_BIO)/cBIO*K))*EL_BIO - EL_BIO/durEL}else{dEL_BIO <-0}
   if( (time > time_LAR_on) && (time > time_BIO_on) ){dEL_LAR_BIO <- c_LAR_BIO*betaCom*NV - f_LAR_BIO*muEL*(1 + ((EL_LAR_BIO+LL_LAR_BIO)/c_LAR_BIO*K))*EL_LAR_BIO - EL_LAR_BIO/durEL}else{dEL_LAR_BIO<-0}


   if( time > time_LAR_on ){dLL_LAR <-cLAR*EL/durEL - fLAR*muLL*(1 + gamma*((EL_LAR+LL_LAR)/K*cLAR))*LL_LAR - LL_LAR/durLL}else{dLL_LAR<-0}
   if( time > time_BIO_on ){dLL_BIO <-cBIO*EL/durEL - fBIO*muLL*(1 + gamma*((EL_BIO+LL_BIO)/K*cBIO))*LL_BIO - LL_BIO/durLL}else{dLL_BIO<-0}
   if( (time > time_LAR_on) && (time > time_BIO_on) ){dLL_LAR_BIO <-c_LAR_BIO*EL/durEL - f_LAR_BIO*muLL*(1 + gamma*((EL_LAR_BIO+LL_LAR_BIO)/K*c_LAR_BIO))*LL_LAR_BIO - LL_LAR_BIO/durLL}else{dLL_LAR_BIO<-0}

   # #with coverage
   # if( (time > time_LAR_on) && (cLAR >0) ){dLL_LAR <-cLAR*EL/durEL - fLAR*muLL*(1 + gamma*((EL_LAR+LL_LAR)/K*cLAR))*LL_LAR - LL_LAR/durLL}else{dLL_LAR<-0}
   # if( (time > time_BIO_on) && (cBIO >0) ){dLL_BIO <-cBIO*EL/durEL - fBIO*muLL*(1 + gamma*((EL_BIO+LL_BIO)/K*cBIO))*LL_BIO - LL_BIO/durLL}else{dLL_BIO<-0}
   # if( ((time > time_LAR_on) && (cLAR >0)) && ((time > time_BIO_on) && (cBIO >0)) ){dLL_LAR_BIO <-c_LAR_BIO*EL/durEL - f_LAR_BIO*muLL*(1 + gamma*((EL_LAR_BIO+LL_LAR_BIO)/K*c_LAR_BIO))*LL_LAR_BIO - LL_LAR_BIO/durLL}else{dLL_LAR_BIO<-0}
   # #



  if( time > time_LAR_on && cLAR >0 ){dLL_LAR <-cLAR*EL/durEL - fLAR*muLL*(1 + gamma*((EL_LAR+LL_LAR)/K*cLAR))*LL_LAR - LL_LAR/durLL}else{dLL_LAR<-0}
  if( time > time_BIO_on && cBIO >0 ){dLL_BIO <-cBIO*EL/durEL - fBIO*muLL*(1 + gamma*((EL_BIO+LL_BIO)/K*cBIO))*LL_BIO - LL_BIO/durLL}else{dLL_BIO<-0}
  if( time > time_LAR_on && cLAR >0 && time > time_BIO_on && cBIO >0 ){dLL_LAR_BIO <-c_LAR_BIO*EL/durEL - f_LAR_BIO*muLL*(1 + gamma*((EL_LAR_BIO+LL_LAR_BIO)/K*c_LAR_BIO))*LL_LAR_BIO - LL_LAR_BIO/durLL}else{dLL_LAR_BIO<-0}
  #
  #Do we need to LL = LL+LAR+BIO+LAR_BIO????


  #0.5 only dealing with female mosquitoes
  dSV <- 0.5*PL/durPL - lambdaV*SV - muVCom*SV
  dEV <- lambdaV*SV - lambdaV*SVLag*exp(-muVCom*durEV) - muVCom*EV
  dIV <- lambdaV*SVLag*exp(-muVCom*durEV) - muVCom*IV

  #return(list(c(dEL, dLL, dPL, dSV, dEV, dIV)))


  #With larvaciding and biological control
  return(list(c(dEL,dEL_LAR,dEL_BIO,dEL_LAR_BIO, dLL,dLL_LAR,dLL_BIO,dLL_LAR_BIO, dPL, dSV, dEV, dIV)))

  }
######################################################################################

