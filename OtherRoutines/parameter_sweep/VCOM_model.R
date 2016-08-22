#############################################################
######Functions to run VCOM ODE model and return output######
######Sean Wu 8/12/2016######################################
#############################################################


####################################################
###Wrapper function to run VCOM and return output###
####################################################

#parameters is a list to feed to run_vcom function; time_end is only free parameter

run_vcom <- function(parameters=list(
  epsilon0=50, species=1,       
  OVIcov=0,   time_OVI_on=0,
  FOGcov=0,   time_FOG_on=0,
  LARcov=0,   time_LAR_on=0,
  BIOcov=0,   time_BIO_on=0,
  SREcov=0,   time_SRE_on=0,
  IRScov=0,   time_IRS_on=0,
  ITNcov=0,   time_ITN_on=0,
  ECScov=0,   time_ECS_on=0,
  ECTcov=0,   time_ECT_on=0,
  HOUcov=0,   time_HOU_on=0,
  OBTcov=0,   time_OBT_on=0,
  SPRcov=0,   time_SPR_on=0,
  PPMcov=0,   time_PPM_on=0,
  ATSBcov=0,  time_ATSB_on=0,
  SSPcov=0,   time_SSP_on=0),time_end
){
  
  vcom_parms <- with(parameters,{
    vcom_initialization(epsilon0, species,       
                        OVIcov,   time_OVI_on,
                        FOGcov,   time_FOG_on,
                        LARcov,   time_LAR_on,
                        BIOcov,   time_BIO_on,
                        SREcov,   time_SRE_on,
                        IRScov,   time_IRS_on,
                        ITNcov,   time_ITN_on,
                        ECScov,   time_ECS_on,
                        ECTcov,   time_ECT_on,
                        HOUcov,   time_HOU_on,
                        OBTcov,   time_OBT_on,
                        SPRcov,   time_SPR_on,
                        PPMcov,   time_PPM_on,
                        ATSBcov,  time_ATSB_on,
                        SSPcov,   time_SSP_on)
  })
  
  times <- seq(0,time_end,by=1) #times to run the model over
  initState <- vcom_parms$initState #initial state variables
  theta <- vcom_parms$theta
  sim_traj <- data.frame(dede(y = initState, times = times, parms = theta, func = IVM_ode, method = "lsoda"))
  return(sim_traj)
}


#######################
###ODE model of VCOM###
#######################

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
  tau2 <- theta[["tau2"]] # Time spent resting and ovipositing (days)
  NV_eq <- theta[["NV_eq"]] # Number of female mosquitoes at equilibrium
  recRate <- theta[["recRate"]]
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
  
  bv = theta[["bV"]]
  NH = theta[["NH_eq"]]
  bh = theta[["bh"]]
  
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
  
  ##***********Resting and Ovipositing: ATSB, Space spraying, ovitraps ********************
  time_OVI_on <- theta[["time_OVI_on"]] # When ATSB is on)
  fOVI <- theta[["fOVI"]] # factor allowing for increased death rate due to ATSB
  eOVI <- theta[["eOVI"]] #effectiveness of the ovitrap
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
  #Males
  Mm         <- state[["Mm"]]  # Number of adult male mosquitoes
  
  #For SIR - humans
  SH         <- state[["SH"]]  # Susceptitable humans
  IH         <- state[["IH"]]  # Infectious humans
  
  
  ## Derived parameters: (SK - IMPoRANT add to the write up)
  NV <- SV + EV + IV # Total mosquito population size
  #Equation 1
  delta <- 1/(tau1+tau2) # Inverse of gonotrophic cycle without interventions
  #Equation 3
  e_ov <- beta*(exp(muV/delta)-1)/muV # Number of eggs per oviposition per mosquito (DONE)
  b_omega <- gamma*muLL/muEL - durEL/durLL + (gamma-1)*muLL*durEL
  omega <- -0.5*b_omega + sqrt(0.25*b_omega^2 + gamma*beta*muLL*durEL/(2*muEL*muV*durLL*(1+durPL*muPL)))
  K <- 2*NV_eq*muV*durLL*(1 + muPL*durPL)*gamma*(omega+1)/(omega/(muLL*durEL) - 1/(muLL*durLL) - 1) # Larval carrying capacity
  ## Derived parameters which depend on intervention status:
  
  #browser()   #break point
  
  
  ##**********Aquatic habitats - impact of source management ****##############################
  K_sr    <- impactSourceReduction(time,eSRE,SREcov,time_SRE_on,K)
  
  #Update K 
  K = K_sr
  
  if( time > 0){
    #browser()
  }
  #browser()
  ##**************Feeding cycle in presence of interventions ****************************
  feedingCycleImpact= impactFeedingCycleParameters(time,beta,tau1,tau2,e_ov,time_ATSB_on,ATSBcov,time_SSP_on,SSPcov,fSSP,fATSB,muV,
                                                   Q0,aOBT,OBTcov,time_OBT_on,time_ITN_on,ITNcov,time_IRS_on,IRScov,HOUcov,
                                                   time_HOU_on,rITN,sITN,rIRS,rHOU,sIRS,sHOU, phiB, phiI,dHOU,dIRS,
                                                   time_SPR_on,SPRcov,time_PPM_on,PPMcov,rSPR,rPPM,sSPR,sPPM,
                                                   c0,time_ECS_on,ECScov,time_ECT_on,ECTcov,rECT,sECS,sECT,
                                                   time_OVI_on,OVIcov,fOVI,eOVI)
  
  muVCom  = feedingCycleImpact[1]
  betaCom = feedingCycleImpact[2]
  a_theta = feedingCycleImpact[3]
  
  #Introducet the impact of ovitrap on eggs laying
  if (time > time_OVI_on) { OVIcov_t <- OVIcov } else { OVIcov_t <- 0 }
  
  betaCom = betaCom *(1-eOVI*OVIcov_t)
  
  
  ##****************Other Outputs####################
  
  #Compute EIR
  EIR <-computeEIR(a_theta, IV, NH)
  
  #compute Basic reproductive number
  R0 <- computeRO(a_theta,muVCom, NV,bv,bh,NH,theta)
  #Compute Vectoral capacity
  VC <- computeVC(a_theta, NV,NH,muVCom,theta)
  
  ##******Add Larviciding and biological control -move to a function
  
  
  if (time > time_LAR_on) { LARcov_t <- LARcov } else { LARcov_t <- 0 }
  if (time > time_BIO_on) { BIOcov_t <- BIOcov } else { BIOcov_t <- 0 }
  
  #Coverage larvaciding only
  cLAR <- LARcov_t - LARcov_t*BIOcov_t
  # Coverage with biological control  only
  cBIO <- BIOcov_t -  LARcov_t*BIOcov_t
  #both applied
  c_LAR_BIO <-  LARcov_t*BIOcov_t
  # neither applied
  c0_LAR_BIO <- 1 - LARcov_t - BIOcov_t +  LARcov_t*BIOcov_t
  
  #Avoid c0_LAR_BIO to be 0
  if (cLAR == 0 ){
    #browser()
    cLAR  = 2*exp(-16)
  }
  if (cBIO == 0 ){
    #browser()
    cBIO =2*exp(-16) #2*exp(-16)
  }
  if (c_LAR_BIO == 0 ){
    #browser()
    c_LAR_BIO=2*exp(-16)
  }
  if (c0_LAR_BIO == 0 ){
    #browser()
    c0_LAR_BIO =2*exp(-16)
  }
  
  f_LAR_BIO =fLAR*fBIO
  
  
  #browser()
  
  #################**********************************
  ##********************************* ODEs:
  if(time < durEV){
    SVLag <- SV
  }else{
    #Always make sure the lagstates[10] is SV else change 10 to appropriate number
    lagStates <- lagvalue(time-durEV)
    SVLag <- lagStates[10]
  }
  
  dEL   <- c0_LAR_BIO*betaCom*NV - muEL*(1 + ( (EL+LL)/(K*c0_LAR_BIO) ))*EL - EL/durEL
  dLL     <-c0_LAR_BIO*EL/durEL - muLL*(1 + gamma*((EL+LL)/(K*c0_LAR_BIO)))*LL - LL/durLL
  dPL <- (LL+LL_LAR+LL_BIO+LL_LAR_BIO)/durLL - muPL*PL - PL/durPL
  
  ###*************Enable Larvaciding and biological control************
  
  if( (time > time_LAR_on) && (cLAR >0) ){dEL_LAR <- cLAR*betaCom*NV - fLAR*muEL*(1 + ((EL_LAR+LL_LAR)/(cLAR*K)))*EL_LAR - EL_LAR/durEL}else{dEL_LAR<-0}
  if( (time > time_BIO_on) && (cBIO >0) ){dEL_BIO <- cBIO*betaCom*NV - fBIO*muEL*(1 + ((EL_BIO+LL_BIO)/(cBIO*K)))*EL_BIO - EL_BIO/durEL}else{dEL_BIO <-0}
  if( ((time > time_LAR_on) && (cLAR >0)) && ((time > time_BIO_on) && (cBIO >0))){dEL_LAR_BIO <- c_LAR_BIO*betaCom*NV - f_LAR_BIO*muEL*(1 + ((EL_LAR_BIO+LL_LAR_BIO)/(c_LAR_BIO*K)))*EL_LAR_BIO - EL_LAR_BIO/durEL}else{dEL_LAR_BIO<-0}
  
  
  if( (time > time_LAR_on) && (cLAR >0) ){dLL_LAR <-cLAR*EL/durEL - fLAR*muLL*(1 + gamma*((EL_LAR+LL_LAR)/(K*cLAR)))*LL_LAR - LL_LAR/durLL}else{dLL_LAR<-0}
  if( (time > time_BIO_on) && (cBIO >0) ){dLL_BIO <-cBIO*EL/durEL - fBIO*muLL*(1 + gamma*((EL_BIO+LL_BIO)/(K*cBIO)))*LL_BIO - LL_BIO/durLL}else{dLL_BIO<-0}
  if( ((time > time_LAR_on) && (cLAR >0)) && ((time > time_BIO_on) && (cBIO >0))){dLL_LAR_BIO <-c_LAR_BIO*EL/durEL - f_LAR_BIO*muLL*(1 + gamma*((EL_LAR_BIO+LL_LAR_BIO)/(K*c_LAR_BIO)))*LL_LAR_BIO - LL_LAR_BIO/durLL}else{dLL_LAR_BIO<-0}
  
  
  
  #Do we need to LL = LL+LAR+BIO+LAR_BIO????
  
  
  #0.5 only dealing with female mosquitoes
  dSV <- 0.5*PL/durPL - lambdaV*SV - muVCom*SV
  dEV <- lambdaV*SV - lambdaV*SVLag*exp(-muVCom*durEV) - muVCom*EV
  dIV <- lambdaV*SVLag*exp(-muVCom*durEV) - muVCom*IV
  
  #Introducing males
  
  # Get the actual variables
  muMm = muV
  f_SwS = 11
  
  muMmCom = muMm*f_SwS
  
  dMm <- 0.5*PL/durPL - muMmCom*Mm
  
  
  ##*******************SIR model for humans********************************************
  
  dSH <- -(NV/NH)*a_theta*(IV/NV)*bh*SH +  recRate*IH
  dIH <-  (NV/NH)*a_theta*(IV/NV)*bh*SH -  recRate*IH
  
  ##3**********************************************************************
  
  
  #if you add more states - make sure the lagstates[10] is still SV -- SK will automate
  return(list(c(dEL,dEL_LAR,dEL_BIO,dEL_LAR_BIO, dLL,dLL_LAR,dLL_BIO,dLL_LAR_BIO, dPL, dSV, dEV, dIV,dSH,dIH,dMm), EIR=EIR, VC = VC, R0 = R0))
  
}
######################################################################################


