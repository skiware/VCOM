#############################################
######Initialization functions for VCOM######
######Sean Wu 8/19/2016######################
#############################################


#################################################
###Function to return model parameters for run###
#################################################

vcom_initialization <- function(epsilon0=1,species=1,  
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
                                SSPcov=0,   time_SSP_on=0) {
  
  #retrieve intervention parameters
  intervention_parameters <- getInterventionsParameters(  OVIcov,   time_OVI_on,
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
  
  mosquito_parameters <- getMosquitoParameters(species=species,epsilon0=epsilon0)
  
  theta <- getTheta(speciesSpecificParameters=mosquito_parameters$speciesSpecificParameters, 
                    interventionParameters=intervention_parameters,
                    additionalTransmissionParameters=mosquito_parameters$additionalTransmissionParameters, 
                    mosquitoLifeCycleParameters=mosquito_parameters$mosquitoLifeCycleParameters)
  
  initStateCalculation <- calculateInitialState(theta=theta)
  initState <- initStateCalculation$initState
  theta[["NV_eq"]] <- initStateCalculation$thetaUpdate[["NV_eq"]]
  theta[["lambdaV"]] <- initStateCalculation$thetaUpdate[["lambdaV"]]
  return(list(theta=theta,initState=initState))
}



########################################################
###Helper functions to return initial state and theta###
########################################################

###return initial intervention parameters###
getInterventionsParameters = function(
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
  SSPcov,   time_SSP_on
  
){
  c(
    OVIcov=OVIcov,time_OVI_on=time_OVI_on,
    FOGcov=FOGcov,time_FOG_on=time_FOG_on,
    LARcov=LARcov,time_LAR_on=time_LAR_on,
    BIOcov=BIOcov,time_BIO_on=time_BIO_on,
    SREcov=SREcov,time_SRE_on=time_SRE_on,
    IRScov=IRScov,time_IRS_on=time_IRS_on,
    ITNcov=ITNcov,time_ITN_on=time_ITN_on,
    ECScov=ECScov,time_ECS_on=time_ECS_on,
    ECTcov=ECTcov,time_ECT_on=time_ECT_on,
    HOUcov=HOUcov,time_HOU_on=time_HOU_on,
    OBTcov=OBTcov,time_OBT_on=time_OBT_on,
    SPRcov=SPRcov,time_SPR_on=time_SPR_on,
    PPMcov=PPMcov,time_PPM_on=time_PPM_on,
    ATSBcov=ATSBcov,time_ATSB_on=time_ATSB_on,
    SSPcov=SSPcov,time_SSP_on=time_SSP_on
  )
}


#######################
###Initialize States###
#######################

###calculate initial states and update theta###
calculateInitialState = function(theta){

  beta <- theta[["beta"]]; muEL <- theta[["muEL"]]; muLL <- theta[["muLL"]]
  muPL <- theta[["muPL"]]; muV <- theta[["muV"]]; durEL <- theta[["durEL"]]
  durLL <- theta[["durLL"]]; durPL <- theta[["durPL"]]; durEV <- theta[["durEV"]]
  gamma <- theta[["gamma"]]; Q0 <- theta[["Q0"]]; f0 <- theta[["f0"]]
  epsilon0 <- theta[["epsilon0"]]; recRate <- theta[["recRate"]]
  NH_eq <- theta[["NH_eq"]]; bV <- theta[["bV"]];bh <- theta[["bh"]]
  
  b_omega <- gamma*muLL/muEL - durEL/durLL + (gamma-1)*muLL*durEL
  omega <- -0.5*b_omega + sqrt(0.25*b_omega^2 + gamma*beta*muLL*durEL/(2*muEL*muV*durLL*(1+durPL*muPL)))
  a0 <- Q0*f0 # Human biting rate at equilibrium
  
  #Incorporating humans
  iH_eq <- (epsilon0 *bh)/(recRate+(epsilon0*bh))
  IH_eq <- iH_eq*NH_eq 
  SH_eq = NH_eq* (1-iH_eq)
  
  lambdaV <- a0*iH_eq*bV # Force of infection in mosquitoes at equilibrium

  iV_eq <- lambdaV*exp(-muV*durEV)/(lambdaV + muV)
  sV_eq <- iV_eq*muV/(lambdaV*exp(-muV*durEV))
  eV_eq <- 1 - sV_eq - iV_eq
  
  NV_eq <- epsilon0*NH_eq/(iV_eq*a0) # Include equilibrium vector population size in vector of parameters (theta)
  
  EL_eq <- 2*omega*muV*durLL*(1 + muPL*durPL)*NV_eq
  LL_eq <- 2*muV*durLL*(1 + muPL*durPL)*NV_eq
  PL_eq <- 2*muV*durPL*NV_eq
  SV_eq <- sV_eq*NV_eq
  EV_eq <- eV_eq*NV_eq
  IV_eq <- iV_eq*NV_eq
  
  initState <- c(EL = EL_eq,EL_LAR = 0,EL_BIO = 0,EL_LAR_BIO = 0,LL = LL_eq,LL_LAR = 0,LL_BIO = 0,LL_LAR_BIO = 0,PL = PL_eq,SV = SV_eq,EV = EV_eq,IV = IV_eq, SH = SH_eq, IH = IH_eq, Mm = SV_eq)
  thetaUpdate <- c(lambdaV=lambdaV,NV_eq=NV_eq)
  
  return(list(initState=initState,thetaUpdate=thetaUpdate))
}


####################################
###Initialize Mosquito Parameters###
####################################

###get mosquito parameters (this incorporates additionalTransmissionParameters, speciesSpecificParameters, mosquitoLifeCycleParameters)###
getMosquitoParameters <- function(species,epsilon0){
  if(species==1){
    species_param <- getAnGambiaeParameters()
  }
  if(species==2){
    species_param <- getAnArabiensisParameters()
  }
  if(species==3){
    species_param <- getAnFunestusParameters()
  }
  additional_param <- getAdditionalTransmissionParameters(epsilon0=epsilon0)
  mosq_param <- getMosquitoLifeCycleParameters()
  
  return(list(speciesSpecificParameters=species_param,
              additionalTransmissionParameters=additional_param,
              mosquitoLifeCycleParameters=mosq_param))
}


getAdditionalTransmissionParameters = function(
  #For getting low, medium, high transmission
  f0=1/3,epsilon0,recRate=1/50,
  NH_eq=2000,bV=0.05,bh=0.5   #bv = originally 0.05
){
  #. getAdditionalTransmissionParameters: Returns the transmission parameters
  c(
    f0=f0,epsilon0=epsilon0/365, 
    recRate=recRate,NH_eq=NH_eq, 
    bV=bV,bh=bh
  )
}

getMosquitoLifeCycleParameters = function(
  beta=21.19,muEL=0.034,muLL=0.035,muPL=0.25,
  durEL=6.64,durLL=6.64,durPL=0.64,durEV=10,gamma=13.25,
  tau1=0.68,tau2=2.32
){
  #. getMosquitoLifeCycleParameters: Returns the list of mosquito life cycle parameters
  c(
    beta=beta,
    muEL=muEL,muLL=muLL,muPL=muPL,
    durEL=durEL,durLL=durLL,durPL=durPL,durEV=durEV,
    gamma=gamma,tau1=tau1,tau2=tau2
  )
}

getAnGambiaeParameters = function(){
  #. getAnGambiaeParameters: Returns the list of Anopheles Gambiae specific parameters
  c(
    muV = 1/7.6, Q0 = 0.92,   phiB = 0.89, phiI = 0.97,
    rOVI = 0.00, sOVI = 0.00, rFOG = 0.00, sFOG = 0.00,
    rLAR = 0.00, sLAR = 0.00, rBIO = 0.00, sBIO = 0.00,
    rSRE = 0.00, sSRE = 0.00, rIRS = 0.60, sIRS = 0.00,
    rITN = 0.56, sITN = 0.03, rECS = 0.00, sECS = 0.40,
    rHOU = 0.00, sHOU = 0.50, rODO = 0.00, sODO = 0.00,
    rSPR = 0.00, sSPR = 0.00, rECT = 0.30, sECT = 0.40,
    rPPM = 0.00, sPPM = 0.00, dHOU = 0.10, dIRS = 0.10,
    aOBT = 2.00, eSRE = 0.70, fATSB =1.5, fSSP =  1.50,
    fOVI = 1.50, fLAR = 55.5, fBIO = 35.5
  )
}
getAnArabiensisParameters = function(){
  #. getAnArabiensisParameters: Returns the list of Anopheles Arabiensis specific parameters
  c(
    #Phi = 0.96, phiB = 90 ;;;modified get the actual value
    #muV = 1/7.6, Q0 = 0.71,   phiB = 0.50, phiI = 0.50,
    muV = 1/7.6, Q0 = 0.71,   phiB = 0.90, phiI = 0.96,
    rOVI = 0.00, sOVI = 0.00, rFOG = 0.00, sFOG = 0.00,
    rLAR = 0.00, sLAR = 0.00, rBIO = 0.00, sBIO = 0.00,
    rSRE = 0.00, sSRE = 0.00, rIRS = 0.60, sIRS = 0.00,
    rITN = 0.56, sITN = 0.03, rECS = 0.50, sECS = 0.50,
    rHOU = 0.80, sHOU = 0.15, rODO = 0.00, sODO = 0.00,
    rSPR = 0.80, sSPR = 0.15, rECT = 0.50, sECT = 0.50,
    rPPM = 0.50, sPPM = 0.40, dHOU = 0.10, dIRS = 0.10,
    aOBT = 2.00, eSRE = 0.70, fATSB =1.50, fSSP = 1.50,
    fOVI = 1.50, fLAR = 55.5, fBIO = 15.5
  )
}
getAnFunestusParameters = function(){
  #. getAnFunestusParameters: Returns the list of Anopheles Funestus specific parameters
  c(
    muV = 1/8.9, Q0 = 0.94, phiB = 0.90, phiI = 0.98,
    rOVI = 0.00, sOVI = 0.00, rFOG = 0.00, sFOG = 0.00,
    rLAR = 0.00, sLAR = 0.00, rBIO = 0.00, sBIO = 0.00,
    rSRE = 0.00, sSRE = 0.00, rIRS = 0.60, sIRS = 0.00,
    rITN = 0.56, sITN = 0.03, rECS = 0.60, sECS = 0.60,
    rHOU = 0.00, sHOU = 0.00, rODO = 0.00, sODO = 0.00,
    rSPR = 0.00, sSPR = 0.00, rECT = 0.60, sECT = 0.60,
    rPPM = 0.00, sPPM = 0.00, dHOU = 0.10, dIRS = 0.10,
    aOBT = 2.00, eSRE = 0.70, fATSB =1.50, fSSP = 1.50,
    fOVI = 1.50, fLAR = 55.5, fBIO = 35.5
  )
}


######################
###Initialize Theta###
######################

getTheta = function(
  speciesSpecificParameters, 
  interventionParameters,
  additionalTransmissionParameters, 
  mosquitoLifeCycleParameters
){
  c(
    beta        = mosquitoLifeCycleParameters[["beta"]],
    muEL        = mosquitoLifeCycleParameters[["muEL"]],
    muLL        = mosquitoLifeCycleParameters[["muLL"]],
    muPL        = mosquitoLifeCycleParameters[["muPL"]],
    durEL       = mosquitoLifeCycleParameters[["durEL"]],
    durLL       = mosquitoLifeCycleParameters[["durLL"]],
    durPL       = mosquitoLifeCycleParameters[["durPL"]],
    durEV       = mosquitoLifeCycleParameters[["durEV"]],
    gamma       = mosquitoLifeCycleParameters[["gamma"]],
    tau1        = mosquitoLifeCycleParameters[["tau1"]],
    tau2        = mosquitoLifeCycleParameters[["tau2"]],
    ITNcov      = interventionParameters[["ITNcov"]],
    IRScov      = interventionParameters[["IRScov"]],
    ECScov      = interventionParameters[["ECScov"]],
    ECTcov      = interventionParameters[["ECTcov"]],
    HOUcov      = interventionParameters[["HOUcov"]],
    SPRcov      = interventionParameters[["SPRcov"]],
    PPMcov      = interventionParameters[["PPMcov"]],
    OBTcov      = interventionParameters[["OBTcov"]],
    SREcov      = interventionParameters[["SREcov"]],
    BIOcov      = interventionParameters[["BIOcov"]],
    LARcov      = interventionParameters[["LARcov"]],
    ATSBcov     = interventionParameters[["ATSBcov"]],
    SSPcov      = interventionParameters[["SSPcov"]],
    OVIcov      = interventionParameters[["OVIcov"]],
    time_ITN_on = interventionParameters[["time_ITN_on"]],
    time_IRS_on = interventionParameters[["time_IRS_on"]],
    time_HOU_on = interventionParameters[["time_HOU_on"]],
    time_ECS_on = interventionParameters[["time_ECS_on"]],
    time_ECT_on = interventionParameters[["time_ECT_on"]],
    time_SPR_on = interventionParameters[["time_SPR_on"]],
    time_PPM_on = interventionParameters[["time_PPM_on"]],
    time_OBT_on = interventionParameters[["time_OBT_on"]],
    time_BIO_on = interventionParameters[["time_BIO_on"]],
    time_SRE_on = interventionParameters[["time_SRE_on"]],
    time_LAR_on = interventionParameters[["time_LAR_on"]],
    time_OVI_on = interventionParameters[["time_OVI_on"]],
    time_ATSB_on = interventionParameters[["time_ATSB_on"]],   #Need to use 3 letters, lol
    time_SSP_on = interventionParameters[["time_SSP_on"]],
    muV         = speciesSpecificParameters[["muV"]],
    Q0          = speciesSpecificParameters[["Q0"]],
    phiB        = speciesSpecificParameters[["phiB"]],
    phiI        = speciesSpecificParameters[["phiI"]],
    rITN        = speciesSpecificParameters[["rITN"]],
    sITN        = speciesSpecificParameters[["sITN"]],
    rIRS        = speciesSpecificParameters[["rIRS"]],
    sIRS        = speciesSpecificParameters[["sIRS"]],
    rHOU        = speciesSpecificParameters[["rHOU"]],
    sHOU        = speciesSpecificParameters[["sHOU"]],
    rSPR        = speciesSpecificParameters[["rSPR"]],
    sSPR        = speciesSpecificParameters[["sSPR"]],
    rPPM        = speciesSpecificParameters[["rPPM"]],
    sPPM        = speciesSpecificParameters[["sPPM"]],
    rECS        = speciesSpecificParameters[["rECS"]],
    sECS        = speciesSpecificParameters[["sECS"]],
    rECT        = speciesSpecificParameters[["rECT"]],
    sECT        = speciesSpecificParameters[["sECT"]],
    dIRS        = speciesSpecificParameters[["dIRS"]],
    dHOU        = speciesSpecificParameters[["dHOU"]],
    aOBT        = speciesSpecificParameters[["aOBT"]],
    eSRE        = speciesSpecificParameters[["eSRE"]],
    fBIO        = speciesSpecificParameters[["fBIO"]],
    fLAR        = speciesSpecificParameters[["fLAR"]],
    fATSB       = speciesSpecificParameters[["fATSB"]],
    fSSP        = speciesSpecificParameters[["fSSP"]],
    fOVI        = speciesSpecificParameters[["fOVI"]],
    f0          = additionalTransmissionParameters[["f0"]],
    epsilon0    = additionalTransmissionParameters[["epsilon0"]],
    recRate     = additionalTransmissionParameters[["recRate"]],
    NH_eq       = additionalTransmissionParameters[["NH_eq"]],
    bV          = additionalTransmissionParameters[["bV"]],
    bh          = additionalTransmissionParameters[["bh"]]
  )
}
