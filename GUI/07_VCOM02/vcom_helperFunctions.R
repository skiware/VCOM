#######################################################
######VCOM Optimization Helper Functions###############
######Hector Sanchez; edited by Sean Wu 5/19/2016######
#######################################################


############################
###Vector Specific Params###
############################

getAnGambiaeParameters = function(){
  #. getAnGambiaeParameters: Returns the list of Anopheles Gambiae specific parameters
  c(
    muV = 1/7.6, Q0 = 0.92,
    phiB = 0.89, phiI = 0.97,
    rOVI = 0.00, sOVI = 0.00,
    rFOG = 0.00, sFOG = 0.00,
    rLAR = 0.00, sLAR = 0.00,
    rBIO = 0.00, sBIO = 0.00,
    rSRE = 0.00, sSRE = 0.00,
    rIRS = 0.60, sIRS = 0.00,
    rITN = 0.56, sITN = 0.03,
    rIVM = 0.00, sIVM = 0.00,
    rHOU = 0.00, sHOU = 0.00,
    rODO = 0.00, sODO = 0.00,
    rSPA = 0.00, sSPA = 0.00
  )
}

getAnArabiensisParameters = function(){
  #. getAnArabiensisParameters: Returns the list of Anopheles Arabiensis specific parameters
  c(
    muV = 1/7.6, Q0 = 0.71,
    phiB = 0.90, phiI = 0.96,
    rOVI = 0.00, sOVI = 0.00,
    rFOG = 0.00, sFOG = 0.00,
    rLAR = 0.00, sLAR = 0.00,
    rBIO = 0.00, sBIO = 0.00,
    rSRE = 0.00, sSRE = 0.00,
    rIRS = 0.60, sIRS = 0.00,
    rITN = 0.56, sITN = 0.03,
    rIVM = 0.00, sIVM = 0.00,
    rHOU = 0.00, sHOU = 0.00,
    rODO = 0.00, sODO = 0.00,
    rSPA = 0.00, sSPA = 0.00
  )
}

getAnFunestusParameters = function(){
  #. getAnFunestusParameters: Returns the list of Anopheles Funestus specific parameters
  c(
    muV = 1/8.9, Q0 = 0.94,
    phiB = 0.90, phiI = 0.98,
    rOVI = 0.00, sOVI = 0.00,
    rFOG = 0.00, sFOG = 0.00,
    rLAR = 0.00, sLAR = 0.00,
    rBIO = 0.00, sBIO = 0.00,
    rSRE = 0.00, sSRE = 0.00,
    rIRS = 0.60, sIRS = 0.00,
    rITN = 0.56, sITN = 0.03,
    rIVM = 0.00, sIVM = 0.00,
    rHOU = 0.00, sHOU = 0.00,
    rODO = 0.00, sODO = 0.00,
    rSPA = 0.00, sSPA = 0.00
  )
}

getMosquitoLifeCycleParameters = function(){
  #. getMosquitoLifeCycleParameters: Returns the list of mosquito life cycle parameters
  c(
    beta = 21.19,
    muEL = 0.034, muLL = 0.035, muPL = 0.25,
    durEL = 6.64, durLL = 3.72, durPL = 0.64, durEV = 10,
    gamma = 13.25, tau1 = 0.68, tau2 = 2.32
  )
}


######################
###Model Parameters###
######################


#function to return intervention coverage [0,1]; WHEN is integer specifying when that intervention is turned on
getInterventionsParameters <- function(OVI=0,FOG=0,LAR=0,BIO=0,SRE=0,IRS=0,ITN=0,IVM=0,HOU=0,ODO=0,SPA=0,WHEN=10){
  parms <-  c(
    OVIcov = OVI, time_OVI_on = 0,
    FOGcov = FOG, time_FOG_on = 0,
    LARcov = LAR, time_LAR_on = 0,
    BIOcov = BIO, time_BIO_on = 0,
    SREcov = SRE, time_SRE_on = 0,
    IRScov = IRS, time_IRS_on = 0,
    ITNcov = ITN, time_ITN_on = 0,
    IVMcov = IVM, time_IVM_on = 0,
    HOUcov = HOU, time_HOU_on = 0,
    ODOcov = ODO, time_ODO_on = 0,
    SPAcov = SPA, time_SPA_on = 0
  )
  if(any(parms != 0)){
    parms_index <- unname(which(parms != 0))
    parms_index <- parms_index + 1
    parms[parms_index] <- WHEN
  }
  return(parms)
}

getAdditionalTransmissionParameters = function(){
  #. getAdditionalTransmissionParameters: Returns the transmission parameters
  c(
    f0 = 1/3, epsilon0 = 10/365, 
    iH_eq = 0.35, NH_eq = 2000, 
    bV = 0.05
  )
}

getTheta = function(
  #. getTheta: Facade function to return theta from selected parameters
  speciesSpecificParameters=getAnGambiaeParameters(), 
  interventionParameters=getInterventionsParameters(),
  additionalTransmissionParameters=getAdditionalTransmissionParameters(), 
  mosquitoLifeCycleParameters=getMosquitoLifeCycleParameters()
){
  c(
    beta = mosquitoLifeCycleParameters[["beta"]],
    muEL = mosquitoLifeCycleParameters[["muEL"]],
    muLL = mosquitoLifeCycleParameters[["muLL"]],
    muPL = mosquitoLifeCycleParameters[["muPL"]],
    durEL = mosquitoLifeCycleParameters[["durEL"]],
    durLL = mosquitoLifeCycleParameters[["durLL"]],
    durPL = mosquitoLifeCycleParameters[["durPL"]],
    durEV = mosquitoLifeCycleParameters[["durEV"]],
    gamma = mosquitoLifeCycleParameters[["gamma"]],
    tau1 = mosquitoLifeCycleParameters[["tau1"]],
    tau2 = mosquitoLifeCycleParameters[["tau2"]],
    ITNcov = interventionParameters[["ITNcov"]],
    IRScov = interventionParameters[["IRScov"]],
    time_ITN_on = interventionParameters[["time_ITN_on"]],
    time_IRS_on = interventionParameters[["time_IRS_on"]],
    muV = speciesSpecificParameters[["muV"]],
    Q0 = speciesSpecificParameters[["Q0"]],
    phiB = speciesSpecificParameters[["phiB"]],
    phiI = speciesSpecificParameters[["phiI"]],
    rITN = speciesSpecificParameters[["rITN"]],
    sITN = speciesSpecificParameters[["sITN"]],
    rIRS = speciesSpecificParameters[["rIRS"]],
    sIRS = speciesSpecificParameters[["sIRS"]],
    f0 = additionalTransmissionParameters[["f0"]],
    epsilon0 = additionalTransmissionParameters[["epsilon0"]],
    iH_eq = additionalTransmissionParameters[["iH_eq"]],
    NH_eq = additionalTransmissionParameters[["NH_eq"]],
    bV = additionalTransmissionParameters[["bV"]],
    NV_eq = 0,
    lambdaV =0
  )
}

#function that returns a named list of theta and initState
initParameters <- function(theta){
  beta <- theta[["beta"]]; muEL <- theta[["muEL"]]; muLL <- theta[["muLL"]]
  muPL <- theta[["muPL"]]; muV <- theta[["muV"]]; durEL <- theta[["durEL"]]
  durLL <- theta[["durLL"]]; durPL <- theta[["durPL"]]; durEV <- theta[["durEV"]]
  gamma <- theta[["gamma"]]; Q0 <- theta[["Q0"]]; f0 <- theta[["f0"]]
  epsilon0 <- theta[["epsilon0"]]; iH_eq <- theta[["iH_eq"]]
  NH_eq <- theta[["NH_eq"]]; bV <- theta[["bV"]]
  
  b_omega <- gamma*muLL/muEL - durEL/durLL + (gamma-1)*muLL*durEL
  omega <- -0.5*b_omega + sqrt(0.25*b_omega^2 + gamma*beta*muLL*durEL/(2*muEL*muV*durLL*(1+durPL*muPL)))
  a0 <- Q0*f0 # Human biting rate at equilibrium
  
  lambdaV <- a0*iH_eq*bV # Force of infection in mosquitoes at equilibrium
  theta[["lambdaV"]] <- lambdaV # Include vector force of infection in vector of parameters (theta)
  
  iV_eq <- lambdaV*exp(-muV*durEV)/(lambdaV + muV)
  sV_eq <- iV_eq*muV/(lambdaV*exp(-muV*durEV))
  eV_eq <- 1 - sV_eq - iV_eq
  
  NV_eq <- epsilon0*NH_eq/(iV_eq*a0)
  theta[["NV_eq"]] <- NV_eq # Include equilibrium vector population size in vector of parameters (theta)
  
  EL_eq <- 2*omega*muV*durLL*(1 + muPL*durPL)*NV_eq
  LL_eq <- 2*muV*durLL*(1 + muPL*durPL)*NV_eq
  PL_eq <- 2*muV*durPL*NV_eq
  SV_eq <- sV_eq*NV_eq
  EV_eq <- eV_eq*NV_eq
  IV_eq <- iV_eq*NV_eq
  
  initState <- c(
    EL = EL_eq,
    LL = LL_eq,
    PL = PL_eq, 
    SV = SV_eq, 
    EV = EV_eq, 
    IV = IV_eq
  )
  return(list(initState=initState,theta=theta))
}


###########################
###ODE Model and Wrapper###
###########################


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
  ## Add other interventions - SK
  ## Add other scenarios e.g., probability of dying after feeding FOR each intervention - SK
  ## States:  - Defn added by SK
  EL <- state[["EL"]]  # Early Instar stage
  LL <- state[["LL"]]  # Late Instar stage
  PL <- state[["PL"]]  # Pupal stage
  SV <- state[["SV"]]  # Susceptitable Vectors
  EV <- state[["EV"]]  # Latent period
  IV <- state[["IV"]]  # Infectious Vectors
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
  if (time > time_ITN_on) { ITNcov_t <- ITNcov } else { ITNcov_t <- 0 }
  if (time > time_IRS_on) { IRScov_t <- IRScov } else { IRScov_t <- 0 }
  # zCom: Probability of a mosquito being repelled from an ITN or IRS-treated house:
  #Equation 4
  # Unprotected proportion (SK
  c0 <- 1 - ITNcov_t - IRScov_t + ITNcov_t*IRScov_t
  #ITN protection (SK) Equation 4a
  cITN <- ITNcov_t - ITNcov_t*IRScov_t
  # IRS protection (SK) - Equation 4b
  cIRS <- IRScov_t - ITNcov_t*IRScov_t
  #ITN & IRS combined protection (SK) - Equation 4c
  cCom <- ITNcov_t*IRScov_t
  # repeating due to IRS
  rCom <- rIRS + (1-rIRS)*rITN
  sCom  <- (1-rIRS)*sITN*sIRS
  #Equation 6
  #zcom new search probability after a mosq is repelled (SK
  zCom <- Q0*cITN*phiB*rITN + Q0*cIRS*phiI*rIRS + Q0*cCom*(phiI-phiB)*rIRS + Q0*cCom*phiB*rCom 
  #Equation 7
  # deltaCom: Inverse of gonotrophic cycle length with ITNs & IRS: (SK equation 2)
  deltaCom <- 1/(tau1/(1-zCom) + tau2)
  # Equation 8
  # wCom: Probability that a surviving mosquito succeeds in feeding during a single attempt:
  #Need to adjust for repelency in IRS (1-rIRS) SK?
  #given by surving cattle (1-Q0), unprotected human (Q0C0), and protected human
  wCom <- 1 - Q0 + Q0*c0 + Q0*cITN*(1-phiB+phiB*sITN) + Q0*cIRS*(1-phiI+phiI*sIRS) + Q0*cCom*((phiI-phiB)*sIRS + 1-phiI + phiB*sCom)
  ## Add cattle treated with ivermectin
  Cc = 0.8   # prop of cattle treated
  #Cattlecov <- theta[["Cattlecov"]] # Prop of cattle treated
  wCom <- (1 - Q0)*(1-Cc) + Q0*c0 + Q0*cITN*(1-phiB+phiB*sITN) + Q0*cIRS*(1-phiI+phiI*sIRS) + Q0*cCom*((phiI-phiB)*sIRS + 1-phiI + phiB*sCom)
  # muVCom: Female mosquito death rate in presence of ITNs & IRS:
  #Equation 1a
  # probability of surviving feeding period in the absence of an intervetion (SK)
  p10 <- exp(-muV*tau1)
  #Equation 9
  # Probability of surviving the first attempt, second, and so on -eqn 3 (SK)
  p1Com <- p10*wCom/(1 - zCom*p10)
  #Equation 1b
  #probabbility of resting (SK)
  p2 <- exp(-muV*tau2)
  #Equation 10
  #probability of surviving one day (SK)
  pCom <- (p1Com*p2)^deltaCom # SAme as eqn 4 in Menach (SK)
  #Need to add lambda the rate at which mosq emerge per human per day (constant value) - (SK)
  #Equation 11
  # female mosquito mortality rate due to ITN and IRS (SK)
  muVCom <- -log(pCom)
  #Equation 13
  # betaCom: Eggs laid per day by female mosquitoes in presence of ITNs & IRS:
  betaCom <- e_ov*muVCom/(exp(muVCom/deltaCom) - 1)
  # SK - ADD the following output
  # Vectorial capacity
  # HBO
  # EIR
  # Sam, add delay explanation in the write-up
  ## ODEs:
  if (time < durEV) {
    SVLag <- SV
  } else {
    lagStates <- lagvalue(time-durEV)
    SVLag <- lagStates[4]
  }
  ## The impact is observed thoughout since the number of eggs is adjusted depending on interventions (SK)
  #Also the muv is now muvCom to incorporate intervention impact (SK)
  ## Moving out of Early stage  -- number of eggs can be affected by an intervention
  dEL <- betaCom*NV - muEL*(1 + ((EL+LL)/K))*EL - EL/durEL
  dLL <- EL/durEL - muLL*(1 + gamma*((EL+LL)/K))*LL - LL/durLL
  dPL <- LL/durLL - muPL*PL - PL/durPL
  #0.5 only dealing with female mosquitoes
  dSV <- 0.5*PL/durPL - lambdaV*SV - muVCom*SV
  dEV <- lambdaV*SV - lambdaV*SVLag*exp(-muVCom*durEV) - muVCom*EV
  dIV <- lambdaV*SVLag*exp(-muVCom*durEV) - muVCom*IV
  return(list(c(dEL, dLL, dPL, dSV, dEV, dIV))) 
}

#wrapper function, tMax is integer, initParms is a named list (output from initParameters())
runVCOM <- function(tMax,initParms,method="lsoda"){
  
  #check input
  if(!is.character(method)){
    stop("method should be a character!")
  }
  
  #run integrator
  times <- seq_len(tMax)-1
  mod_out <- data.frame(dede(y=initParms$initState,times=times,parms=initParms$theta,func=IVM_ode))
  return(mod_out)
}