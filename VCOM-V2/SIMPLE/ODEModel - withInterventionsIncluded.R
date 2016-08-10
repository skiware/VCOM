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
  NH_eq <- theta[["NH_eq"]]; bV <- theta[["bV"]]
  
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
  
  initState <- c(EL = EL_eq,LL = LL_eq,PL = PL_eq,SV = SV_eq,EV = EV_eq,IV = IV_eq)
  
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
 
  ## endocticide-treated cattle (either topical or systemic)
  ECScov <- theta[["ECScov"]] # endocticide (e.g., ivermectin) coverage - systemic coverage
  time_ECS_on <- theta[["time_ECS_on"]] # When cattle are treated (systemic) (days)
 # time_ENDO_of <- theta[["time_ENDO_of"]] # When ivermectin is no longer effective (days)
  sECS <- theta[["sECS"]] # Probability of mosquito feeding and surviving in presence of systemic-treated cattle
  
  ECTcov <- theta[["ECTcov"]] # endocticide (e.g., ivermectin) coverage - topical coverage
  time_ECT_on <- theta[["time_ECT_on"]] # When cattle are treated (topical) (days)
  sECT <- theta[["sECT"]] # Probability of mosquito feeding and surviving in presence of topical-treated cattle
  rECT <- theta[["rECT"]] # Probability of mosquito repeating a feeding attempt due to topical-treated cattle
  
  
  # Moden house
  #HMcov <- theta[["HMcov"]] # proportion of houses that are mosquito proofed housing
  #rHM <- theta[["rHM"]] # Probability of mosquito repeating a feeding attempt due to a mosquito proofed housing
  #sHM <- theta[["sHM"]] # Probability of mosquito feeding and surviving in presence of mosquito proofed housing
  
  
  
  
  
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
  
  ##*************************Human - Indoor protection *********************************##
  # if (time > time_ITN_on) { ITNcov_t <- ITNcov } else { ITNcov_t <- 0 }
  # if (time > time_IRS_on) { IRScov_t <- IRScov } else { IRScov_t <- 0 }
  # 
  # # Unprotected proportion (SK
  # c0 <- 1 - ITNcov_t - IRScov_t + ITNcov_t*IRScov_t
  # #ITN protection (SK) Equation 4a
  # cITN <- ITNcov_t - ITNcov_t*IRScov_t
  # # IRS protection (SK) - Equation 4b
  # cIRS <- IRScov_t - ITNcov_t*IRScov_t
  # #ITN & IRS combined protection (SK) - Equation 4c
  # cCom <- ITNcov_t*IRScov_t
  # # repeating due to IRS
  # rCom <- rIRS + (1-rIRS)*rITN
  # sCom  <- (1-rIRS)*sITN*sIRS
  # 
  # #Human - new search probability after a mosq is repelled (SK) by indoor interventions
  # zCom_Human <- Q0*cITN*phiB*rITN + Q0*cIRS*phiI*rIRS + Q0*cCom*(phiI-phiB)*rIRS + Q0*cCom*phiB*rCom
  # 
  # # Human - Probability that a surviving mosquito succeeds in feeding during a single attempt:##
  # wComHuman <- 1 - Q0 + Q0*c0 + Q0*cITN*(1-phiB+phiB*sITN) + Q0*cIRS*(1-phiI+phiI*sIRS) + Q0*cCom*((phiI-phiB)*sIRS + 1-phiI + phiB*sCom)
  
  ## Initialize the model
  impactIndoor <<- impactIndoorInterventions(time,time_ITN_on,ITNcov,time_IRS_on,IRScov,rITN,sITN,rIRS,sIRS,Q0, phiB, phiI)
  zCom_Human = impactIndoor[1]
  wComHuman = impactIndoor[2]
  
  ##*************************** Cattle *********************************************##
  if (time > time_ECS_on) { ECScov_t <- ECScov } else { ECScov_t <- 0 }
  if (time > time_ECT_on) { ECTcov_t <- ECTcov } else { ECTcov_t <- 0 }
  
  #Coverage with systemic insecticide only
  cECS <- ECScov_t - ECScov_t*ECTcov_t
  # Coverage with topical insecticide only
  cECT <- ECTcov_t - ECScov_t*ECTcov_t
  #both applied
  cCom_Cattle <- ECScov_t*ECTcov_t
  # neither applied
  c0_Cattle <- 1 - ECScov_t - ECTcov_t + ECScov_t*ECTcov_t
  
  # repeating due to encountering insecticide (topical applied (only)) treated cattle
  #Human - new search probability after a mosq is repelled (SK) by insecticide treated cattle (topical)
  zCom_Cattle <- (1 - Q0)*(cECT*rECT+cCom_Cattle*rECT)
  
  #Add cattle treated with endocticide (systemic and/or topical applied)
  wComCattle <- (1 - Q0)*(c0_Cattle+cECS*sECS+cECT*sECT+cCom_Cattle*sECS*sECT)
  
  ######**************************************************************************#####
  
  ## zCom: Probability of a mosquito being repelled : SAM CHECK THIS
  zCom <- zCom_Cattle + zCom_Human
  # deltaCom: Inverse of gonotrophic cycle length with ITNs & IRS: (SK equation 2)
  deltaCom <- 1/(tau1/(1-zCom) + tau2)
  
  ### wCom: Probability that a surviving mosquito succeeds in feeding during a single attempt:##
  wCom <- wComCattle + wComHuman
  #******************************************************************************************
  ## muVCom: Female mosquito death rate in presence of ITNs & IRS:
  
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
######################################################################################