# Do we need to provide range of values? Provide cite where these came from
## Model parameters:
theta <- c(
  ## Mosquito life cycle parameters:
  beta = 21.19, # Number of eggs laid per day by female mosquito
  muEL = 0.034, # Early larval instar daily mortality
  muLL = 0.035, # Late larval instar daily mortality
  muPL = 0.25, # Pupal daily mortality
  durEL = 6.64, # Duration of early instar stage
  durLL = 3.72, # Duration of late instar stage
  durPL = 0.64, # Duration of pupal stage
  durEV = 10, # Duration of latent period in mosquito (days)
  gamma = 13.25, # Effect of density-dependence on late instars relative to early instars
  tau1 = 0.68, # Time spent foraging for a blood meal at 0% ITN coverage
  tau2 = 2.32, # Time spent resting and ovipositing by a mosquito
  ## Intervention parameters (variable):
  ITNcov = 0.8, # ITN coverage
  IRScov = 0.8, # IRS coverave
  time_ITN_on = 50, # When ITNs are applied (days)
  time_IRS_on = 50, # When IRS is applied (days)
  ## Species-specific parameters:
  ## An. gambiae:
  muV = 1/7.6, # Adult mosquito daily mortality
  Q0 = 0.92, # Human blood index
  phiB = 0.89, # Proportion of bites on a person while they are in bed
  phiI = 0.97, # Proportion of bites on a person while they are indoors
  rITN = 0.56, # Probability of mosquito repeating a feeding attempt due to IRS
  sITN = 0.03, # Probability of mosquito feeding and surviving in presence of ITNs
  rIRS = 0.60, # Probability of mosquito repeating a feeding attempt due to IRS
  sIRS = 0, # Probability of mosquito feeding and surviving in presence of IRS
  ## Additional transmission parameters:
  f0 = 1/3, # Daily biting rate by mosquitoes on animals and humans
  epsilon0 = 10/365, # Daily entomological inolculation rate
  iH_eq = 0.35, # Equilibrium malaria prevalence in humans
  NH_eq = 200, # Equilibrium human population size
  bV = 0.05 # Probability of transmission from human to vector per infectious bite
)

getTheta = function(lifeCycleParameters, interventionParameters){
  c(
    ## Mosquito life cycle parameters:
    beta = 21.19, # Number of eggs laid per day by female mosquito
    muEL = 0.034, # Early larval instar daily mortality
    muLL = 0.035, # Late larval instar daily mortality
    muPL = 0.25, # Pupal daily mortality
    durEL = 6.64, # Duration of early instar stage
    durLL = 3.72, # Duration of late instar stage
    durPL = 0.64, # Duration of pupal stage
    durEV = 10, # Duration of latent period in mosquito (days)
    gamma = 13.25, # Effect of density-dependence on late instars relative to early instars
    tau1 = 0.68, # Time spent foraging for a blood meal at 0% ITN coverage
    tau2 = 2.32, # Time spent resting and ovipositing by a mosquito
    ## Additional transmission parameters:
    f0 = 1/3, # Daily biting rate by mosquitoes on animals and humans
    epsilon0 = 10/365, # Daily entomological inolculation rate
    iH_eq = 0.35, # Equilibrium malaria prevalence in humans
    NH_eq = 200, # Equilibrium human population size
    bV = 0.05 # Probability of transmission from human to vector per infectious bite
  )
}

#------------------------------------------------------------------------
############ SPECIES SPECIFIC PARAMETERS ################################
#------------------------------------------------------------------------
# muV   Adult mosquito daily mortality
# Q0    Human blood index
# phiB  Proportion of bites on a person while they are in bed
# phiI  Proportion of bites on a person while they are indoors
# rITN  Probability of mosquito repeating a feeding attempt due to IRS
# sITN  Probability of mosquito feeding and surviving in presence of ITN
# rIRS  Probability of mosquito repeating a feeding attempt due to IRS
# sIRS  Probability of mosquito feeding and surviving in presence of IRS
#------------------------------------------------------------------------
getAnGambiaeParameters = function(){
  c(
    muV = 1/7.6, Q0 = 0.92,
    phiB = 0.89, phiI = 0.97,
    rITN = 0.56, sITN = 0.03,
    rIRS = 0.60, sIRS = 0
  )
}
getAnArabiensisParameters = function(){
  c(
    muV = 1/7.6, Q0 = 0.71,
    phiB = 0.90, phiI = 0.96,
    rITN = 0.48, sITN = 0.39,
    rIRS = 0.60, sIRS = 0
  )
}
getAnFunestusParameters = function(){
  c(
    muV = 1/8.9, Q0 = 0.94,
    phiB = 0.90, phiI = 0.98,
    rITN = 0.56, sITN = 0.03,
    rIRS = 0.63, sIRS = 0
  )
}
#########################################################################


#------------------------------------------------------------------------
############ INTERVENTIONS PARAMETERS ###################################
#------------------------------------------------------------------------
# ITNcov        ITN coverage
# IRScov        IRS coverage
# time_ITN_on   When ITNs are applied (days)
# time_IRS_on   When IRSs are applied (days)
#------------------------------------------------------------------------
getAnGambiaeParameters = function(){
  c(
    ITNcov = 0.8, IRScov = 0.8,
    time_ITN_on = 50, time_IRS_on = 50
  )
}
#########################################################################
#------------------------------------------------------------------------