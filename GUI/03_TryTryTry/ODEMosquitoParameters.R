# Do we need to provide range of values? Provide cite where these came from
## Model parameters:
getTheta = function(
                    speciesSpecificParameters=getAnGambiaeParameters(), 
                    interventionParameters=getInterventionsParameters(),
                    additionalTransmissionParameters=getAdditionalTransmissionParameters(), 
                    mosquitoLifeCycleParameters=getMosquitoLifeCycleParameters()
                  ){
  #Facade for parameters selection
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
    bV = additionalTransmissionParameters[["bV"]]
  )
}
######################################################################################
#------------------------------------------------------------------------
############ SPECIES SPECIFIC PARAMETERS ################################
#------------------------------------------------------------------------
#*  muV     Adult mosquito daily mortality
#* Q0      Human blood index
#* phiB    Proportion of bites on a person while they are in bed
#* phiI    Proportion of bites on a person while they are indoors
#* rITN    Probability of mosquito repeating a feeding attempt due to IRS
#* sITN    Probability of mosquito feeding and surviving in presence of ITN
#* rIRS    Probability of mosquito repeating a feeding attempt due to IRS
#* sIRS    Probability of mosquito feeding and surviving in presence of IRS
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
######################################################################################
#------------------------------------------------------------------------
############ INTERVENTIONS PARAMETERS ###################################
#------------------------------------------------------------------------
#* ITNcov        ITN coverage
#* IRScov        IRS coverage
#* time_ITN_on   When ITNs are applied (days)
#* time_IRS_on   When IRSs are applied (days)
#------------------------------------------------------------------------
getInterventionsParameters = function(){
  c(
    ITNcov = 0.8, IRScov = 0.8,
    time_ITN_on = 50, time_IRS_on = 50
  )
}
######################################################################################
#------------------------------------------------------------------------
############ MOSQUITO LIFE CYCLE PARAMETERS #############################
#------------------------------------------------------------------------
#* beta    Number of eggs laid per day per female mosquito
#* muEL    Early larval instar daily mortality
#* muLL    Late larvarl instar daily mortality
#* muPL    Pupal daily mortality
#* durEL   Duration of early instar stage
#* durLL   Duration of late instar stage
#* durPL   Pupal daily mortality
#* durPL   Duration of pupal stage
#* durEV   Duration of latent period in mosquito (days)
#* gamma   Effect of density-dependence on late instars relative to early instars
#* tau1    Time spent foraginf for a blood meal at 0% ITN coverage
#* tau2    Time spent resting and ovipositing by a mosquito
#------------------------------------------------------------------------
getMosquitoLifeCycleParameters = function(){
  c(
    beta = 21.19,
    muEL = 0.034, muLL = 0.035, muPL = 0.25,
    durEL = 6.64, durLL = 3.72, durPL = 0.64, durEV = 10,
    gamma = 13.25, tau1 = 0.68, tau2 = 2.32
  )
}
######################################################################################
#------------------------------------------------------------------------
############ ADDITIONAL TRANSMISSION PARAMETERS #########################
#------------------------------------------------------------------------
#* f0        Daily biting rate by mosquitoes on animals and humans
#* epsilon0  Daily entomological inolculation rate
#* iH_eq     Equilibrium malaria prevalence in humans
#* NH_eq     Equilibrium human population size
#* bV        Probability of transmission from human to vector per infectious bite
#------------------------------------------------------------------------
getAdditionalTransmissionParameters = function(){
  c(
    f0 = 1/3, epsilon0 = 10/365, 
    iH_eq = 0.35, NH_eq = 200, 
    bV = 0.05
  )
}
######################################################################################