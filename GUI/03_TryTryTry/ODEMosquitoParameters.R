########################################################################
#=======================================================================
# ODEMosquitoParameters.R
# Contains the accessors and setters for mosquitos and control measures
# 
#=======================================================================
########################################################################

######################################################################################
#------------------------------------------------------------------------
############ THETA FACADE #############################################
#------------------------------------------------------------------------
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
    bV = additionalTransmissionParameters[["bV"]]
  )
}
######################################################################################
#------------------------------------------------------------------------
############ SPECIES SPECIFIC PARAMETERS ################################
#------------------------------------------------------------------------
#* muV:     Adult mosquito daily mortality
#* Q0:      Human blood index
#* phiB:    Proportion of bites on a person while they are in bed
#* phiI:    Proportion of bites on a person while they are indoors
#* rOVI:    
#* sOVI:
#* rFOG:
#* sFOG:
#* rLAR:
#* sLAR:
#* rBIO:
#* sBIO:
#* rSRE:
#* sSRE:
#* rIRS:    Probability of mosquito repeating a feeding attempt due to IRS
#* sIRS:    Probability of mosquito feeding and surviving in presence of IRS
#* rITN:    Probability of mosquito repeating a feeding attempt due to IRS
#* sITN:    Probability of mosquito feeding and surviving in presence of ITN
#* rIVM:
#* sIVM:
#* rHOU:
#* sHOU:
#* rODO:
#* sODO:
#* rSPA:
#* sSPA:
#------------------------------------------------------------------------
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
######################################################################################
#------------------------------------------------------------------------
############ INTERVENTIONS PARAMETERS ###################################
#------------------------------------------------------------------------
#* OVIcov:        Ovitraps coverage
#* time_OVI_on:   When Ovitraps are applied (days)
#* FOGcov:        Fogging coverage
#* time_FOG_on:   When Fogging is applied (days)
#* LARcov:        Larvacide coverage
#* time_LAR_on:   When Larvacide is applied (days)
#* BIOcov:        Biological Control coverage
#* time_BIO_on:   When Biological Control is applied (days)
#* SREcov:        Source Reduction coverage
#* time_SRE_on:   When Source Reduction is applied (days)
#* IRScov:        IRS coverage
#* time_IRS_on:   When IRSs are applied (days)
#* ITNcov:        ITN coverage
#* time_ITN_on:   When ITNs are applied (days)
#* IVMcov:        Ivermectin coverage
#* time_IVM_on:   When Ivermectin is applied (days)
#* HOUcov:        House Modification coverage
#* time_HOU_on:   When House Modifications are applied (days)
#* ODOcov:        Odor Traps coverage
#* time_ODO_on:   When Odor Traps are applied (days)
#* SPAcov:        Spatial Repellents coverage
#* time_SPA_on:   When Space Repellents are applied (days)
#------------------------------------------------------------------------
getInterventionsParameters = function(){
  #. getInterventionsParameters: Returns the list of intervention parameters
  c(
    OVIcov = 0.0, time_OVI_on = 00,
    FOGcov = 0.0, time_FOG_on = 00,
    LARcov = 0.0, time_LAR_on = 00,
    BIOcov = 0.0, time_BIO_on = 00,
    SREcov = 0.0, time_SRE_on = 00,
    IRScov = 0.8, time_IRS_on = 50,
    ITNcov = 0.8, time_ITN_on = 50,
    IVMcov = 0.0, time_IVM_on = 00,
    HOUcov = 0.0, time_HOU_on = 00,
    ODOcov = 0.0, time_ODO_on = 00,
    SPAcov = 0.0, time_SPA_on = 00
  )
}
######################################################################################
#------------------------------------------------------------------------
############ MOSQUITO LIFE CYCLE PARAMETERS #############################
#------------------------------------------------------------------------
#* beta:    Number of eggs laid per day per female mosquito
#* muEL:    Early larval instar daily mortality
#* muLL:    Late larvarl instar daily mortality
#* muPL:    Pupal daily mortality
#* durEL:   Duration of early instar stage
#* durLL:   Duration of late instar stage
#* durPL:   Pupal daily mortality
#* durPL:   Duration of pupal stage
#* durEV:   Duration of latent period in mosquito (days)
#* gamma:   Effect of density-dependence on late instars relative to early instars
#* tau1:    Time spent foraginf for a blood meal at 0% ITN coverage
#* tau2:    Time spent resting and ovipositing by a mosquito
#------------------------------------------------------------------------
getMosquitoLifeCycleParameters = function(){
  #. getMosquitoLifeCycleParameters: Returns the list of mosquito life cycle parameters
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
#* f0:        Daily biting rate by mosquitoes on animals and humans
#* epsilon0:  Daily entomological inolculation rate
#* iH_eq:     Equilibrium malaria prevalence in humans
#* NH_eq:     Equilibrium human population size
#* bV:        Probability of transmission from human to vector per infectious bite
#------------------------------------------------------------------------
getAdditionalTransmissionParameters = function(){
  #. getAdditionalTransmissionParameters: Returns the transmission parameters
  c(
    f0 = 1/3, epsilon0 = 10/365, 
    iH_eq = 0.35, NH_eq = 200, 
    bV = 0.05
  )
}
######################################################################################