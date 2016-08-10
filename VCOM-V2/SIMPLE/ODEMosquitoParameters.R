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
    iH_eq       = additionalTransmissionParameters[["iH_eq"]],
    NH_eq       = additionalTransmissionParameters[["NH_eq"]],
    bV          = additionalTransmissionParameters[["bV"]],
    bh          = additionalTransmissionParameters[["bh"]]
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
#* rSPR:    Probability of mosquito repeating a feeding attempt due to spatial repelent
#* sSPR:    Probability of mosquito feeding succesfully in presence to spatial repelent
#* rPPM:    Probability of mosquito repeating a feeding attempt due to personal protection measures
#* sPPM:    Probability of mosquito feeding succesfully in presence to personal protection measures
#* rIRS:    Probability of mosquito repeating a feeding attempt due to IRS
#* sIRS:    Probability of mosquito feeding and surviving in presence of IRS
#* rITN:    Probability of mosquito repeating a feeding attempt due to IRS
#* sITN:    Probability of mosquito feeding and surviving in presence of ITN
#* rECT:    Probability of mosquito repeating a feeding attempt due to instecide (topical) treated cattle
#* sECT:    Probability of mosquito feeding and surviving in presence of instecide (topical) treated cattle
#* rECS:    Probability of mosquito repeating a feeding attempt due to instecide (systemic) treated cattle
#* sECS:    Probability of mosquito feeding and surviving in presence of instecide (systemic) treated cattle
#* dHOU:    Death due to encountering mosquito proofed housing
#* dIRS:    Death due to encountering IRS treated house
#* aOBT:    availability of one odor baited trap in relation to one human
#* eSRE:    effectiveness of the impact of source reduction in K
#* fATSB    factor allowing for increased mosquito death due to ATBS
#* fLAR    factor allowing for increased mosquito death due to larvaciding
#* fBIO    factor allowing for increased mosquito death due to biological control
#* fSSP     factor allowing for increased mosquito death due to space spraying
#* fOVI     factor allowing for increased mosquito death due to space spraying
#* rHOU:
#* sHOU:


#------------------------------------------------------------------------
getAnGambiaeParameters = function(){
  #. getAnGambiaeParameters: Returns the list of Anopheles Gambiae specific parameters
  c(
    muV = 1/7.6, Q0 = 0.92,   phiB = 0.89, phiI = 0.97,
    rOVI = 0.00, sOVI = 0.00, rFOG = 0.00, sFOG = 0.00,
    rLAR = 0.00, sLAR = 0.00, rBIO = 0.00, sBIO = 0.00,
    rSRE = 0.00, sSRE = 0.00, rIRS = 0.60, sIRS = 0.00,
    rITN = 0.56, sITN = 0.03, rECS = 0.00, sECS = 0.40,
    rHOU = 0.00, sHOU = 0.00, rODO = 0.00, sODO = 0.00,
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
    muV = 1/7.6, Q0 = 0.71, phiB = 0.50, phiI = 0.50,
    rOVI = 0.00, sOVI = 0.00, rFOG = 0.00, sFOG = 0.00,
    rLAR = 0.00, sLAR = 0.00, rBIO = 0.00, sBIO = 0.00,
    rSRE = 0.00, sSRE = 0.00, rIRS = 0.60, sIRS = 0.00,
    rITN = 0.56, sITN = 0.03, rECS = 0.50, sECS = 0.50,
    rHOU = 0.00, sHOU = 0.00, rODO = 0.00, sODO = 0.00,
    rSPR = 0.00, sSPR = 0.00, rECT = 0.50, sECT = 0.50,
    rPPM = 0.00, sPPM = 0.00, dHOU = 0.10, dIRS = 0.10,
    aOBT = 2.00, eSRE = 0.70, fATSB =1.50, fSSP = 1.50,
    fOVI = 1.50, fLAR = 55.5, fBIO = 35.5
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
######################################################################################