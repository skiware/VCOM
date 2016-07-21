
########################################################################
#=======================================================================
# FeedingCycle.R

# Contains a function to compute the following parameters in presence of interventions
#        1) Prob to survive feeding period   
#        2) Prob to survive, resting and oviposition   
#        3) Prob to survive one day
#        4) Death rate
#        5) Egg laid per day per female mosquito    
# 
#======================================================================


#impactFeedingCycleParameters = function(muV_1_Com,tau1,zCom,wCom,muV_2_Com,tau2,deltaCom,e_ov){
impactFeedingCycleParameters = function(time,tau1,tau2,e_ov,time_ATSB_on,ATSBcov,time_SSP_on,SSPcov,fSSP,fATSB,muV,
                                        Q0,aOBT,OBTcov,time_OBT_on,time_ITN_on,ITNcov,time_IRS_on,IRScov,HOUcov,
                                        time_HOU_on,rITN,sITN,rIRS,rHOU,sIRS,sHOU, phiB, phiI,dHOU,dIRS,
                                        time_SPR_on,SPRcov,time_PPM_on,PPMcov,rSPR,rPPM,sSPR,sPPM,
                                        c0,time_ECS_on,ECScov,time_ECT_on,ECTcov,rECT,sECS,sECT,
                                        time_OVI_on,OVIcov,fOVI){
    #. impactFeedingCycleParameters: compute the impact of interventions on feeding cycle parameters
  ##**********Host seeking - ATSB and space spraying ****##############################
  muV_1_Com <<- impactATSBSpaceSpraying(time,time_ATSB_on,ATSBcov,time_SSP_on,SSPcov,fSSP,fATSB,muV)
  
  #update muV with muv_1
  
  
  
  ##************************Host seeking - Odor baited traps********************
  impactOdorT <<- impactOdorBaitedTraps(time,Q0,aOBT,OBTcov,time_OBT_on)
  
  Q0_t_h = impactOdorT[1]
  Q0_t_c = impactOdorT[2]   # Proportion to cattle affected by traps
  
  ##*************************Human - Indoor protection *********************************##
  impactIndoor <<- impactIndoorProtection(time,time_ITN_on,ITNcov,time_IRS_on,IRScov,HOUcov,time_HOU_on,
                                          
                                          rITN,sITN,rIRS,rHOU,sIRS,sHOU, Q0_t_h,Q0, phiB, phiI,dHOU,dIRS,time_OBT_on)
  
  
  ##*******************For testing only****************#
  #impactIndoor <<- impactIndoorProtection(time,time_ITN_on,ITNcov,time_IRS_on,IRScov,rITN,sITN,rIRS,sIRS,Q0, phiB, phiI)
  
  zCom_Human_Indoor = impactIndoor[1]
  wCom_Human_Indoor = impactIndoor[2]
  c0                = impactIndoor[3]  #Extract cO from indoor see eqn
  
  ##*************************Human - Outdoor protection *********************************##
  
  impactOutdoor <<- impactOutdoorProtection(time,time_SPR_on,SPRcov,time_PPM_on,PPMcov,rSPR,rPPM,sSPR,sPPM,Q0_t_h,Q0,phiI,c0,time_OBT_on)
  # 
  
  
  #impactOutdoor <<- impactOutdoorProtection(time,time_SPR_on,SPRcov,time_PPM_on,PPMcov,rSPR,rPPM,sSPR,sPPM,Q0,phiI,c0)
  #
  
  zCom_Human_Outdoor = impactOutdoor[1]
  wCom_Human_Outdoor = impactOutdoor[2]
  
  ##*************************** Cattle *********************************************##
  impactCattle = impactInsecticideTreatedCattle(time,time_ECS_on,ECScov,time_ECT_on,ECTcov,rECT,sECS,sECT,Q0_t_c,Q0,time_OBT_on)
  zCom_Cattle = impactCattle[1]
  wCom_Cattle = impactCattle[2]
  
  ##*************************** Resting & Ovipositing *********************************************##
  
  muV_2_Com = impactRestingOvipositing(time,time_OVI_on,OVIcov,time_ATSB_on,ATSBcov,SSPcov,time_SSP_on,
                                       fOVI,fATSB,fSSP,muV)
  
  
  ######**************************Computing overall impact***********************************#####
  ## zCom: Probability of a mosquito being repelled : SAM CHECK THIS
  zCom <- zCom_Cattle + zCom_Human_Outdoor + zCom_Human_Indoor
  #zCom <- zCom_Cattle  + zCom_Human_Indoor
  
  ##************************************************
  # deltaCom: Inverse of gonotrophic cycle length with ITNs & IRS:
  deltaCom <- 1/(tau1/(1-zCom) + tau2)
  
  ### wCom: Probability that a surviving mosquito succeeds in feeding during a single attempt:##
  wCom <- wCom_Cattle + wCom_Human_Outdoor + wCom_Human_Indoor
  #wCom <- wCom_Cattle  + wCom_Human_Indoor
  #******************************************************************************************

    # probability of surviving feeding period in the absence of an intervetion
    p10 <- exp(-muV_1_Com*tau1)
    
    # Probability of surviving the first attempt, second, and so on -eqn 3 (SK)
    p1Com <- p10*wCom/(1 - zCom*p10)
    
    #probabbility of resting (SK)
    p2 <- exp(-muV_2_Com*tau2)

    #probability of surviving one day 
    pCom <- (p1Com*p2)^deltaCom 
    # female mosquito mortality rate due to ITN and IRS (SK)
    muVCom <- -log(pCom)
    # betaCom: Eggs laid per day by female mosquitoes in presence of ITNs & IRS:
    betaCom <- e_ov*muVCom/(exp(muVCom/deltaCom) - 1)
    
    impactFeedingCycle <- c(muVCom,betaCom)
    
    return(impactFeedingCycle)

}