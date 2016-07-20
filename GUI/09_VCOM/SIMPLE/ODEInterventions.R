
########################################################################
#=======================================================================
# ODEInterventions.R

# Contains function for all interventions - Targeting mosquito while 
#           Feeding upon Human Indoor: LLINs, IRS, and mosquito proofed housing (HOU)
#           Feeding Upon Human Outdoor: 
#           Feeding upon Cattle: 
# 
#=======================================================================


#Include global parameters here Q0, phiB, phiI

#REQUIRED_PARAMETERS_LIST_GLOBAL = c("Q0")

##******************************************Interventions*****************************##


##**********Aquatic habitats - impact of source management ****##############################
impactSourceReduction = function(time,eSRE,SREcov,time_SRE_on,K){
  #. impactSourceReduction: compute updated K due to source management
  
  if (time > time_SRE_on) { SREcov_t <- SREcov } else { SREcov_t <- 0 }
  
  K_sr = (1-eSRE*SREcov)*K
  return(K_sr)
  
  
}

##**********Aquatic habitats - impact of larvaciding and biological control ****##############################
impactLarvacidingBiologicalControl= function(time,time_LAR_on, LARcov,time_BIO_on,BIOcov,fLAR,fBIO){
  #. impactLarvacidingBiologicalControl: compute the impact of larvaciding and biological control
  
  if (time > time_LAR_on) { LARcov_t <- LARcov } else { LARcov_t <- 0 }
  if (time > time_BIO_on) { BIOcov_t <- BIOcov } else { BIOcov_t <- 0 }
  
  #Coverage larvaciding only
  cLAR <- LARcov_t - LARcov_t*BIOcov_t
  # Coverage with biological control  only
  cBIO <- BIOcov_t -  LARcov_t*BIOcov_t
  #both applied
  cCom_Larval <-  LARcov_t*BIOcov_t
  # neither applied
  c0_Larval <- 1 - LARcov_t - BIOcov_t +  LARcov_t*BIOcov_t
  
  f_LAR_BIO =fLAR*fBIO
  
  
}


##**********Host Seeking - ATSB and Space Spraying ****##############################
impactATSBSpaceSpraying = function(time,time_ATSB_on,ATSBcov,time_SSP_on,SSPcov,fSSP,fATSB,muV){
  #. impactATSBSpaceSpraying: compute the impact of ATSB and SS on P1
  
  
  
  if (time > time_ATSB_on) { ATSBcov_t <- ATSBcov } else { ATSBcov_t <- 0 }
  if (time > time_SSP_on) { SSPcov_t <- SSPcov } else { SSPcov_t <- 0 }
  
  #Coverage ATSB only
  cATSB <- ATSBcov_t - ATSBcov_t*SSPcov_t
  # Coverage space spraying only
  cSSP <- SSPcov_t -  SSPcov_t*ATSBcov_t
  #both applied
  cCom_Searching <-  ATSBcov_t*SSPcov_t
  # neither applied
  c0_Searching <- 1 - ATSBcov_t - SSPcov_t +  ATSBcov_t*SSPcov_t
  
  # Impact on mosquito mortality
  #Initially on the MS
  muV_1 = muV*(cATSB*fATSB+cSSP*fSSP+cCom_Searching*fATSB*fSSP+c0_Searching)
  
  return(muV_1)
  
}


##**********The impact of Odor baited traps ****##############################
impactOdorBaitedTraps = function(time,Q0,aOBT,OBTcov,time_OBT_on){
  #. impactOdorBaitedTraps: compute the impact of odor baited traps in reducing Q0
  #aOBT = availability
  #OBTcov = coverage same as ratio trap to human
  if (time > time_OBT_on) { OBTcov_t <- OBTcov } else {OBTcov_t <- 0 }
  
  Q0_t_h = Q0*(1/(1+aOBT*OBTcov))  #Impact on Q0 for humans
  Q0_t_c = (1-Q0)*(1/(1+aOBT*OBTcov)) #Impact on 1-Q0 prop going to Cattle
  impactOdor <- c(Q0_t_h,Q0_t_c)
  
  return(impactOdor)
  
}

##***********************************Protecting Humans Indoor**********************##
impactIndoorProtection = function(time,time_ITN_on,ITNcov,time_IRS_on,IRScov,HOUcov,time_HOU_on,
                                     rITN,sITN,rIRS,rHOU,sIRS,sHOU, Q0_t_h,Q0, phiB, phiI,dHOU,dIRS,time_OBT_on){
  #. impactIndoorProtection: compute both zcom and wcom for indoor interventions
  # Incorporate odor baited traps
  if (time > time_OBT_on) { Q0 <- Q0_t_h} else {Q0 <- Q0 }
  
   
  ##*************************Human - Indoor protection *********************************##
  if (time > time_ITN_on) { ITNcov_t <- ITNcov } else { ITNcov_t <- 0 }
  if (time > time_IRS_on) { IRScov_t <- IRScov } else { IRScov_t <- 0 }
  if (time > time_HOU_on) { HOUcov_t <- HOUcov } else { HOUcov_t <- 0 }
  
  #Coverage with LLINs only
  cITN <- ITNcov_t - ITNcov_t*IRScov_t - ITNcov_t*HOUcov_t + ITNcov_t*IRScov_t*HOUcov_t 
  # Coverage with IRS only
  cIRS <- IRScov_t - ITNcov_t*IRScov_t - IRScov_t*HOUcov_t + ITNcov_t*IRScov_t*HOUcov_t
  # Coverage with house modification only
  cHOU <- HOUcov_t - ITNcov_t*HOUcov_t - IRScov_t*HOUcov_t + ITNcov_t*IRScov_t*HOUcov_t
  #LLINs and IRS
  cITN_IRS <- ITNcov_t*IRScov_t - ITNcov_t*IRScov_t*HOUcov_t
  #LLINs and HOU
  cITN_HOU <- ITNcov_t*HOUcov_t - ITNcov_t*IRScov_t*HOUcov_t
  #IRS and HOU
  cIRS_HOU <- IRScov_t*HOUcov_t - ITNcov_t*IRScov_t*HOUcov_t
  #LLIN and IRS and HOU
  cCom <- ITNcov_t*IRScov_t*HOUcov_t
  #Neither of them
  c0 <- 1 - ITNcov_t - IRScov_t - HOUcov_t + ITNcov_t*IRScov_t + ITNcov_t*HOUcov_t + IRScov_t*HOUcov_t- 2* ITNcov_t*IRScov_t*HOUcov_t 
  
  # repeating due to LLINs and IRS
  r_LLIN_IRS <- rIRS + (1-rIRS)*rITN
  
  # repeating due to LLINs and HOU
  r_LLIN_HOU <- rHOU + (1-rHOU)*rITN
  
  # repeating due to IRS and HOU
  r_IRS_HOU <- rHOU + (1-rHOU)*rIRS
  
  # repeating due to LLINs, IRS and HOU
  rCom <- rHOU + (1-rHOU)*rIRS + (1-rHOU)*(1-rIRS)*rITN
  
  #Human - new search probability after a mosq is repelled (SK) by indoor interventions
  zCom_Human <- Q0*cITN*phiB*rITN + Q0*cIRS*phiI*rIRS + Q0*cCom*(phiI-phiB)*rIRS + Q0*cCom*phiB*rCom
  # zCom_Human <- Q0*(cHOU*phiI*rHOU + cITN*phiB*rITN + cIRS*phiI*rIRS + cITN_HOU*((phiI-phiB)*rHOU+phiB*r_LLIN_HOU) 
  #                   + cIRS_HOU*phiI*r_IRS_HOU + cITN_IRS*((phiI-phiB)*rIRS+phiB*r_LLIN_IRS) 
  #                   + cCom*((phiI-phiB)*rIRS + phiB*rCom))
  # 
  #ENDELEA HAPA
  # Human - Probability that a surviving mosquito succeeds in feeding during a single attempt:##
  
  #Succesfully Feeding in presence of LLINs and HM
  s_ITN_HOU <- (1-rHOU)*sITN*sHOU
  
  #Succesfully Feeding in presence of LLINs and IRS
  s_ITN_IRS <- (1-rIRS)*sITN*sIRS
  
  #Succesfully Feeding in presence of IRS and HM
  s_IRS_HOU <- (1-rHOU)*(1-rIRS)*sIRS*sHOU
  #dIRS - death due to IRS, and dHOU death due to HM - required to normalize SIR and SHM since the killing effect occurs later
  #Feeding in presence of LLINs and IRS and HM
  sCom  <- (1-rHOU)*(1-rIRS)*sITN*(sIRS/(sIRS+dIRS))*(sHOU/(sHOU+dHOU))
  
  #Prob succesfully feeding in presence of one or all of these interventions
  # wCom_Human <- Q0*(cHOU*((1-phiI)+phiI*sHOU)+cITN*((1-phiB)+phiB*sITN)+cIRS*((1-phiI)+phiI*sIRS) 
  #                   + cITN_HOU*((1-phiI)+(phiI-phiB)*sHOU+phiB*s_ITN_HOU)
  #                   + cITN_IRS*((1-phiI)+(phiI-phiB)*sIRS+phiB*s_ITN_IRS)   
  #                   + cIRS_HOU*((1-phiI)+(phiI )*s_IRS_HOU               ) 
  #                   + cCom*((1-phiI)+(phiI-phiB)*s_IRS_HOU+sCom)         )
  # 
  wCom_Human <- Q0*cITN*(1-phiB+phiB*sITN) + Q0*cIRS*(1-phiI+phiI*sIRS) + Q0*cCom*((phiI-phiB)*sIRS + 1-phiI + phiB*sCom)
  
  #browser()
  #Extracting zcom and wcom
  impactIndoor <- c(zCom_Human,wCom_Human,c0)
  
  #print(c(cITN, c0, cCom, sCom,rCom,sITN,zCom_Human,wCom_Human))
  
  return(impactIndoor)
  
}

##*********Protecting humans outdoor **********************************************##

impactOutdoorProtection = function(time,time_SPR_on,SPRcov,time_PPM_on,PPMcov,rSPR,rPPM,sSPR,sPPM,Q0_t_h,Q0,phiI,c0,time_OBT_on){
  #. impactInsecticideTreatedCattle: compute both zcom and wcom for insecticide treated cattle (systemic and topical)
  
  # Incorporate odor baited traps
  if (time > time_OBT_on) { Q0 <- Q0_t_h} else {Q0 <- Q0 }
  
  if (time > time_SPR_on) { SPRcov_t <- SPRcov } else { SPRcov_t <- 0 }
  if (time > time_PPM_on) { PPMcov_t <- PPMcov } else { PPMcov_t <- 0 }
  
  #Coverage spatial repelent only
  cSPR <- SPRcov_t - SPRcov_t*PPMcov_t
  # Coverage with personal protection measure only
  cPPM <- PPMcov_t -  SPRcov_t*PPMcov_t
  #both applied
  cCom_Outdoor <-  SPRcov_t*PPMcov_t
  # neither applied
  c0_Outdoor <- 1 - SPRcov_t - PPMcov_t +  SPRcov_t*PPMcov_t
  
  # repeating due to encountering any
  r_SPR_PPM <- rSPR + (1-rSPR)*rPPM
  
  #browser()
  
  #Human - new search probability after a mosq is repelled by SRP and/or PPM
  zCom_Outdoor <- Q0*c0*(cSPR*rSPR+cPPM*rPPM+cCom_Outdoor*r_SPR_PPM)
  
  # succesfully feeding in presence of both interventions
  s_SPR_PPM <- (1-rSPR)*sSPR*sPPM
  
  #Add cattle treated with endocticide (systemic and/or topical applied)
  #wCom_Outdoor <- Q0*(1-phiI)*(c0_Outdoor+cSPR*sSPR+cPPM*sPPM+cCom_Outdoor*s_SPR_PPM)
  wCom_Outdoor <- Q0*c0*(c0_Outdoor+cSPR*sSPR+cPPM*sPPM+cCom_Outdoor*s_SPR_PPM)
  
  impactOutdoor <- c(zCom_Outdoor,wCom_Outdoor)
  
  #browser()
 
  return(impactOutdoor)
  
}


##**********************Protection by insecticide treated cattle **********************##

impactInsecticideTreatedCattle = function(time,time_ECS_on,ECScov,time_ECT_on,ECTcov,rECT,sECS,sECT,Q0_t_c,Q0,time_OBT_on){
  #. impactInsecticideTreatedCattle: compute both zcom and wcom for insecticide treated cattle (systemic and topical)
 
   # Incorporate odor baited traps by changing prop of bites to cattle when OBT is on
  if (time > time_OBT_on) { Q0_t_C <- Q0_t_c} else {Q0_t_C <-1- Q0 }
  
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
  #zCom_Cattle <- (1 - Q0)*(cECT*rECT+cCom_Cattle*rECT)
  zCom_Cattle <- Q0_t_C*(cECT*rECT+cCom_Cattle*rECT)
 
   #browser()
  #Add cattle treated with endocticide (systemic and/or topical applied)
  wCom_Cattle <- Q0_t_C*(c0_Cattle+cECS*sECS+cECT*sECT+cCom_Cattle*sECS*sECT)
  
  impactCattle <- c(zCom_Cattle,wCom_Cattle)
  
  return(impactCattle)
  
}


##**************************Targeting Resting, ovipositing mosquitoes**********************##
impactRestingOvipositing = function(time,time_OVI_on,OVIcov,time_ATSB_on,ATSBcov,SSPcov,time_SSP_on,
                                  fOVI,fATSB,fSSP,muV){
  #. impactRestingOvipositing: compute the impact of ATSB, Space Spraying, and ovitraps on muV_2
  
  
  ##*************************Human - Indoor protection *********************************##
  if (time > time_OVI_on) { OVIcov_t <- OVIcov } else { OVIcov_t <- 0 }
  if (time > time_ATSB_on) { ATSBcov_t <- ATSBcov } else { ATSBcov_t <- 0 }
  if (time > time_SSP_on) { SSPcov_t <- SSPcov } else { SSPcov_t <- 0 }
  
  #Coverage with ovitraps only
  cOVI <- OVIcov_t - OVIcov_t*ATSBcov_t - OVIcov_t*SSPcov_t + OVIcov_t*ATSBcov_t*SSPcov_t 
  # Coverage with ATSB only
  cATSB <- ATSBcov_t - ATSBcov_t*OVIcov_t - ATSBcov_t*SSPcov_t + OVIcov_t*ATSBcov_t*SSPcov_t
  # Coverage with space Spraying only
  cSSP <- SSPcov_t - OVIcov_t*SSPcov_t - ATSBcov_t*SSPcov_t + OVIcov_t*ATSBcov_t*SSPcov_t
  #OVI and ATSB
  cOVI_ATSB <- OVIcov_t*ATSBcov_t - OVIcov_t*ATSBcov_t*SSPcov_t
  #OVI and SSP
  cOVI_SSP <- OVIcov_t*SSPcov_t - OVIcov_t*ATSBcov_t*SSPcov_t
  #ATSB and SSP
  cATSB_SSP <- ATSBcov_t*SSPcov_t - OVIcov_t*ATSBcov_t*SSPcov_t
  #OVI and ATSB and OVI
  cCom_RestingOvipositing <- OVIcov_t*ATSBcov_t*SSPcov_t
  #Neither of them
  c0_RestingOvipositing <- 1 - OVIcov_t - ATSBcov_t - SSPcov_t + OVIcov_t*ATSBcov_t + OVIcov_t*SSPcov_t + ATSBcov_t*OVIcov_t- 2* OVIcov_t*ATSBcov_t*SSPcov_t 
  
  muV_2_Com = muV*(cOVI*fOVI + cATSB*fATSB + cSSP*fSSP + cOVI_ATSB *fOVI*fATSB + cOVI_SSP *fOVI*fSSP + cATSB_SSP*fATSB*fSSP
  + cCom_RestingOvipositing *fOVI*fATSB*fSSP + c0_RestingOvipositing)
  
  return(muV_2_Com)
  
}






###Notes for Sam####
# 1. Check intersection of three intervention - ITNcov_t*IRScov_t*HOUcov_t


