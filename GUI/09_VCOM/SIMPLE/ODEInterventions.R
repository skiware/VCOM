
#Include global parameters here Q0, phiB, phiI

#REQUIRED_PARAMETERS_LIST_GLOBAL = c("Q0")


impactIndoorInterventions = function(time,time_ITN_on,ITNcov,time_IRS_on,IRScov,rITN,sITN,rIRS,sIRS,Q0, phiB, phiI){
  #. impactIndoorInterventions: compute both zcom and wcom for indoor interventions
  
  ##*************************Human - Indoor protection *********************************##
  if (time > time_ITN_on) { ITNcov_t <- ITNcov } else { ITNcov_t <- 0 }
  if (time > time_IRS_on) { IRScov_t <- IRScov } else { IRScov_t <- 0 }
  
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
  
  #Human - new search probability after a mosq is repelled (SK) by indoor interventions
  zCom_Human <- Q0*cITN*phiB*rITN + Q0*cIRS*phiI*rIRS + Q0*cCom*(phiI-phiB)*rIRS + Q0*cCom*phiB*rCom
  
  # Human - Probability that a surviving mosquito succeeds in feeding during a single attempt:##
  wCom_Human <- 1 - Q0 + Q0*c0 + Q0*cITN*(1-phiB+phiB*sITN) + Q0*cIRS*(1-phiI+phiI*sIRS) + Q0*cCom*((phiI-phiB)*sIRS + 1-phiI + phiB*sCom)
  
  impactIndoor <- c(zCom_Human,wCom_Human)
  
  return(impactIndoor)
  
}

impactInsecticideTreatedCattle = function(time,time_ECS_on,ECScov,time_ECT_on,ECTcov,rECT,sECS,sECT,Q0){
  #. impactInsecticideTreatedCattle: compute both zcom and wcom for insecticide treated cattle (systemic and topical)
  
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
  wCom_Cattle <- (1 - Q0)*(c0_Cattle+cECS*sECS+cECT*sECT+cCom_Cattle*sECS*sECT)
  
  impactCattle <- c(zCom_Cattle,wCom_Cattle)
  
  return(impactCattle)
  
}