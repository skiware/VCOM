########################################################################
# ODEModelOutput.R
# Contains the functions that produce model outputs
# Need to test them - then delete this function
#=======================================================================
########################################################################

######################################################################################
computeLengthGonotrophicycle = function(deltaCom){
  #. computeLengthGonotrophicycle: Main function that return the length of gonotrophic cycle in presence of interventions
  #Length of gonotrophic cycle
  
  f_theta <- 1/deltaCom
  return(f_theta)
}

computeHumanBitingRate = function(f_theta,HBI_com){
  #. computeHumanBitingRate: Main function that return human biting rate per mosquito in presence of intervetion
    
   a_theta <- HBI_com*f_theta
  return(a_theta)
}
######################################################################################
computeMosqDensity = function(NV,NH){
  #. computeMosqDensity: Main function that return mosquito density in presence of intervetion
  Mdensity <- NV/NH
  return(Mdensity)
}
######################################################################################
computeHBI = function(wCom_human,wCom){
  #. computeceHBI: Main function that return human blood index
  #HBI_com <- (Pfi+pfo)/pf
  HBI_com <- (wCom_human)/wCom
  return(HBI_com)
}

######################################################################################
computeVC = function(a_theta, NV,NH,muVCom,theta){
  #. computeVC: Main function that return vectorial capacity
  ## n incubation period
  tilda <- theta["durEV"]
  VC <- ((NV/NH) * a_theta^2 *exp(-muVCom*tilda))/muVCom
  return(VC)
}
######################################################################################
computeEIR = function(a_theta, IV, NH){
  #. computeEIR: Main function that return entomological innoculation rate
  ## X_theta = iH_eq, bh - add to transmission parameter
  EIR <- 365*(IV*a_theta)/NH    #365 per year
  return(EIR)
}

######################################################################################
computeRO = function(a_theta,muVCom, NV,bv,bh,NH,theta){
  #. computeRO : Main function that return basic reproduction rate
  ## n incubation period
  tilda <- theta[["durEV"]]
  R_o <- (NV/NH)*a_theta^2*bv*(bh*exp(-muVCom*tilda)/(muVCom))
  return(R_o)
}
