########################################################################
# ODEModelOutput.R
# Contains the functions that produce model outputs
# Need to test them - then delete this function
#=======================================================================
########################################################################

######################################################################################
produceHumanBitingRate = function(V,H){
  #. produceHumanBitingRate: Main function that return human biting rate per mosquito in presence of intervetion
  Mdensity <- V/H
  return(Mdensity)
}
######################################################################################
produceMosqDensity = function(V,H){
  #. produceMosqDensity: Main function that return mosquito density in presence of intervetion
  Mdensity <- V/H
  return(Mdensity)
}
######################################################################################
produceHBI = function(wCom_human,wCom){
  #. produceHBI: Main function that return human blood index
  #HBI_com <- (Pfi+pfo)/pf
  HBI_com <- (wCom_human)/wCom
  return(HBI_com)
}

######################################################################################
produceVC = function(a_theta, Mdensity,muVCom,durEV){
  #. produceVC: Main function that return vectorial capacity
  ## n incubation period
  tilda <- durEV
  VC <- (Mdensity * a_theta^2 *exp(-muVCom*tilda))/muVCom
  return(VC)
}
######################################################################################
produceEIR = function(a_theta, IV, H){
  #. produceEIR: Main function that return entomological innoculation rate
  ## X_theta = iH_eq, bh - add to transmission parameter
  EIR <- (IV*a_theta)/H
  return(EIR)
}

######################################################################################
produceEIR = function(a_theta,muVCom, V,bv,bh,H){
  #. produceR_0: Main function that return entomological innoculation rate
  R_o <- (V/H)*a_theta^2*bv*(bh/(muVCom*V))
  return(R_o)
}
