########################################################################
#=======================================================================
# ODETransmissionParameters.R
# Contains the functions that deal with the transmission of a disease
#
#=======================================================================
########################################################################

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
    iH_eq = 0.35, NH_eq = 2000, 
    bV = 0.05
  )
}
######################################################################################