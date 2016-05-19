########################################################################
#=======================================================================
# ODEControlMeasuresParameters.R
# Contains the functions that handle the population's interventions
#
#=======================================================================
########################################################################

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
  # (SK) and the time that a particular intervention is on
  c(
    OVIcov = 0.0, time_OVI_on = 00,
    FOGcov = 0.0, time_FOG_on = 00,
    LARcov = 0.0, time_LAR_on = 00,
    BIOcov = 0.0, time_BIO_on = 00,
    SREcov = 0.0, time_SRE_on = 00,
    IRScov = 0.0, time_IRS_on = 10,
    ITNcov = 0.0, time_ITN_on = 00,
    IVMcov = 0.0, time_IVM_on = 00,
    HOUcov = 0.0, time_HOU_on = 00,
    ODOcov = 0.0, time_ODO_on = 00,
    SPAcov = 0.0, time_SPA_on = 00
  )
}
######################################################################################