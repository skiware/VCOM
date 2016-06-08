######################################################################
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
getInterventionsParameters = function(
  OVIcov=0,   time_OVI_on=0,
  FOGcov=0,   time_FOG_on=0,
  LARcov=0,   time_LAR_on=0,
  BIOcov=0,   time_BIO_on=0,
  SREcov=0,   time_SRE_on=0,
  IRScov=0,   time_IRS_on=0,
  ITNcov=.5,  time_ITN_on=20,
  IVMcov=0,   time_IVM_on=0,
  HOUcov=0,   time_HOU_on=0,
  ODOcov=0,   time_ODO_on=0,
  SPAcov=0,   time_SPA_on=0
){
  #. getInterventionsParameters: Returns the list of intervention parameters
  # (SK) and the time that a particular intervention is on
  c(
    OVIcov=OVIcov,time_OVI_on=time_OVI_on,
    FOGcov=FOGcov,time_FOG_on=time_FOG_on,
    LARcov=LARcov,time_LAR_on=time_LAR_on,
    BIOcov=BIOcov,time_BIO_on=time_BIO_on,
    SREcov=SREcov,time_SRE_on=time_SRE_on,
    IRScov=IRScov,time_IRS_on=time_IRS_on,
    ITNcov=ITNcov,time_ITN_on=time_ITN_on,
    IVMcov=IVMcov,time_IVM_on=time_IVM_on,
    HOUcov=HOUcov,time_HOU_on=time_HOU_on,
    ODOcov=ODOcov,time_ODO_on=time_ODO_on,
    SPAcov=SPAcov,time_SPA_on=time_SPA_on
  )
}
######################################################################################