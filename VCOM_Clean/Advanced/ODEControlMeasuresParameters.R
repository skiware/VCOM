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
#* ECScov:        endocticide coverage (systemic applied)
#* time_ECS_on:   When endocticide is (systemic) applied (days)
#* ECTcov:        endocticide coverage (topical applied)
#* time_ECT_on:   When endocticide is (topical) applied (days)
#* HOUcov:        House Modification coverage
#* time_HOU_on:   When House Modifications are applied (days)
#* OBTcov:        Odor Traps coverage (ratio of traps to human)
#* time_OBT_on:   When Odor Traps are applied (days)
#* SPRcov:        Spatial Repellents coverage
#* time_SPR_on:   When Spatial Repellents are applied (days)
#* time_ATSB_on:  When ATSB are applied (days)
#* ATSBcov:       ATSB coverage
#* time_SSP_on:   When Space spraying are applied (days)
#* SSPcov:        Space spraying coverage
#* aOBT:          availability of one odor baited trap in relation to one human
#* SPRcov:        Spatial Repellents coverage
#* time_SPR_on:   When Space Repellents are applied (days)
#* PPMcov:        Personal Protection Measures coverage
#* time_PPM_on:   When the personal protection measures are used by people

#------------------------------------------------------------------------
getInterventionsParameters = function(
  OVIcov=0,   time_OVI_on=0,
  FOGcov=0,   time_FOG_on=0,
  LARcov=0,   time_LAR_on=0,
  BIOcov=0,   time_BIO_on=0,
  SREcov=0,   time_SRE_on=0,
  IRScov=0,   time_IRS_on=0,
  ITNcov=0,   time_ITN_on=0,
  ECScov=0,   time_ECS_on=0,
  ECTcov=0,   time_ECT_on=0,
  HOUcov=0,   time_HOU_on=0,
  OBTcov=0,   time_OBT_on=0,
  SPRcov=0,   time_SPR_on=0,
  PPMcov=0,   time_PPM_on=0,
  ATSBcov=0,  time_ATSB_on=0,
  SSPcov=0,   time_SSP_on=0

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
    ECScov=ECScov,time_ECS_on=time_ECS_on,
    ECTcov=ECTcov,time_ECT_on=time_ECT_on,
    HOUcov=HOUcov,time_HOU_on=time_HOU_on,
    OBTcov=OBTcov,time_OBT_on=time_OBT_on,
    SPRcov=SPRcov,time_SPR_on=time_SPR_on,
    PPMcov=PPMcov,time_PPM_on=time_PPM_on,
    ATSBcov=ATSBcov,time_ATSB_on=time_ATSB_on,
    SSPcov=SSPcov,time_SSP_on=time_SSP_on
  )
}
######################################################################################
