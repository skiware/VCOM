#########################################################################
## Malaria vector ODE model                                            ##
## John Marshall (john.marshall@berkeley.edu)                          ##
## 30/May/2015                                                         ##
## Adapted and extended by SK starting Feb 2016                        ##
#########################################################################
#########################################################################
##    ##
#########################################################################
#########################################################################
## Adapted by SK on Feb 08, 2016 for extension:                        ##
#########################################################################

#------------------------------------------------------------------------
########### LOAD LIBRARIES AND SOURCES ##################################
rm(list=ls())
library(deSolve)
library(ggplot2)
source("ODEModel.R")
source("ODEMosquitoParameters.R")
source("ODEAuxiliaryFunctions.R")
source("ODEControlMeasuresParameters.R")
source("ODETransmissionParameters.R")
########### INIT MODEL ##################################################
inter=  c(
  OVIcov = 0.0, time_OVI_on = 00,
  FOGcov = 0.0, time_FOG_on = 00,
  LARcov = 0.0, time_LAR_on = 00,
  BIOcov = 0.0, time_BIO_on = 00,
  SREcov = 0.0, time_SRE_on = 00,
  IRScov = 1, time_IRS_on = 10,
  ITNcov = 1, time_ITN_on = 10,
  IVMcov = 0.0, time_IVM_on = 00,
  HOUcov = 0.0, time_HOU_on = 00,
  ODOcov = 0.0, time_ODO_on = 00,
  SPAcov = 0.0, time_SPA_on = 00
  
  #expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1))
)
theta = getTheta(interventionParameters=inter)
initState = calculateInitialState(theta) 
########### RUN THE IVM ODEs ############################################
tMax <- 360 # Simulation runs up to 365 days
tIncrement <- 1 #time increment
IVM_traj = runODE(tMax,tIncrement,initState,theta,"lsoda")
########### PLOT RESULTS ################################################
#dev.off()  #Clear current figures (SK)
par(mfrow=c(2,1))
barChartMosquitoDemographics(IVM_traj)
plotTrajectory(IVM_traj)
#------------------------------------------------------------------------
