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
theta = getTheta()
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
