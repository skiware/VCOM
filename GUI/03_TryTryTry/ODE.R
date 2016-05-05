#########################################################################
## Malaria vector ODE model                                            ##
## John Marshall (john.marshall@berkeley.edu)                          ##
## 30/May/2015                                                         ##
## Adapted and extended by SK starting Feb 2016                        ##
#########################################################################
#########################################################################
## Coding vector model from White et al. (2011) as a system of ODEs:   ##
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
########### INIT MODEL ##################################################
theta = getTheta()
initState = calculateInitialState(theta) 
########### RUN THE IVM ODEs ############################################
IVM_traj = runODE(365,1,initState,theta,"lsoda")
########### PLOT RESULTS ################################################
par(mfrow=c(2,1))
barChartMosquitoDemographics(IVM_traj)
plotTrajectory(IVM_traj)
#------------------------------------------------------------------------

