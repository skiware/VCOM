### This is the driver ###

# Source all the required files
# Run the model and generate outputs

## ##########################

# Clear all stored parameters:
rm(list=ls())

library(shiny)
library(deSolve)
library(ggplot2)
#library(shinyjs)
#library(shinythemes)
source("ODEModel.R")
source("ODEMosquitoParameters.R")
source("ODEAuxiliaryFunctions.R")
source("ODEControlMeasuresParameters.R")
source("ODETransmissionParameters.R")
source("ODEInterventions.R")
source("ODEModelOutput.R")

## Get parameter for a specific specie ######

# Run first for An. gambiae
MOSQUITO_PARAMETERS = getAnGambiaeParameters()

# Set values for control measures??
INITIAL_TIME_VALUE   = 365
#LLINs, coverage value, time it is on
INITIAL_ITN_COVERAGE = 0.50
INITIAL_ITN_TIME     = 100
#IRS
INITIAL_IRS_COVERAGE = 0.00
INITIAL_IRS_TIME     = 200
#House modification
INITIAL_HOU_COVERAGE = 0.0
INITIAL_HOU_TIME     = 200
#Cattle - Systemic
INITIAL_ECS_COVERAGE = 0.0
INITIAL_ECS_TIME     = 200
#Cattle - Topical
INITIAL_ECT_COVERAGE = 0.00
INITIAL_ECT_TIME     = 100


## Get intervetions parameters - LLINs for testing
#INTERVENTION_PARAMETERS = getInterventionsParameters(ITNcov=INITIAL_ITN_COVERAGE,time_ITN_on=INITIAL_ITN_TIME)

## Get intervetions parameters
INTERVENTION_PARAMETERS = getInterventionsParameters(
                                 #LLINs
                          ITNcov=INITIAL_ITN_COVERAGE,time_ITN_on=INITIAL_ITN_TIME,
                                  #IRS
                          IRScov=INITIAL_IRS_COVERAGE,time_IRS_on=INITIAL_IRS_TIME,
                                   #House Modification
                          HOUcov=INITIAL_HOU_COVERAGE,time_HOU_on=INITIAL_HOU_TIME,
                                  #Cattle - Systemic
                          ECScov=INITIAL_ECS_COVERAGE,time_ECS_on=INITIAL_ECS_TIME,
                                 #Cattle - topical
                           ECTcov=INITIAL_ECT_COVERAGE,time_ECT_on=INITIAL_ECT_TIME)


theta <<- getTheta(interventionParameters=INTERVENTION_PARAMETERS)

## Initialize the model
initState <<- calculateInitialState(theta)



## Run the model
IVM_traj <<- runODE(INITIAL_TIME_VALUE,1,initState,theta,"lsoda")

## Generate outputs - using Histograms
barChartMosquitoDemographics(IVM_traj)

## Generate output - using gg plots
plotTrajectory(IVM_traj)



##Notes for Sam #######
#1. Adding rECT (repelency for topical) - number of Suc Mosq increases instead of descreading - Check behaviour
