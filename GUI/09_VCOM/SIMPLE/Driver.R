### This is the driver ###

# Source all the required files
# Run the model and generate outputs

## ##########################

library(shiny)
library(deSolve)
library(ggplot2)
library(shinyjs)
library(shinythemes)
source("ODEModel.R")
source("ODEMosquitoParameters.R")
source("ODEAuxiliaryFunctions.R")
source("ODEControlMeasuresParameters.R")
source("ODETransmissionParameters.R")

## Get parameter for a specific specie ######

# Run first for An. gambiae
MOSQUITO_PARAMETERS = getAnGambiaeParameters()

## Get intervetions parameters
INTERVENTION_PARAMETERS = getInterventionsParameters(ITNcov=INITIAL_ITN_COVERAGE,time_ITN_on=INITIAL_ITN_TIME)
theta <<- getTheta(interventionParameters=INTERVENTION_PARAMETERS)

## Initialize the model
initState <<- calculateInitialState(theta)

## Run the model
IVM_traj <<- runODE(INITIAL_TIME_VALUE,1,initState,theta,"lsoda")

## Generate output - using gg plots
plotTrajectory(IVM_traj)

## Generate outputs - using Histograms
barChartMosquitoDemographics(IVM_traj)
