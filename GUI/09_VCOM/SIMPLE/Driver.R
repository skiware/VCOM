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
#source("ODEInterventions - FirstPass.R")
source("ODEModelOutput.R")
source("FeedingCycle.R")

## Get parameter for a specific specie ######

# Run first for An. gambiae
MOSQUITO_PARAMETERS = getAnGambiaeParameters()

# Set values for control measures??
INITIAL_TIME_VALUE   = 100

#Source reduction, coverage value, time it is on
INITIAL_SRE_COVERAGE = .00
INITIAL_SRE_TIME     = 20

#LArvaciding, coverage value, time it is on
#INITIAL_LAR_COVERAGE = .00
#INITIAL_LAR_TIME     = 0

#Biological, coverage value, time it is on
#INITIAL_BIO_COVERAGE = .10
#INITIAL_BIO_TIME     = 00


#ATSB, coverage value, time it is on
INITIAL_ATSB_COVERAGE = .00
INITIAL_ATSB_TIME     = 00

#Space Spraying, coverage value, time it is on
INITIAL_SSP_COVERAGE = .00
INITIAL_SSP_TIME     = 00

#Odor Traps, coverage value, time it is on
INITIAL_OBT_COVERAGE = .00
INITIAL_OBT_TIME     = 60
#LLINs, coverage value, time it is on
INITIAL_ITN_COVERAGE = .50
INITIAL_ITN_TIME     = 50
#IRS
INITIAL_IRS_COVERAGE = 0.60
INITIAL_IRS_TIME     = 20
#House modification
INITIAL_HOU_COVERAGE = 0.0
INITIAL_HOU_TIME     = 60
#Cattle - Systemic
INITIAL_ECS_COVERAGE = 0.0
INITIAL_ECS_TIME     = 60
#Cattle - Topical
INITIAL_ECT_COVERAGE = 0.0
INITIAL_ECT_TIME     =20

#Resting and Ovipositing - OviTraps -assuming same coverage for ATSB and SSP
INITIAL_OVI_COVERAGE = 0.00
INITIAL_OVI_TIME     =20



# Sam - check the behaviour of cattle
## Get intervetions parameters - LLINs for testing
#INTERVENTION_PARAMETERS = getInterventionsParameters(ITNcov=INITIAL_ITN_COVERAGE,time_ITN_on=INITIAL_ITN_TIME)

## Get intervetions parameters
INTERVENTION_PARAMETERS = getInterventionsParameters(
                                  #Source Reduction
                          SREcov=INITIAL_SRE_COVERAGE,time_SRE_on=INITIAL_SRE_TIME, 
                                #Larvaciding
                          #LARcov=INITIAL_LAR_COVERAGE,time_LAR_on=INITIAL_LAR_TIME,
                                #Biological Control
                          #BIOcov=INITIAL_BIO_COVERAGE,time_BIO_on=INITIAL_BIO_TIME,
                          
                                  #ATSB
                          ATSBcov=INITIAL_ATSB_COVERAGE,time_ATSB_on=INITIAL_ATSB_TIME,
                                #Space Spraying
                          SSPcov=INITIAL_SSP_COVERAGE,time_SSP_on=INITIAL_SSP_TIME,
                                  #Odor Traps
                          OBTcov=INITIAL_OBT_COVERAGE,time_OBT_on=INITIAL_OBT_TIME,
                                 #LLINs
                          ITNcov=INITIAL_ITN_COVERAGE,time_ITN_on=INITIAL_ITN_TIME,
                                  #IRS
                          IRScov=INITIAL_IRS_COVERAGE,time_IRS_on=INITIAL_IRS_TIME,
                                   #House Modification
                          HOUcov=INITIAL_HOU_COVERAGE,time_HOU_on=INITIAL_HOU_TIME,
                                  #Cattle - Systemic
                          ECScov=INITIAL_ECS_COVERAGE,time_ECS_on=INITIAL_ECS_TIME,
                                 #Cattle - topical
                          ECTcov=INITIAL_ECT_COVERAGE,time_ECT_on=INITIAL_ECT_TIME,
                                 #Resting & Ovipositing - Ovitraps --same for ATSB, SSP
                          OVIcov=INITIAL_OVI_COVERAGE,time_OVI_on=INITIAL_OVI_TIME)


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
#browser() IF YOU WANT TO DEBURG
