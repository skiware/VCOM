### This is the driver ###

# Source all the required files
# Run the model and generate outputs

## ##########################

# Clear all stored parameters:
rm(list=ls())

library(shiny)
library(deSolve)
library(ggplot2)
library(gridGraphics)
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
source("multiplot.R")
## Get parameter for a specific specie ######

# Run first for An. gambiae -
MOSQUITO_PARAMETERS = getAnGambiaeParameters()

# simulation runs per day - enter the end time
INITIAL_MODELRUNTIME_VALUE   = 100

#Enter the coverage for a specific intervetion
# To turn it on - enter the required time < the max model run time
#To turn it off - enter INITIAL_MODELRUNTIME_VALUE + 1
#Source reduction, coverage value, time it is on
INITIAL_SRE_COVERAGE = 00
INITIAL_SRE_TIME     = INITIAL_MODELRUNTIME_VALUE + 1

#LArvaciding, coverage value, time it is on
INITIAL_LAR_COVERAGE = .00
INITIAL_LAR_TIME     = 0

#Biological, coverage value, time it is on
INITIAL_BIO_COVERAGE = .00
INITIAL_BIO_TIME     = INITIAL_MODELRUNTIME_VALUE + 1


#ATSB, coverage value, time it is on
INITIAL_ATSB_COVERAGE = .00
INITIAL_ATSB_TIME     = INITIAL_MODELRUNTIME_VALUE + 1

#Space Spraying, coverage value, time it is on
INITIAL_SSP_COVERAGE = .00
INITIAL_SSP_TIME     = INITIAL_MODELRUNTIME_VALUE + 1

#Odor Traps, coverage value, time it is on
INITIAL_OBT_COVERAGE = .00
INITIAL_OBT_TIME     = INITIAL_MODELRUNTIME_VALUE + 1
#LLINs, coverage value, time it is on
INITIAL_ITN_COVERAGE = .50
INITIAL_ITN_TIME     = 20
#IRS
INITIAL_IRS_COVERAGE = 0.50
INITIAL_IRS_TIME     = 40
#House modification
INITIAL_HOU_COVERAGE = 0.00
INITIAL_HOU_TIME     = INITIAL_MODELRUNTIME_VALUE + 1
#Cattle - Systemic
INITIAL_ECS_COVERAGE = 0.00
INITIAL_ECS_TIME     = INITIAL_MODELRUNTIME_VALUE + 1
#Cattle - Topical
INITIAL_ECT_COVERAGE = 0.00
INITIAL_ECT_TIME     = INITIAL_MODELRUNTIME_VALUE + 1

#Resting and Ovipositing - OviTraps -assuming same coverage for ATSB and SSP
INITIAL_OVI_COVERAGE = 0.00
INITIAL_OVI_TIME     = INITIAL_MODELRUNTIME_VALUE + 1

## Get intervetions parameters - LLINs for testing
#INTERVENTION_PARAMETERS = getInterventionsParameters(ITNcov=INITIAL_ITN_COVERAGE,time_ITN_on=INITIAL_ITN_TIME)

## Get intervetions parameters
INTERVENTION_PARAMETERS = getInterventionsParameters(
                                  #Source Reduction
                          SREcov=INITIAL_SRE_COVERAGE,time_SRE_on=INITIAL_SRE_TIME,
                                #Larvaciding
                          LARcov=INITIAL_LAR_COVERAGE,time_LAR_on=INITIAL_LAR_TIME,
                                #Biological Control
                          BIOcov=INITIAL_BIO_COVERAGE,time_BIO_on=INITIAL_BIO_TIME,

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

## Run the model - eventually produce different IVM_traj for different scenarios, eg., 1 intervention, 2 inter, etc
IVM_traj <<- runODE(INITIAL_MODELRUNTIME_VALUE,1,initState,theta,"lsoda")

## Generate outputs - using Histograms
barChartMosquitoDemographics(IVM_traj)

## Generate output - using gg plots
plotTrajectory(IVM_traj)


#compute and plot EIR, VC, R0
#If need be I can seprate the function for the three graphs
RunTime=seq(0, INITIAL_MODELRUNTIME_VALUE,by=1)
plotEIR_VC_R0(IVM_traj,theta,INITIAL_MODELRUNTIME_VALUE)




##Notes for Sam #######
#
#browser() IF YOU WANT TO DEBURG
