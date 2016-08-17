# Vector Control Optimization Model [VCOM]

## Description	
VCOM is a model created as part of the <a href="http://globalhealthsciences.ucsf.edu/global-health-group/malaria-elimination-initiative"> to help policy makers make decisions on which mosquito-control interventions should be prioritised in different given scenarios. To this end we have created two graphical user interfaces (GUIs) to make the model accessible to people without programming or modelling backgrounds.

Check out our <a href="http://chipdelmal.github.io/VCOM/">website</a> for more information.

<img src="./Media/ucsf.png" alt="University of California, San Francisco">

## Citation
VCOM, Awesome Journal

## Contact
Awesome people

## Parameters	
* OVIcov: Ovitraps coverage
* time_OVI_on: When Ovitraps are applied (days)
* FOGcov: Fogging coverage
* time_FOG_on: When Fogging is applied (days)
* LARcov: Larvacide coverage
* time_LAR_on: When Larvacide is applied (days)
* BIOcov: Biological Control coverage
* time_BIO_on: When Biological Control is applied (days)
* SREcov: Source Reduction coverage
* time_SRE_on: When Source Reduction is applied (days)
* IRScov: IRS coverage
* time_IRS_on: When IRSs are applied (days)
* ITNcov: ITN coverage
* time_ITN_on: When ITNs are applied (days)
* ECScov: endocticide coverage (systemic applied)
* time_ECS_on: When endocticide is (systemic) applied (days)
* ECTcov: endocticide coverage (topical applied)
* time_ECT_on: When endocticide is (topical) applied (days)
* HOUcov: House Modification coverage
* time_HOU_on: When House Modifications are applied (days)
* OBTcov: Odor Traps coverage (ratio of traps to human)
* time_OBT_on: When Odor Traps are applied (days)
* SPRcov: Spatial Repellents coverage
* time_SPR_on: When Spatial Repellents are applied (days)
* time_ATSB_on: When ATSB are applied (days)
* ATSBcov: ATSB coverage
* time_SSP_on: When Space spraying are applied (days)
* SSPcov: Space spraying coverage
* aOBT: availability of one odor baited trap in relation to one human
* SPRcov: Spatial Repellents coverage
* time_SPR_on: When Space Repellents are applied (days)
* PPMcov: NA
* time_PPM_on: NA
* muV: Adult mosquito daily mortality
* Q0: Human blood index
* phiB: Proportion of bites on a person while they are in bed
* phiI: Proportion of bites on a person while they are indoors
* rOVI: NA
* sOVI: NA
* rFOG: NA
* sFOG: NA
* rLAR: NA
* sLAR: NA
* rBIO: NA
* sBIO: NA
* rSRE: NA
* sSRE: NA
* rSPR: Probability of mosquito repeating a feeding attempt due to spatial repelent
* sSPR: Probability of mosquito feeding succesfully in presence to spatial repelent
* rPPM: Probability of mosquito repeating a feeding attempt due to personal protection measures
* sPPM: Probability of mosquito feeding succesfully in presence to personal protection measures
* rIRS: Probability of mosquito repeating a feeding attempt due to IRS
* sIRS: Probability of mosquito feeding and surviving in presence of IRS
* rITN: Probability of mosquito repeating a feeding attempt due to IRS
* sITN: Probability of mosquito feeding and surviving in presence of ITN
* rECT: Probability of mosquito repeating a feeding attempt due to instecide (topical) treated cattle
* sECT: Probability of mosquito feeding and surviving in presence of instecide (topical) treated cattle
* rECS: Probability of mosquito repeating a feeding attempt due to instecide (systemic) treated cattle
* sECS: Probability of mosquito feeding and surviving in presence of instecide (systemic) treated cattle
* dHOU: Death due to encountering mosquito proofed housing
* dIRS: Death due to encountering IRS treated house
* aOBT: availability of one odor baited trap in relation to one human
* eSRE: effectiveness of the impact of source reduction in K
* fATSB factor allowing for increased mosquito death due to ATBS: NA
* fLAR factor allowing for increased mosquito death due to larvaciding: NA
* fBIO factor allowing for increased mosquito death due to biological control: NA
* fSSP factor allowing for increased mosquito death due to space spraying: NA
* fOVI factor allowing for increased mosquito death due to space spraying: NA
* rHOU: NA
* sHOU: NA
* beta: Number of eggs laid per day per female mosquito
* muEL: Early larval instar daily mortality
* muLL: Late larvarl instar daily mortality
* muPL: Pupal daily mortality
* durEL: Duration of early instar stage
* durLL: Duration of late instar stage
* durPL: Pupal daily mortality
* durPL: Duration of pupal stage
* durEV: Duration of latent period in mosquito (days)
* gamma: Effect of density-dependence on late instars relative to early instars
* tau1: Time spent foraginf for a blood meal at 0% ITN coverage
* tau2: Time spent resting and ovipositing by a mosquito
* f0: Daily biting rate by mosquitoes on animals and humans
* epsilon0: Daily entomological inolculation rate
* iH_eq: Equilibrium malaria prevalence in humans
* NH_eq: Equilibrium human population size
* bV: Probability of transmission from human to vector per infectious bite
* bh: transmission efficiency from an infectious mosquito to an uninfected, susceptible human

## Functions	
* impactFeedingCycleParameters: compute the impact of interventions on feeding cycle parameters
* plotTrajectory: Plots the evolution of the dynamics of the system
* plotEIR_VC_R0: Plots EIR, VC and R0 dynamics of the system
* barChartMosquitoDemographics: Generates a bar chart with the amount of mosquitos in each life stage
* parseImportedCSVParameters: Once a csv file has been imported this function converts the data into a theta object
* importCSVParametersFromDirectory: Loads the verified parameters of the simulation from a CSV file into the workflow
* validateCSVParameters: Verifies that the user has imported a valid CSV file with all the parameters defined
* importXLSParametersFromDirectory: Loads the verified parameters of the simulation from an XLS file into the workflow
* importCSVXLSParametersFromDirectoryShiny: Wrapper function that works only within shiny to select the proper import function to use on a given file
* plotTrajectory: Plots the evolution of the dynamics of the system
* plotEIR_VC_R0: Plots EIR, VC and R0 dynamics of the system
* plot_SA_Figures - produce some figures for presentations: NA
* plot_SA_Figures - produce some figures for presentations: NA
* barChartMosquitoDemographics: Generates a bar chart with the amount of mosquitos in each life stage
* parseImportedCSVParameters: Once a csv file has been imported this function converts the data into a theta object
* importCSVParametersFromDirectory: Loads the verified parameters of the simulation from a CSV file into the workflow
* validateCSVParameters: Verifies that the user has imported a valid CSV file with all the parameters defined
* importXLSParametersFromDirectory: Loads the verified parameters of the simulation from an XLS file into the workflow
* importCSVXLSParametersFromDirectoryShiny: Wrapper function that works only within shiny to select the proper import function to use on a given file
* getInterventionsParameters: Returns the list of intervention parameters
* impactIndoorInterventions: compute both zcom and wcom for indoor interventions
* impactInsecticideTreatedCattle: compute both zcom and wcom for insecticide treated cattle (systemic and topical)
* impactSourceReduction: compute updated K due to source management
* impactLarvacidingBiologicalControl: compute the impact of larvaciding and biological control
* impactATSBSpaceSpraying: compute the impact of ATSB and SS on P1
* impactOdorBaitedTraps: compute the impact of odor baited traps in reducing Q0
* impactIndoorProtection: compute both zcom and wcom for indoor interventions
* impactInsecticideTreatedCattle: compute both zcom and wcom for insecticide treated cattle (systemic and topical)
* impactInsecticideTreatedCattle: compute both zcom and wcom for insecticide treated cattle (systemic and topical)
* impactRestingOvipositing: compute the impact of ATSB, Space Spraying, and ovitraps on muV_2
* computeLengthGonotrophicycle: Main function that return the length of gonotrophic cycle in presence of interventions
* computeHumanBitingRate: Main function that return human biting rate per mosquito in presence of intervetion
* computeMosqDensity: Main function that return mosquito density in presence of intervetion
* computeceHBI: Main function that return human blood index
* computeVC: Main function that return vectorial capacity
* computeEIR: Main function that return entomological innoculation rate
* computeRO : Main function that return basic reproduction rate
* runODE: Main ODE wrapper for simulating the mosquito population
* calculateInitialState: Calculates the initial conditions of a system given the theta parameters
* IVM_ode: ODE Model definition
* runODE: Main ODE wrapper for simulating the mosquito population
* calculateInitialState: Calculates the initial conditions of a system given the theta parameters
* IVM_ode: ODE Model definition
* getTheta: Facade function to return theta from selected parameters
* getAnGambiaeParameters: Returns the list of Anopheles Gambiae specific parameters
* getAnArabiensisParameters: Returns the list of Anopheles Arabiensis specific parameters
* getAnFunestusParameters: Returns the list of Anopheles Funestus specific parameters
* getMosquitoLifeCycleParameters: Returns the list of mosquito life cycle parameters
* getAdditionalTransmissionParameters: Returns the transmission parameters