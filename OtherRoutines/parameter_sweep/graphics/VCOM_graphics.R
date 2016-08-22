##########################################################
######Routines to generate plots for VCOM manuscript######
######Sean Wu 8/22/2016###################################
##########################################################

#

# Clear all stored parameters:
rm(list=ls())
###load libraries and source files##
#source model files
#source("C:/Users/WuS/Dropbox/IVM Team/SeanWu/VCOM/VCOM/OtherRoutines/parameter_sweep/VCOM_initialization.R")
#source("C:/Users/WuS/Dropbox/IVM Team/SeanWu/VCOM/VCOM/OtherRoutines/parameter_sweep/VCOM_output.R")
#source("C:/Users/WuS/Dropbox/IVM Team/SeanWu/VCOM/VCOM/OtherRoutines/parameter_sweep/VCOM_model.R")

#Sourcing from SK laptop

source("C:/Users/skiware/Documents/VCOM/VCOM/OtherRoutines/parameter_sweep/VCOM_initialization.R")
source("C:/Users/skiware/Documents/VCOM/VCOM/OtherRoutines/parameter_sweep/VCOM_output.R")
source("C:/Users/skiware/Documents/VCOM/VCOM/OtherRoutines/parameter_sweep/VCOM_model.R")


library(deSolve)
library(ggplot2)
library(parallel)
library(doSNOW)
library(foreach)

#######################
###Creating Figure 1###
#######################

#create the iterator
parameter_names <- c("epsilon0","species","OVIcov","time_OVI_on","FOGcov","time_FOG_on","LARcov","time_LAR_on",
                     "BIOcov",   "time_BIO_on",
                     "SREcov",   "time_SRE_on",
                     "IRScov",   "time_IRS_on",
                     "ITNcov",   "time_ITN_on",
                     "ECScov",   "time_ECS_on",
                     "ECTcov",   "time_ECT_on",
                     "HOUcov",   "time_HOU_on",
                     "OBTcov",   "time_OBT_on",
                     "SPRcov",   "time_SPR_on",
                     "PPMcov",   "time_PPM_on",
                     "ATSBcov",  "time_ATSB_on",
                     "SSPcov",   "time_SSP_on")

fig1_iterator <- expand.grid(epsilon0=c(10,50,100),species=c(1,2,3),ITNcov=c(0,0.5,0.8))
fig1_iterator$time_ITN_on <- rep(50,27)
fig1_iterator <- cbind(fig1_iterator,replicate(n=length(parameter_names)-4,expr=rep(0,nrow(fig1_iterator))))
colnames(fig1_iterator)[5:ncol(fig1_iterator)] <- parameter_names[!parameter_names %in% c("epsilon0","species","ITNcov","time_ITN_on")]
fig1_iterator <- fig1_iterator[,parameter_names]

fig1_iterator_list <- list()
for(i in 1:nrow(fig1_iterator)){
  fig1_iterator_list[[i]] <- as.list(fig1_iterator[i,])
}

#sweep through parameters
cl <- makeCluster(spec=detectCores())
registerDoSNOW(cl)

fig1_output <- foreach(i=fig1_iterator_list,.packages=c("deSolve"),.verbose=TRUE) %dopar% {
  run_vcom(parameters=i,time_end=365)
}

stopCluster(cl)
rm(cl)

fig1_df <- cbind(expand.grid(epsilon0=c(10,50,100),species=c("An. Gambiae","An. Arabiensis","An. Funestus"),ITNcov=c(0,0.5,0.8)),
                 EIR=sapply(fig1_output,function(x){x$EIR[366]}))

#plot the data as heatmap
ggplot(data=fig1_df) +
  geom_raster(aes(x=ITNcov,y=epsilon0,fill=EIR)) +
  # scale_fill_gradientn(colours = heat.colors(10)) +
  geom_text(aes(x=ITNcov,y=epsilon0,label=round(EIR,digits=2)),color="white",size=4.5) +
  facet_grid(~species) +
  scale_y_continuous(breaks=c(10,50,100)) +
  scale_x_continuous(breaks=c(0,0.50,0.80),labels=scales::percent) +
  guides(fill=FALSE) +
  labs(x="ITN Coverage",y="Baseline EIR") +
  theme_bw(base_size = 18) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        strip.text.x=element_text(size=12,face="bold.italic"))
  
#plot the data as lines
fig1_line_df <- data.frame(EIR=unlist(lapply(fig1_output,function(x){x$EIR})))
fig1_line_df$epsilon0 <- rep(expand.grid(epsilon0=c("Baseline EIR: 10","Baseline EIR: 50","Baseline EIR: 100"),species=c("An. Gambiae","An. Arabiensis","An. Funestus"),ITNcov=c(0,0.5,0.8))$epsilon0,each=366)
fig1_line_df$species <- rep(expand.grid(epsilon0=c("Baseline EIR: 10","Baseline EIR: 50","Baseline EIR: 100"),species=c("An. Gambiae","An. Arabiensis","An. Funestus"),ITNcov=c(0,0.5,0.8))$species,each=366)
fig1_line_df$ITNcov <- rep(expand.grid(epsilon0=c("Baseline EIR: 10","Baseline EIR: 50","Baseline EIR: 100"),species=c("An. Gambiae","An. Arabiensis","An. Funestus"),ITNcov=c(0,0.5,0.8))$ITNcov,each=366)
fig1_line_df$time <- rep(1:366,27)

ggplot(data=fig1_line_df) +
  geom_line(aes(x=time,y=EIR,color=as.factor(ITNcov),group=ITNcov),size=1.75,alpha=0.75) +
  scale_color_discrete(name="ITN\nCoverage",labels=c("0% ITN","50% ITN","80% ITN")) +
  facet_grid(epsilon0 ~ species,scales="free_y") +
  #facet_grid(epsilon0 ~ species) +
  labs(x="Time (Days)") +
  guides(colour=guide_legend(override.aes=list(alpha=1))) +
  theme_bw(base_size = 18) +
  theme(axis.title.y=element_blank(),
        strip.text.x=element_text(size=12,face="bold.italic"),
        strip.text.y=element_text(size=10,face="bold"))


