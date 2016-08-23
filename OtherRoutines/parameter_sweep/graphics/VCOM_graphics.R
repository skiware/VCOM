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


##########################################################
###Creating Figure for ITN + combinations to reduce EIR###
##########################################################

itn_n2_dat <- readRDS("C:/Users/WuS/Dropbox/IVM Team/SeanWu/VCOM/VCOM/OtherRoutines/parameter_sweep/FullTraj_sweepLLIN-2.rds")
itn_n2_tuples <- read.csv("C:/Users/WuS/Dropbox/IVM Team/SeanWu/VCOM/VCOM/OtherRoutines/parameter_sweep/tuple_namesLLIN-2.txt")

#filter data to not include interventions at 100% (negative index, ie; what NOT to include)

null_int <- c("SSP","IRS","BIO","SRE","ECS","OBT")
null_int_cols <- unname(unlist(sapply(null_int,function(x){
  grep(x,colnames(itn_n2_tuples))
},simplify=FALSE)))

filter_index <- NULL
for(i in 1:nrow(itn_n2_tuples)){
  if(any(itn_n2_tuples[i,grep("cov",colnames(itn_n2_tuples))] == 1) | 
     any(unlist(itn_n2_tuples[i,null_int_cols][,grep("cov",colnames(itn_n2_tuples[,null_int_cols]))]) != 0)){
    filter_index <- c(filter_index,i)
  } else {
    next()
  }
}

itn_n2_dat <- itn_n2_dat[-filter_index]
itn_n2_tuples <- itn_n2_tuples[-filter_index,]

#vector of indicies to sort data by smallest to largest post-intervention EIR
eir_sort <- order(sapply(itn_n2_dat,function(x){
  min(x$EIR)
}))

itn_n2_dat <- itn_n2_dat[eir_sort]
itn_n2_tuples <- itn_n2_tuples[eir_sort,]

itn_n2_tuples <- itn_n2_tuples[,-grep("time",colnames(itn_n2_tuples))]
itn_n2_tuples <- itn_n2_tuples[,-which(sapply(itn_n2_tuples,function(x){all(x==0)}))]

itn_n2_dat <- data.frame(itn_n2_tuples,"EIR"=sapply(itn_n2_dat,function(x){min(x$EIR)}))

#plot the 8 best combinations
itn_n2_plot_dat <- data.frame(EIR=itn_n2_dat[2:9,"EIR"])
itn_n2_plot_dat$combination <- LETTERS[1:8]

ggplot(data=itn_n2_plot_dat) +
  geom_bar(aes(x=combination,y=EIR,fill=combination),stat="identity",colour="black") +
  scale_y_continuous(labels=scales::scientific) +
  scale_x_discrete(labels=c("ITN 50%\nLarvacide 80%\nATSB 80%",
                            "ITN 50%\nLarvacide 60%\nATSB 80%",
                            "ITN 50%\nSpace Spray 80%\nATSB 80%",
                            "ITN 50%\nSpace Spray 60%\nATSB 80%",
                            "ITN 50%\nLarvacide 40%\nATSB 80%",
                            "ITN 50%\nSpace Spray 40%\nATSB 80%",
                            "ITN 50%\nTopical Endectocide 80%\nATSB 80%",
                            "ITN 50%\nLarvacide 20%\nATSB 80%")) +
  guides(fill=FALSE) +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_text(size=16,face="bold"),
        axis.text.x=element_text(size=12))


#################################################
###Creating Figures for ITN + IRS combinations###
#################################################

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

comb_iterator <- expand.grid(epsilon0=c(10,50,100),species=c(1,2,3),ITNcov=seq(0,1,by=0.05),IRScov=seq(0,1,by=0.05))
comb_iterator$time_ITN_on <- rep(50,3969)
comb_iterator$time_IRS_on <- rep(50,3969)
comb_iterator <-cbind(comb_iterator,replicate(n=length(parameter_names)-6,expr=rep(0,nrow(comb_iterator))))
colnames(comb_iterator)[7:ncol(comb_iterator)] <- parameter_names[!parameter_names %in% c("epsilon0","species","ITNcov","time_ITN_on","IRScov","time_IRS_on")]
comb_iterator <- comb_iterator[,parameter_names]

comb_iterator_list <- list()
for(i in 1:nrow(comb_iterator)){
  comb_iterator_list[[i]] <- as.list(comb_iterator[i,])
}

#sweep through parameters
cl <- makeCluster(spec=detectCores())
registerDoSNOW(cl)

comb_output <- foreach(i=comb_iterator_list,.packages=c("deSolve"),.verbose=TRUE) %dopar% {
  run_vcom(parameters=i,time_end=365)
}

stopCluster(cl)
rm(cl)

#save output
saveRDS(object=comb_output,file="itn_irs_comb.rds")
con <- file("itn_irs_tuples.txt","w+")
write(x=names(comb_iterator_list[[1]]),file=con,ncolumns=32,append=TRUE,sep=",")
for(i in 1:length(comb_iterator_list)){
  write(x=as.character(unname(comb_iterator_list[[i]])),file=con,ncolumns=32,append=TRUE,sep=",")
}
close.connection(con,"w+")

#load output
# comb_output <- readRDS(file="itn_irs_comb.rds")
# itn_irs_tuples <- read.csv("C:/Users/WuS/Dropbox/IVM Team/SeanWu/VCOM/VCOM/OtherRoutines/parameter_sweep/graphics/itn_irs_tuples.txt")

#plot grid heatmap
comb_df <- cbind(expand.grid(epsilon0=c("Baseline EIR: 10","Baseline EIR: 50","Baseline EIR: 100"),species=c("An. Gambiae","An. Arabiensis","An. Funestus"),ITNcov=seq(0,1,by=0.05),IRScov=seq(0,1,by=0.05)),
                 EIR=sapply(comb_output,function(x){x$EIR[366]}))

ggplot(data=comb_df) +
  geom_raster(aes(x=ITNcov,y=IRScov,fill=EIR),interpolate = TRUE) +
  geom_contour(aes(x=ITNcov,y=IRScov,z=EIR)) +
  facet_grid(epsilon0 ~ species) +
  scale_fill_continuous(low="darkblue",high="red") +
  scale_y_continuous(breaks=c(0,.25,.5,.75,1),labels=scales::percent) +
  scale_x_continuous(breaks=c(0,.25,.5,.75,1),labels=scales::percent) +
  labs(x="ITN Coverage",y="IRS Coverage") +
  theme_bw() +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        strip.text.x=element_text(size=12,face="bold.italic"),
        strip.text.y=element_text(size=10,face="bold"))

#plot grid heatmap with labeled contours
comb_v1 <- ggplot(data=comb_df) +
  geom_raster(aes(x=ITNcov,y=IRScov,fill=EIR),interpolate = TRUE) +
  facet_grid(epsilon0 ~ species) +
  scale_fill_continuous(low="darkblue",high="red") +
  scale_y_continuous(breaks=c(0,.25,.5,.75,1),labels=scales::percent) +
  scale_x_continuous(breaks=c(0,.25,.5,.75,1),labels=scales::percent) +
  labs(x="ITN Coverage",y="IRS Coverage") +
  theme_bw() +
  guides(fill=FALSE) +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        strip.text.x=element_text(size=12.5,face="bold.italic"),
        strip.text.y=element_text(size=11,face="bold"),
        axis.title.x=element_text(size=12.5,face="bold"),
        axis.title.y=element_text(size=12.5,face="bold"))

comb_v2 <- comb_v1 + 
  stat_contour(aes(x=ITNcov,y=IRScov,z=EIR,colour=..level..)) + 
  scale_colour_gradient(limits=c(1e3,2e3),na.value="#85c7f9")

direct.label(comb_v2,method="top.pieces")
