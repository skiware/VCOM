#######################################################################
######Routines to preform grid search of parameter space for VCOM######
######Sean Wu 8/19/2016################################################
#######################################################################

###load libraries and source files###

#set working directory to source file location
source("VCOM_initialization.R")
source("VCOM_output.R")
source("VCOM_model.R")

library(deSolve)
library(parallel)
library(doSNOW)
library(foreach)


####################################################
###Creating tuples of parameters to sweep through###
####################################################


#n is number of interventions to run through (eg; 15 choose n)
#epsilon0 is the baseline EIR
#species is the mosquito species desired
#time_on is when the intervention(s) activate
#granularity is how much to divide the coverage interval
create_tuples <- function(n,epsilon0,species,time_on,granularity){
  
  intervention_names <- c("OVIcov","FOGcov","LARcov","BIOcov","SREcov","IRScov","ITNcov","ECScov","ECTcov","HOUcov","OBTcov","SPRcov","PPMcov","ATSBcov","SSPcov")
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
  
  if(granularity < 1){
    stop("granularity must be greater than 1!")
  }
  interval <- 1/granularity
  n_parameters <- length(intervention_names)
  
  #for 1 parameter at a time
  if(n==1){
    
    cov_interval <- seq(from=0,to=1,by=interval) #generate 1-dimensional vector of parameter values
    
    cov_grid <- matrix(0,nrow=length(cov_interval)*n_parameters,ncol=n_parameters*2+2,dimnames=list(NULL,parameter_names))
    cov_grid[,"epsilon0"] <- epsilon0
    cov_grid[,"species"] <- species
    cov_grid[,grep("time",colnames(cov_grid))] <- 1
    
    rows_index <- list()
    tmp_rows <- cut(1:nrow(cov_grid),n_parameters,labels=FALSE)
    for(i in 1:n_parameters){
      rows_index[[i]] <- which(tmp_rows==i)
    }
    
    cols_index <- lapply(intervention_names,function(x){c(grep(x,colnames(cov_grid)),grep(x,colnames(cov_grid))+1)})
    
    for(i in 1:n_parameters){
      
      cov_grid[rows_index[[i]],cols_index[[i]][1]] <- cov_interval
      cov_grid[rows_index[[i]],cols_index[[i]][2]] <- time_on
      
    }
    
  }
  
  #for >1 parameter at a time
  if(n>1){
    
    tuples <- combn(1:n_parameters,n,simplify=FALSE) #generate n-dimensional combinations to sweep through
    cov_intervals <- lapply(1:n,function(x){seq(from=0,to=1,by=interval)})
    cov_intervals_tuples <- expand.grid(cov_intervals)
    
    tuple_mat <- NA
    
    duplicated_tuples <- lapply(1:n_parameters,function(j){
      which(sapply(tuples,function(x){x[1]==j}))[-1]
    })
    
    for(i in 1:length(tuples)){
      
      if(i %in% unlist(duplicated_tuples)){
        cov_intervals_tuples_i <- cov_intervals_tuples[-which(cov_intervals_tuples[,ncol(cov_intervals_tuples)]==0),]
      } else {
        cov_intervals_tuples_i <- cov_intervals_tuples
      }
      
      tuple_names <- intervention_names[tuples[[i]]]
      tuple_i_mat <- matrix(0,nrow=nrow(cov_intervals_tuples_i),ncol=n_parameters*2+2,dimnames=list(NULL,parameter_names))
      tuple_i_mat[,"epsilon0"] <- epsilon0
      tuple_i_mat[,"species"] <- species
      tuple_i_mat[,grep("time",colnames(tuple_i_mat))] <- 1
      tuple_i_mat[,tuple_names] <- unname(as.matrix(cov_intervals_tuples_i))
      tuple_i_mat[,which(colnames(tuple_i_mat) %in% tuple_names)+1]  <- matrix(time_on,nrow=nrow(cov_intervals_tuples_i),ncol=ncol(cov_intervals_tuples_i))
      
      tuple_mat <- rbind(tuple_mat,tuple_i_mat)
    }
    
    tuple_mat <- tuple_mat[-1,]
    
  }
  
  if(n==1){
    
    data_names <- colnames(cov_grid)
    cov_list <- as.list(data.frame(t(cov_grid)))
    for(i in 1:length(cov_list)){
      names(cov_list[[i]]) <- colnames(cov_grid)
    }
    return(cov_list)
    
  } else {
    
    data_names <- colnames(tuple_mat)
    tuple_list <- as.list(data.frame(t(tuple_mat)))
    for(i in 1:length(tuple_list)){
      names(tuple_list[[i]]) <- colnames(tuple_mat)
    }
    return(tuple_list)
    
  }
}

#constant is a character vector indicating which intervention(s) to hold constant (OVIcov, FOGcov, etc) 
#constant_cov is a numeric vector [0,1] indicating the constant level(S) for that intervention(s)
create_tuples_constant <- function(n,epsilon0,constant,constant_cov,species,time_on,granularity){
  
  intervention_names <- c("OVIcov","FOGcov","LARcov","BIOcov","SREcov","IRScov","ITNcov","ECScov","ECTcov","HOUcov","OBTcov","SPRcov","PPMcov","ATSBcov","SSPcov")
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
  
  #sanity checks
  if(granularity < 1){
    stop("granularity must be greater than 1!")
  }
  if(length(constant)!=length(constant_cov)){
    stop("constant and constant_cov must be character and numeric vectors of the same length!")
  }
  if(n > length(intervention_names)){
    stop("cannot sweep through more dimensions than exist!")
  }
  
  #set up iterators for interventions held at a constant value
  constant_length <- attributes(regexpr("^[A-Z]{3,4}",constant))$match.length
  constant_time <- paste0("time_",unname(mapply(FUN=function(x,y){substr(x,1,y)},x=constant,y=constant_length)),"_on")
  
  #levels to sweep through
  interval <- 1/granularity
  n_parameters <- length(intervention_names) - length(constant)
  
  #for 1 parameter at a time
  if(n==1){
    
    cov_interval <- seq(from=0,to=1,by=interval) #generate 1-dimensional vector of parameter values
    
    cov_grid <- matrix(0,nrow=length(cov_interval)*n_parameters,ncol=n_parameters*2+2,dimnames=list(NULL,parameter_names[!(parameter_names %in% c(constant_time,constant))]))
    cov_grid[,"epsilon0"] <- epsilon0
    cov_grid[,"species"] <- species
    cov_grid[,grep("time",colnames(cov_grid))] <- 1
    
    rows_index <- list()
    tmp_rows <- cut(1:nrow(cov_grid),n_parameters,labels=FALSE)
    for(i in 1:n_parameters){
      rows_index[[i]] <- which(tmp_rows==i)
    }
    
    cols_index <- lapply(intervention_names[!(intervention_names %in% constant)],function(x){c(grep(x,colnames(cov_grid)),grep(x,colnames(cov_grid))+1)})
    
    for(i in 1:n_parameters){
      
      cov_grid[rows_index[[i]],cols_index[[i]][1]] <- cov_interval
      cov_grid[rows_index[[i]],cols_index[[i]][2]] <- time_on
      
    }
    
    for(i in 1:length(constant)){
      
      cov_grid <- cbind(cov_grid,rep(constant_cov[i],nrow(cov_grid)))
      colnames(cov_grid)[ncol(cov_grid)] <- constant[i]
      cov_grid <- cbind(cov_grid,rep(time_on,nrow(cov_grid)))
      colnames(cov_grid)[ncol(cov_grid)] <- constant_time[i]
      
    }
    
    cov_grid <- cov_grid[,parameter_names]
  }
  
  #for >1 parameter at a time
  if(n>1){
    
    tuples <- combn(1:n_parameters,n,simplify=FALSE) #generate n-dimensional combinations to sweep through
    cov_intervals <- lapply(1:n,function(x){seq(from=0,to=1,by=interval)})
    cov_intervals_tuples <- expand.grid(cov_intervals)
    
    tuple_mat <- NA
    
    duplicated_tuples <- lapply(1:n_parameters,function(j){
      which(sapply(tuples,function(x){x[1]==j}))[-1]
    })
    
    for(i in 1:length(tuples)){
      
      if(i %in% unlist(duplicated_tuples)){
        cov_intervals_tuples_i <- cov_intervals_tuples[-which(cov_intervals_tuples[,ncol(cov_intervals_tuples)]==0),]
      } else {
        cov_intervals_tuples_i <- cov_intervals_tuples
      }
      
      tuple_names <- intervention_names[!(intervention_names %in% constant)][tuples[[i]]]
      tuple_i_mat <- matrix(0,nrow=nrow(cov_intervals_tuples_i),ncol=n_parameters*2+2,dimnames=list(NULL,parameter_names[!(parameter_names %in% c(constant_time,constant))]))
      tuple_i_mat[,"epsilon0"] <- epsilon0
      tuple_i_mat[,"species"] <- species
      tuple_i_mat[,grep("time",colnames(tuple_i_mat))] <- 1
      tuple_i_mat[,tuple_names] <- unname(as.matrix(cov_intervals_tuples_i))
      tuple_i_mat[,which(colnames(tuple_i_mat) %in% tuple_names)+1]  <- matrix(time_on,nrow=nrow(cov_intervals_tuples_i),ncol=ncol(cov_intervals_tuples_i))
      
      tuple_mat <- rbind(tuple_mat,tuple_i_mat)
    }
    
    tuple_mat <- tuple_mat[-1,]
    
    for(i in 1:length(constant)){
      
      tuple_mat <- cbind(tuple_mat,rep(constant_cov[i],nrow(tuple_mat)))
      colnames(tuple_mat)[ncol(tuple_mat)] <- constant[i]
      tuple_mat <- cbind(tuple_mat,rep(time_on,nrow(tuple_mat)))
      colnames(tuple_mat)[ncol(tuple_mat)] <- constant_time[i]
      
    }
    
    tuple_mat <- tuple_mat[,parameter_names]
  }
  
  if(n==1){
    
    data_names <- colnames(cov_grid)
    cov_list <- as.list(data.frame(t(cov_grid)))
    for(i in 1:length(cov_list)){
      names(cov_list[[i]]) <- colnames(cov_grid)
    }
    return(cov_list)
    
  } else {
    
    data_names <- colnames(tuple_mat)
    tuple_list <- as.list(data.frame(t(tuple_mat)))
    for(i in 1:length(tuple_list)){
      names(tuple_list[[i]]) <- colnames(tuple_mat)
    }
    return(tuple_list)
    
  }
}


######################
###Parameter Sweeps###
######################


###run parameter sweep in parallel###
vcom_iterator <- create_tuples(n=1,epsilon0=100,species=3,time_on=20,granularity=5)

cl <- makeCluster(spec=detectCores())
registerDoSNOW(cl)

vcom_parameter_sweep <- foreach(i=vcom_iterator,.packages=c("deSolve"),.verbose=TRUE) %dopar% {
  run_vcom(parameters=as.list(i),time_end=150)
}

stopCluster(cl)
rm(cl)

#export results
EIR_sweep <- sapply(vcom_parameter_sweep,function(x){
  x$EIR[length(x$EIR)]
})

write(EIR_sweep,ncolumns=1,file="EIR_sweep.txt",sep=",")

con <- file("tuple_names.txt","w+")
write(x=names(vcom_iterator[[1]]),file=con,ncolumns=32,append=TRUE,sep=",")
for(i in 1:length(vcom_iterator)){
  write(x=as.character(unname(vcom_iterator[[i]])),file=con,ncolumns=32,append=TRUE,sep=",")
}
close.connection(con,"w+")


###run parameter sweep in parallel holding ITN constant###
vcom_iterator_llin <- create_tuples_constant(n=2,epsilon0=50,constant="ITNcov",constant_cov=.5,species=2,time_on=20,granularity=5)

cl <- makeCluster(spec=detectCores())
registerDoSNOW(cl)

vcom_parameter_sweep_llin <- foreach(i=vcom_iterator_llin,.packages=c("deSolve"),.verbose=TRUE) %dopar% {
  run_vcom(parameters=as.list(i),time_end=150)
}

stopCluster(cl)
rm(cl)

#export results
EIR_sweepLLIN <- sapply(vcom_parameter_sweep_llin,function(x){
  x$EIR[length(x$EIR)]
})

write(EIR_sweepLLIN,ncolumns=1,file="EIR_sweepLLIN-2.txt",sep=",")

con <- file("tuple_namesLLIN-2.txt","w+")
write(x=names(vcom_iterator_llin[[1]]),file=con,ncolumns=32,append=TRUE,sep=",")
for(i in 1:length(vcom_iterator_llin)){
  write(x=as.character(unname(vcom_iterator_llin[[i]])),file=con,ncolumns=32,append=TRUE,sep=",")
}
close.connection(con,"w+")