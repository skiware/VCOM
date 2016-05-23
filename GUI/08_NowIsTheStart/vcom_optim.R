######################################
######VCOM Optimization Routines######
######Sean Wu 5/19/2016###############
######################################

#load libraries and source files
library(parallel)
library(foreach)
library(doSNOW)
library(deSolve)
source("vcom_helperFunctions.R")

#optim_parms must be a vector of parameters to be optimized over
optim_IV <- function(optim_parms,tMax=365){
  theta <- getTheta(interventionParameters=getInterventionsParameters(ITN=optim_parms[1],IRS=optim_parms[2]))
  parms <- initParameters(theta)
  mod_traj <- data.frame(dede(y=parms$initState,times=seq_len(tMax)-1,parms=parms$theta,func=IVM_ode))
  return(mean(mod_traj$IV))
}

#box constrained optimization
optim(par=c(.1,.1),
      fn=optim_IV,
      method="L-BFGS-B",
      lower=rep(0,2),upper=rep(.8,2),
      control=list(trace=6,maxit=500))

#linearly constrained optimization
ui <- matrix(data=c(1,0,-1,0,0,1,0,-1),byrow=TRUE,ncol=2)
ci <- rep(c(0,-.8),2)
constrOptim(theta=c(.1,.1),
            f=optim_IV,
            ui=ui,ci=ci,
            method="Nelder-Mead",
            control=list(trace=6,maxit=500,alpha=1,beta=.5,gamma=2))

#grid search
optim_gridParms <- expand.grid(seq(0,1,by=0.01),seq(0,1,by=0.01))

cl <- makeCluster(spec=detectCores())
registerDoSNOW(cl)

optim_grid <- foreach(i=1:nrow(optim_gridParms),.combine="c",.verbose=TRUE,.packages="deSolve") %dopar% {
  optim_IV(optim_parms=as.numeric(optim_gridParms[i,]))
}

stopCluster(cl)
rm(cl)

saveRDS(object=optim_grid,file="optim_grid.RDS")

