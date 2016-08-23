########################################################################
#=======================================================================
# ODEAuxiliaryFunctions.R
#    Contains the functions that deal with calling functions to compute model out and plotting different scenarios + performing other actions
#
#

#=======================================================================
########################################################################
plotTrajectory = function(IVM_traj){
  #. plotTrajectory: Plots the evolution of the dynamics of the system
  ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
    #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) +
    theme_grey(base_size = 18, base_family = "") +
    geom_line(aes(y = IV, col = "Infected"), size = 1.75) +
    geom_line(aes(y = SV, col = "Suceptible"), size = 1.75) +
    geom_line(aes(y = EV, col = "Exposed"), size = 1.75) +
    ylim(0, NA) +
    labs(x = "Time (days)", y = "Number of mosquitoes")
}
plotTrajectoryHumans = function(IVM_traj){
  #. plotTrajectory: Plots the evolution of the dynamics of the system
  ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
    theme_grey(base_size = 18, base_family = "") +
    #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) +
    geom_line(aes(y = IH, col = "Infected"), size = 1.75) +
    geom_line(aes(y = SH, col = "Suceptible"), size = 1.75) +
    ylim(0, NA) +
    labs(x = "Time (days)", y = "Number of humans")
}
plotEIR = function(IVM_traj){
  #. plotEIR_VC_R0: Plots EIR, VC and R0 dynamics of the system
  ggplot(IVM_traj, aes(x = time, y = EIR, color = State)) +
    geom_line(aes(y = IVM_traj[["EIR"]], col = "EIR"), size = 1.2) +
    labs(x = "Time (days)", y = " EIR")
}

plotEIR = function(IVM_traj){
  #. plotEIR_VC_R0: Plots EIR, VC and R0 dynamics of the system
   ggplot(IVM_traj, aes(x = time, y = EIR, color = State)) +
    theme_grey(base_size = 18, base_family = "") +
    geom_line(aes(y = IVM_traj[["EIR"]], col = "EIR"), size = 1.75, colour = 'magenta') +
    ylim(0, NA) +
    labs(x = "Time (days)", y = "Entomological Inoculation Rate (EIR)")
}
plotVC = function(IVM_traj){
  #. plotVC
  ggplot(IVM_traj, aes(x = time, y = VC, color = State)) +
    theme_grey(base_size = 18, base_family = "") +
    geom_line(aes(y = IVM_traj[["VC"]], col = "VC"), size = 1.75, colour = 'purple') +
    ylim(0, NA) +
    labs(x = "Time (days)", y = "Vectorial Capacity (VC)")
}
plotR0 = function(IVM_traj){
  #. plotR0: Plots EIR, VC and R0 dynamics of the system
  ggplot(IVM_traj, aes(x = time, y = EIR, color = State)) +
    theme_grey(base_size = 18, base_family = "") +
    geom_line(aes(y = IVM_traj[["R0"]], col = "R0"), size = 1.75, colour = "blue") +
    ylim(0, NA) +
    labs(x = "Time (days)", y = "Basic Reproduction Rate (R0)")
 }
barChartMosquitoDemographics <- function(IVM_traj){
  traj_dat <- data.frame(stage=c("EL","LL","PL","SV","EV","IV"),sum=c(sum(IVM_traj["EL"]),sum(IVM_traj["LL"]),sum(IVM_traj["PL"]),sum(IVM_traj["SV"]),sum(IVM_traj["EV"]),sum(IVM_traj["IV"])))
  traj_dat$stage <- factor(traj_dat$stage,levels=traj_dat$stage)
  ggplot(data=traj_dat,aes(stage,sum,fill=stage)) +
    geom_bar(stat="identity",colour="black") +
    scale_y_log10() +
    theme_bw() +
    guides(fill=FALSE) +
    labs(x="Mosquito Stage",y="Number of Mosquitoes")
}
