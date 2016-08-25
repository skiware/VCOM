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
  plot=ggplot(IVM_traj, aes(x = time, y = IVM_traj, color = State)) +
    theme_grey(base_size = 18, base_family = "") +
    #geom_line(aes(y = SV+EV+IV, col = "NV"), size = 1.2) +
    geom_line(aes(y = IH, col = "Infected"), size = 1.75) +
    geom_line(aes(y = SH, col = "Suceptible"), size = 1.75) +
    ylim(0, NA) +
    labs(x = "Time (days)", y = "Number of humans")
}
plotEIR = function(IVM_traj){
  #. plotEIR_VC_R0: Plots EIR, VC and R0 dynamics of the system
  plot=ggplot(IVM_traj, aes(x = time, y = EIR, color = State)) +
    geom_line(aes(y = IVM_traj[["EIR"]], col = "EIR"), size = 1.75, colour = 'magenta') +
    ylim(0, NA) +
    labs(x = "Time (days)", y = "Entomological Inoculation Rate (EIR)")
}
plotVC = function(IVM_traj){
  #. plotVC
  plot=ggplot(IVM_traj, aes(x = time, y = VC, color = State)) +
    theme_grey(base_size = 18, base_family = "") +
    geom_line(aes(y = IVM_traj[["VC"]], col = "VC"), size = 1.75, colour = 'purple') +
    ylim(0, NA) +
    labs(x = "Time (days)", y = "Vectorial Capacity (VC)")
}
plotR0 = function(IVM_traj){
  #. plotR0: Plots EIR, VC and R0 dynamics of the system
  plot=ggplot(IVM_traj, aes(x = time, y = EIR, color = State)) +
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
    theme_grey(base_size = 18, base_family = "") +
    scale_y_log10() +
    guides(fill=FALSE) +
    labs(x="Mosquito Stage",y="Number of Mosquitoes")
}
########################################################################
LINE_WIDTH = 4
LINE_STYLE = list(width=LINE_WIDTH)
FONT_LABEL <<- list(
  size = 15,
  color = "grey"
)
FONT_TICKS <- list(
  size = 10,
  color = "grey"
)
AxisStyle = function(label){
  list(
    title = label,
    tickwidth = 2,
    titlefont = FONT_LABEL,
    tickfont = FONT_TICKS,
    tickcolor = toRGB("grey"),
    rangemode = "tozero",
    linewidth = 4,
    linecolor = "grey",
    mirror = "ticks"
  )
}
AxisStyleBar = function(label){
  list(
    title = label,
    tickwidth = 2,
    titlefont = FONT_LABEL,
    tickfont = list(size = 8, color = "black"),
    tickcolor = toRGB("grey"),
    linewidth = 4,
    linecolor = "grey",
    mirror = "ticks"
  )
}
AxisStyleLog = function(label){
  list(
    title = label,
    tickwidth = 2,
    titlefont = FONT_LABEL,
    tickfont = FONT_TICKS,
    tickcolor = toRGB("grey"),
    rangemode = "tozero",
    type = "log",
    linewidth = 4,
    linecolor = "grey",
    mirror = "ticks"
  )
}
AxisStyleLogBar = function(label){
  list(
    title = label,
    tickwidth = 2,
    titlefont = FONT_LABEL,
    tickfont = FONT_TICKS,
    tickcolor = toRGB("grey"),
    rangemode = "tozero",
    type = "log",
    linewidth = 4,
    linecolor = "grey",
    mirror = "ticks"
  )
}
plotTrajectoryPlotLy = function(IVM_traj){
  #. plotTrajectory: Plots the evolution of the dynamics of the system
  plot_ly(x = IVM_traj$time, y = IVM_traj$SV, line = LINE_STYLE, name = "Suceptible") %>%
    add_trace(y = IVM_traj$IV, line = LINE_STYLE, name = "Infected") %>%
    add_trace(y = IVM_traj$EV, line = LINE_STYLE, name = "Exposed") %>%
    layout(xaxis = AxisStyle("Time"), yaxis = AxisStyleLog("Number of Mosquitos"))
}
plotTrajectoryHumansPlotLy = function(IVM_traj){
  #. plotTrajectory: Plots the evolution of the dynamics of the system
  plot_ly(x = IVM_traj$time, y = IVM_traj$SH, line = LINE_STYLE, name = "Suceptible") %>%
    add_trace(y = IVM_traj$IH, line = LINE_STYLE, name = "Infected") %>%
    layout(xaxis = AxisStyle("Time"), yaxis = AxisStyle("Number of Humans"))
}
plotEIRPlotLy = function(IVM_traj){
  #. plotEIR_VC_R0: Plots EIR, VC and R0 dynamics of the system
  plot_ly(x = IVM_traj$time, y = IVM_traj$EIR, line = list(width=LINE_WIDTH,color="magenta"), name = "EIR") %>%
    layout(xaxis = AxisStyle("Time"), yaxis = AxisStyle("Entomological Inoculation Rate (EIR)"))
}
plotVCPlotLy = function(IVM_traj){
  #. plotVC
  plot_ly(x = IVM_traj$time, y = IVM_traj$VC, line = list(width=LINE_WIDTH,color="red"), name = "VC", color="cyan") %>%
    layout(xaxis = AxisStyle("Time"), yaxis = AxisStyle("Vectorial Capacity (VC)"))
}
plotR0PlotLy = function(IVM_traj){
  #. plotR0: Plots EIR, VC and R0 dynamics of the system
  plot_ly(x = IVM_traj$time, y = IVM_traj$R0, line = list(width=LINE_WIDTH,color="violet"), name = "R0") %>%
    layout(xaxis = AxisStyle("Time"), yaxis = AxisStyle("Basic Reproduction Rate (R0)"))
 }
barChartMosquitoDemographicsPlotLy = function(IVM_traj){
  traj_dat <- data.frame(stage=c("Early Instar","Late Instar","Pupae","Susceptible Adult","Exposed Adult","Infected Adult"),sum=c(sum(IVM_traj["EL"]),sum(IVM_traj["LL"]),sum(IVM_traj["PL"]),sum(IVM_traj["SV"]),sum(IVM_traj["EV"]),sum(IVM_traj["IV"])))
  traj_dat$stage <- factor(traj_dat$stage,levels=traj_dat$stage)
  plot_ly(x = c("Early Instar","Late Instar","Pupae","Susceptible Adult","Exposed Adult","Infected Adult"), y = traj_dat$sum,type = "bar", name = list("EL","LL","PL","SV","EV","IV"), color = list("EL","LL","PL","SV","EV","IV")) %>%
    layout(xaxis = AxisStyleBar(""), yaxis = AxisStyleLogBar("Number of Mosquitos"), showlegend = FALSE)
}
plotEIRVCR0PlotLy = function(IVM_traj){
  #. plotEIR_VC_R0: Plots EIR, VC and R0 dynamics of the system
  plot_ly(x = IVM_traj$time, y = IVM_traj$EIR, line = list(width=LINE_WIDTH,color="magenta"), name = "EIR", text="Entomological Inoculation Rate") %>%
    add_trace(y = IVM_traj$R0, line = list(width=LINE_WIDTH,color="blue"), name = "R0", text="Basic Reproduction Rate") %>%
    add_trace(y = IVM_traj$VC, line = list(width=LINE_WIDTH,color="red"), name = "VC", text="Vectorial Capacity") %>%
    layout(xaxis = AxisStyle("Time"), yaxis = AxisStyleLog("Magnitude"))
}
