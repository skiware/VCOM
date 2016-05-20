#############################
######VCOM Graphics##########
######Sean Wu 5/19/2016######
#############################


#load libraries
library(ggplot2)
library(reshape2)


###initial ITN/IRS sweeps###
sweep_00 <- read.csv("C:/Users/WuS/Dropbox/IVM Team/SeanWu/VCOM/param_sweeps/00.txt")
sweep_01 <- read.csv("C:/Users/WuS/Dropbox/IVM Team/SeanWu/VCOM/param_sweeps/01.txt")
sweep_10 <- read.csv("C:/Users/WuS/Dropbox/IVM Team/SeanWu/VCOM/param_sweeps/10.txt")
sweep_11 <- read.csv("C:/Users/WuS/Dropbox/IVM Team/SeanWu/VCOM/param_sweeps/11.txt")

sweep_00$id <- rep(1,length.out=nrow(sweep_00))
sweep_01$id <- rep(2,length.out=nrow(sweep_01))
sweep_10$id <- rep(3,length.out=nrow(sweep_10))
sweep_11$id <- rep(4,length.out=nrow(sweep_11))

sweep_df <- rbind(sweep_00,sweep_01,sweep_10,sweep_11)
sweep_df <- sweep_df[,-2]
sweep_melt <- melt(sweep_df,id.vars=c("id","X"))

ggplot(data=sweep_melt) +
  geom_line(aes(x=X,y=value,colour=variable),size=1.10) +
  facet_grid(.~id) +
  theme_bw()

###ITN/IRS grid sweep###

optim_grid <- readRDS(file="optim_grid.RDS")

optim_grid <- cbind(optim_gridParms,optim_grid)
names(optim_grid) <- c("ITN","IRS","Vector")

ggplot(data=optim_grid) +
  geom_raster(aes(x=ITN,y=IRS,fill=Vector)) +
  #scale_fill_gradient(low="blue",high="red") +
  scale_fill_gradientn(colours=terrain.colors(10)) + 
  theme_bw()
  
  