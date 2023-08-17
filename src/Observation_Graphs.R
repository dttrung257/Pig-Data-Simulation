rm(list=ls(all=TRUE))
setwd("D:/Documents/Pig-Data-Simulation")
load("data/JRPData.Rdata")
wd <- getwd()
# set library for create plot
library(ggplot2)
library(htmlwidgets)
library(reticulate)
#Order number of Animal_ID
ID <- unique(JRP_NA$ANIMAL_ID)

#Pig ID
Pig_ID <- JRP_NA$ANIMAL_ID

#Age (days)
Age <- JRP_NA$AGE
#Daily Feed Intake (kg)
DFI <- JRP_NA$FEED_INTAKE


for(i in seq_along(ID)) {
  id <- ID[i]
  Data <- JRP_NA[JRP_NA$ANIMAL_ID == id,]
  DFI.fig <- ggplot(Data,aes(x=AGE,y=FEED_INTAKE))+geom_point(color='blue')
  DFI.fig <- DFI.fig + labs(title=paste0('Daily Feed Intake\n','PigID:',id), x='Age (d)', y='Daily Feed Intake, kg')
  ggsave(filename=paste0(wd,"/graphs/Observation/DFI_PNG/", id, ".", "DFI", ".png"),plot=DFI.fig)  
  
  CFI.fig <- ggplot(Data,aes(x=AGE,y=CFI))+geom_point(color='blue')
  CFI.fig <- CFI.fig + labs(title=paste0('Cumulative Feed Intake\n','PigID:',id), x='Age (d)', y='Cumulative Feed Intake, kg')
  ggsave(filename=paste0(wd,"/graphs/Observation/CFI_PNG/", id, ".", "CFI", ".png"),plot=CFI.fig)  
  
  
}

