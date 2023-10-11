# Clear all variables, function, etc in Global Environment
rm(list = ls(all = TRUE))

setwd(getwd())

# Get working directory
wd <- getwd()

library(ggplot2)
library(fs)
library(dplyr)
library("minpack.lm")
library("nlstools")
library(nls2)
# Load functions
source("src/funcs.R")

# Load data
# load("data/JRPData.Rdata")
load("Data/JRP.DFI.neg.RData")
load("Data/JRP.DFI.pos2.RData")
load("Data/JRP.DFI.pos1.RData")
load("data/JRPData_TTC.Rdata")

output.dir <- paste0(wd, '/graphs/Target/') # Folder get output images or html files from graph
clear_data(paste0(output.dir, 'TTC_PNG_FINAL/')) # clear old images in CFI_PNG folder
check_path(paste0(output.dir, 'TTC_PNG_FINAL/')) # If the directory does not exist, create a new directory

#Order number of Animal_ID
ID <- unique(as.factor(ITC.Data.1$ANIMAL_ID))

#===============================================================
# For loop for automatically estimating ITC of all pigs
#===============================================================
IDC <- seq_along(ID)
for (idc in IDC){
  # idc = 3
  i <- ID[idc]
  Data <- ITC.Data.1[ITC.Data.1$ANIMAL_ID == i,]
  idc1 <- unique(as.numeric(Data$idc.1))
  AGE <- Data$AGE # initial Ages of Animal i
  CFI.exp <- Data$CFI # initial CFI of animal i
  DFI.exp <- Data$FEED_INTAKE # initial DFI of animal i
  ITC <- NULL
  ITD <- NULL
  Xs <- NULL
  func.type <- NULL
  
  #Choose the right function for the right animal
  
  if(i %in% unique(ITC.param.neg$ANIMAL_ID)){
    func.type <- "LM"
    param.i <- ITC.param.neg[ITC.param.neg$ANIMAL_ID == i,]
    FuncType <- param.i[dim(param.i)[1],]$FuncType
    Slope <- param.i[dim(param.i)[1],]$Slope
    param.i <- as.numeric(param.i[dim(param.i)[1], 4:5])
    ITC <- predict.target.trajectory.0(param.i, AGE)[[1]]
    ITD <- rep(param.i[2], length(AGE))
    
    Data.remain <- Age.remain.neg[Age.remain.neg$ANIMAL_ID == i,]
    Age.remain <- Data.remain$Age
    DFI.remain <- Data.remain$Observed_DFI
    CFI.remain <- Data.remain$Observed_CFI
    
    
  } else if(i %in% unique(ITC.param.pos1$ANIMAL_ID)){
    func.type <- "QDR"
    param.i <- ITC.param.pos1[ITC.param.pos1$ANIMAL_ID == i,]
    # print(param.i)
    FuncType <- param.i[dim(param.i)[1],]$FuncType
    Slope <- param.i[dim(param.i)[1],]$Slope
    param.i <- as.numeric(param.i[dim(param.i)[1], 5:7])
    ITC <- predict.target.trajectory.1(param.i, AGE)[[1]]
    ITD <- predict.target.trajectory.1(param.i, AGE)[[2]]
    
    Data.remain <- Age.remain.pos1[Age.remain.pos1$ANIMAL_ID == i,]
    Age.remain <- Data.remain$Age
    DFI.remain <- Data.remain$Observed_DFI
    CFI.remain <- Data.remain$Observed_CFI
    
  } else{
    func.type <- "QLM"
    param.i <- ITC.param.pos2[ITC.param.pos2$ANIMAL_ID == i,]
    FuncType <- param.i[dim(param.i)[1],]$FuncType
    Slope <- param.i[dim(param.i)[1],]$Slope
    Xs <- param.i[dim(param.i)[1],]$Xs
    param.i <- as.numeric(param.i[dim(param.i)[1], 2:5])
    ITC <- predict.target.trajectory.2(param.i, AGE)[[1]]
    ITD <- predict.target.trajectory.2(param.i, AGE)[[2]]
    
    Data.remain <- Age.remain.pos2[Age.remain.pos2$ANIMAL_ID == i,]
    Age.remain <- Data.remain$Age
    DFI.remain <- Data.remain$Observed_DFI
    CFI.remain <- Data.remain$Observed_CFI
    
  }
  # print(paste0("id: ", i))
  x <- AGE
  CFI.target.per.day <- data.frame(x, ITC)
  
  # Create a new column for color based on the value of X
  if (!is.null(Xs)) {
    CFI.target.per.day$QLM_COLOR <- ifelse(CFI.target.per.day$x <= Xs, "Quadratic", "Linear")
  }
  
  CFI.fig <- NULL
  if (i %in% unique(ITC.param.pos2$ANIMAL_ID)) {
    # Create the ggplot using geom_segment
    CFI.fig <- ggplot(CFI.target.per.day,
                      aes(x = x, xend = lead(x, order_by = x), y = ITC, yend = lead(ITC, order_by = x))) +
      geom_segment(aes(color = QLM_COLOR)) +
      labs(
        title = paste("Cummulative Feed Intake:", i, "\nFunction Type: QLM"),
        x = 'Age(days)', y = 'Cummulative Feed Intake'
      ) +
      scale_color_manual(values = c("Quadratic" = "purple", "Linear" = "yellow"))
  } else if (i %in% unique(ITC.param.pos1$ANIMAL_ID)) {
    CFI.fig <- ggplot(CFI.target.per.day,
                      aes(x = x, xend = lead(x, order_by = x), y = ITC, yend = lead(ITC, order_by = x))) +
      geom_segment(aes(color = "Quadratic")) +
      labs(
        title = paste("Cummulative Feed Intake:", i, "\nFunction Type: QDR"),
        x = 'Age(days)', y = 'Cummulative Feed Intake'
      ) +
      scale_color_manual(values = c("Quadratic" = "purple"))
  } else {
    CFI.fig <- ggplot(CFI.target.per.day,
                      aes(x = x, xend = lead(x, order_by = x), y = ITC, yend = lead(ITC, order_by = x))) +
      geom_segment(aes(color = "Linear")) +
      labs(
        title = paste("Cummulative Feed Intake:", i, "\nFunction Type: LM"),
        x = 'Age(days)', y = 'Cummulative Feed Intake'
      ) +
      scale_color_manual(values = c("Linear" = "yellow"))
  }
  ggsave(filename = paste0(output.dir, 'TTC_PNG_FINAL/', i, ".", "Target.CFI", ".", func.type, ".png"), plot = CFI.fig, width = 10, height = 10)

} # end FOR loop
