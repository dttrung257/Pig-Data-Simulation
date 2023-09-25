# Clear all variables, function, etc in Global Environment
rm(list = ls(all = TRUE))

setwd(getwd())
# Load data
load("data/JRPData.Rdata")

# Get working directory
wd <- getwd()

library(ggplot2)
library(fs)
library(dplyr)
library("minpack.lm")
library(nls2)
# Load functions
source("src/funcs.R")

unique.ids <- unique(JRP.estimate$ANIMAL_ID)
# data frame contain ideal trajectory include function type
ITC.Data.1 <- NULL

output.dir <- paste0(wd, '/graphs/Target/') # Folder get output images or html files from graph
clear_data(paste0(output.dir, 'TTC_PNG/')) # clear old images in CFI_PNG folder
check_path(paste0(output.dir, 'TTC_PNG/')) # If the directory does not exist, create a new directory


for (i in 1:length(unique.ids)) {
  id <- unique.ids[i]
  print(paste0('id: ', id))
  pig.data <- JRP.estimate[JRP.estimate$ANIMAL_ID==id,]
  X <- as.numeric(pig.data$AGE)
  expected.ages <- max(X) - min(X) +1
  
  if(length(X) < expected.ages) {
    next
  }
  Y <- as.numeric(pig.data$CFI)
  dfi <- as.numeric(pig.data$DFI)
  data.xy <- as.data.frame(cbind(X,Y))
  
  ### initial parameters for estimate parameters
  firstday <- X[1]
  lastday <- X[length(X)]
  
  ### provide set of initial parameter
  Xs.1 <- round(seq(firstday + 1, lastday - 1, len = 30), digits = 0)
  X0.1 <- rep(firstday, length(Xs.1))
  DFI.param <- NULL
  CFI.param <- NULL
  for (day in 1:length(Xs.1)) {
    DFI <- pig.data[pig.data$AGE == Xs.1[day],]$FEED_INTAKE
    CFI <- pig.data[pig.data$AGE == Xs.1[day],]$CFI
    DFI.param <- c(DFI.param,DFI)
    CFI.param <- c(CFI.param,CFI)
  }
  st1 <- data.frame(X0.1, Xs.1, DFI.param, CFI.param)
  names(st1) <- c("X0","Xs", "DFIs","CFIs")
  # fit model to data 
  st2 <- nls2(Y ~ nls.func(X0, Xs, DFIs, CFIs),
              data.xy,
              start = st1,
              algorithm = "brute-force")
  # data at changing quadratic to linear point
  par_init <- coef(st2)
  print(par_init)
  
  c <- target.coef(par_init)[3]
  print(paste0('c', c))
  Xs.0 <- par_init[2] # Quadratic to linear point
  if (c < 0) {
    func.type <- "LM"
  } else if (c > 0 && Xs.0 >= ((lastday - firstday) * 0.2 + firstday)) {
    func.type <- "QLM"
  } else {
    func.type <- "QDR"
  }
  print(paste("Functype of id: ", id, func.type))
  
  # Plot Function with initial parameters to data
  target_CFI.0 <- as.numeric(unlist(predict.target.trajectory(par_init, X)[1]))
  target_DFI.0 <- as.numeric(unlist(predict.target.trajectory(par_init, X)[2]))
  CFI.target.per.day <- data.frame(X, target_CFI.0)
  color_mapping <- c("Xs.0" = "green", "other" = "red")
  
  # Create a new column for color based on the value of X
  print(CFI.target.per.day$X)
  print(Xs.0)
  CFI.target.per.day$color <- ifelse(CFI.target.per.day$X < Xs.0, "Xs.0", "other")
  
  # Create the ggplot
  CFI.fig <- ggplot(CFI.target.per.day, aes(x = X, y = target_CFI.0, color = color)) +
    geom_line() +
    scale_color_manual(values = color_mapping) +  # Specify the color mapping
    labs(
      title = paste("Cummulative Feed Intake:", id, "\nFunction Type:", func.type),
      x = 'Age(days)', y = 'Cummulative Feed Intake'
    )
  ggsave(filename = paste0(output.dir, 'TTC_PNG/', id, ".", "Target.CFI", ".png"), plot = CFI.fig)
  
  id.1 <- rep(id,dim(pig.data)[1])
  func.type <- rep(func.type, dim(pig.data)[1])
  pig.data$FuncType <- func.type
  
  pig.data$id.1 <- id.1
  ITC.Data.1 <- rbind(ITC.Data.1 , pig.data)
}

CFI.LM <- subset(ITC.Data.1, func.type == "LM")
CFI.QDR <- subset(ITC.Data.1, func.type == "QDR")
CFI.QLM <- subset(ITC.Data.1, func.type == "QLM")
save(ITC.Data.1, CFI.LM, CFI.QDR, CFI.QLM, 
     file = "data/JRPData_TTC.RData")
