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
  x <- as.numeric(pig.data$AGE)
  expected.ages <- max(x) - min(x) +1
  
  if(length(x) < expected.ages) {
    next
  }
  Y <- as.numeric(pig.data$CFI)
  dfi <- as.numeric(pig.data$DFI)
  data.xy <- as.data.frame(cbind(x,Y))
  
  ### initial parameters for estimate parameters
  firstday <- x[1]
  lastday <- x[length(x)]
  
  ### provide set of initial parameter
  Xs.1 <- round(seq(firstday + 1, lastday - 1, len = 30), digits = 0)
  X0.1 <- rep(firstday, length(Xs.1))
  DFI.param <- NULL
  CFI.param <- NULL
  for (day in 1:length(Xs.1)) {
    DFI <- pig.data[pig.data$AGE == Xs.1[day],]$FEED_INTAKE
    CFI <- pig.data[pig.data$AGE == Xs.1[day],]$CFI
    DFI.param <- c(DFI.param, DFI)
    CFI.param <- c(CFI.param, CFI)
  }
  st1 <- data.frame(X0.1, Xs.1, DFI.param, CFI.param)
  names(st1) <- c("X0","Xs", "DFIs","CFIs")
  # fit model to data 
  st2 <- nls2(Y ~ nls.func.2(X0, Xs, DFIs, CFIs),
              data.xy,
              start = st1,
              algorithm = "brute-force")
  # data at changing quadratic to linear point
  par_init <- coef(st2)
  print(par_init)
  
  c <- target.coef.2(par_init)[3]
  Xs.0 <- par_init[2] # Quadratic to linear point
  if (c < 0) {
    func.type <- "LM"
  } else if (c > 0 && Xs.0 >= ((lastday - firstday) * 0.2 + firstday)) {
    func.type <- "QLM"
  } else {
    func.type <- "QDR"
  }
  print(paste0("Functype of id: ", id, " ", func.type))
  print(paste0("Xs: ", Xs.0))
  
  # Plot Function with initial parameters to data
  target_CFI.0 <- as.numeric(unlist(predict.target.trajectory.2(par_init, x)[1]))
  target_DFI.0 <- as.numeric(unlist(predict.target.trajectory.2(par_init, x)[2]))
  CFI.target.per.day <- data.frame(x, target_CFI.0)
  
  # Create a new column for color based on the value of X
  CFI.target.per.day$QLM_COLOR <- ifelse(CFI.target.per.day$x <= Xs.0 & func.type == "QLM", "Quadratic", "Linear")
  
  CFI.fig <- NULL
  if (func.type == "QLM") {
  # Create the ggplot using geom_segment
    CFI.fig <- ggplot(CFI.target.per.day,
                      aes(x = x, xend = lead(x, order_by = x), y = target_CFI.0, yend = lead(target_CFI.0, order_by = x))) +
      geom_segment(aes(color = QLM_COLOR)) +
      labs(
        title = paste("Cummulative Feed Intake:", id, "\nFunction Type:", func.type),
        x = 'Age(days)', y = 'Cummulative Feed Intake'
      ) +
      scale_color_manual(values = c("Quadratic" = "purple", "Linear" = "yellow"))
  } else if (func.type == "QDR") {
    CFI.fig <- ggplot(CFI.target.per.day,
                      aes(x = x, xend = lead(x, order_by = x), y = target_CFI.0, yend = lead(target_CFI.0, order_by = x))) +
      geom_segment(aes(color = "Quadratic")) +
      labs(
        title = paste("Cummulative Feed Intake:", id, "\nFunction Type:", func.type),
        x = 'Age(days)', y = 'Cummulative Feed Intake'
      ) +
      scale_color_manual(values = c("Quadratic" = "purple"))
  } else {
    CFI.fig <- ggplot(CFI.target.per.day,
                      aes(x = x, xend = lead(x, order_by = x), y = target_CFI.0, yend = lead(target_CFI.0, order_by = x))) +
      geom_segment(aes(color = "Linear")) +
      labs(
        title = paste("Cummulative Feed Intake:", id, "\nFunction Type:", func.type),
        x = 'Age(days)', y = 'Cummulative Feed Intake'
      ) +
      scale_color_manual(values = c("Linear" = "yellow"))
  }
  ggsave(filename = paste0(output.dir, 'TTC_PNG/', id, ".", "Target.CFI", ".", func.type, ".png"), plot = CFI.fig)
  
  id.1 <- rep(id,dim(pig.data)[1])
  func.type <- rep(func.type, dim(pig.data)[1])
  pig.data$FuncType <- func.type
  
  pig.data$id.1 <- id.1
  ITC.Data.1 <- rbind(ITC.Data.1, pig.data)
}

CFI.LM <- unique(ITC.Data.1[ITC.Data.1$FuncType == "LM", ]$ANIMAL_ID)
CFI.QDR <- unique(ITC.Data.1[ITC.Data.1$FuncType == "QDR", ]$ANIMAL_ID)
CFI.QLM <- unique(ITC.Data.1[ITC.Data.1$FuncType == "QLM", ]$ANIMAL_ID)
save(ITC.Data.1, CFI.LM, CFI.QDR, CFI.QLM, 
     file = "data/JRPData_TTC.RData")