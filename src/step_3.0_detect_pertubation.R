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
load("Data/JRP.DFI.neg.RData")
load("Data/JRP.DFI.pos2.RData")
load("Data/JRP.DFI.pos1.RData")
load("data/JRPData_TTC.Rdata")

# Data frame about information of Spline function of the differences 
# between ITC and CFI and their derivative
spline.dif1 <- data.frame()

# Data frame about information of start and end dates of perturbations and their values
pert.interval <- data.frame()

# Data frame about information of difference between CFI and ITC
dif.CFI <- data.frame()
