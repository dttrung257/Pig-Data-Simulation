rm(list=ls(all=TRUE)) # Clear all variables, function, etc in Global Environment
setwd(getwd())
wd <- getwd()

library(ggplot2)
library(fs)
library(dplyr)
library("minpack.lm")
library("nlstools")
library(nls2)
source("src/funcs.R")

#load dataset
load("data/JRPData_TTC.Rdata") #load dataset created in MissingData.step
data.remain <- data.frame(ANIMAL_ID=character(),
                          Age=double(),
                          obs.CFI=double(),
                          tt=double(),
                          ttt=double())
ITC.param.pos2 <- data.frame(ANIMAL_ID=factor(),
                             X0=double(),
                             Y1=double(),
                             Y2=double(),
                             Ylast=double(),
                             a=double(),
                             b=double(),
                             c=double(),
                             d=double())
unique.ids <- unique(CFI.QLM)
unique.ids <- unique.ids[!unique.ids %in% c(5320,5470)]
for (i in seq_along(unique.ids)) {
  
  id <- unique.ids[i]
  pig.data <- ITC.Data.1[ITC.Data.1$ANIMAL_ID==id,]
  id.1 <- unique(as.numeric(pig.data$id.1))
  # CFI có dạng Y = hàm(x) 
  x <- as.numeric(pig.data$AGE)
  Y <- as.numeric(pig.data$CFI)
  dfi <- as.numeric(pig.data$FEED_INTAKE)
  data.xy <- as.data.frame(cbind(x, Y))
  
  ### initial parameters for estimate parameters
  firstday <- x[1]
  lastday <- x[length(x)]
  
  ### provide set of initial parameter
  Xs.1 <- round(seq(firstday + 1, lastday - 1, len = 30), digits = 0)
  X0.1 <- rep(firstday, length(Xs.1))
  DFI.param <- NULL
  CFI.param <- NULL
  for (day in seq_along(Xs.1)) {
    DFI <- pig.data[pig.data$AGE == Xs.1[day],]$FEED_INTAKE
    CFI <- pig.data[pig.data$AGE == Xs.1[day],]$CFI
    DFI.param <- c(DFI.param,DFI)
    CFI.param <- c(CFI.param,CFI)
  }
  st1 <- data.frame(X0.1,Xs.1,DFI.param,CFI.param)
  names(st1) <- c("X0","Xs", "DFIs","CFIs")
  # fit model to data
  # find the most fit model for 1 set (X0,Xs,DFIs,CFIs) among 30 day as Xs.1 for observed data
  st2 <- nls2(Y ~ nls.func.2(X0, Xs, DFIs, CFIs),
              data.xy,
              start = st1,
              # weights = weight,
              # trace = T,
              algorithm = "brute-force")
  ############## continue tomorrow
  #set of (X0,Xs,DFIs,CFIs) that lead to coef c,b,a fit most to observed data 
  par_init <- coef(st2)
  # store data for non linear regression
  AC.res <-list()
  #list of set (X0,Xs,DFIs,CFIs) after each turn fit model by nlsLM
  par.list <- list()
  # data frame consist coef of quadratic function (c,b,a) 
  par.coef <- data.frame(rbind(target.coef.2(as.vector(par_init))))
  
  #pertubation value 
  p.value <- 0
  
  par.fit <- data.frame(rbind(par_init))
  
  # data point of pig left after each loop 
  datapoint.left <- as.numeric(dim(pig.data)[1])
  dpl <- datapoint.left
  
  pvalues <- NULL
  pvalues[1] <- p.value
  j<-2
  param.aggregate <- data.frame(X0=double(),
                                Xs=double(),
                                DFIs=double(),
                                CFIs=double(),
                                a=double(),
                                b=double(),
                                c=double())
  #if pertubation value of CFI decrease percent <= 0.05 then still continue loop
  # else there is a pertubation then stop delete data
  # if data point left less than 20 than stop deleting data
  print("enter while loop")
  while(p.value <=0.05 && datapoint.left >=20) {
    Xs.compare <- Xs.1
    weight <- 1/Y^2
    st2 <- nls2(Y ~ nls.func.2(X0, Xs, DFIs, CFIs),
                data.xy,
                start = st1,
                weights = weight,
                trace = F,
                algorithm = "brute-force")
    par_init <- coef(st2)
    
    nls.CFI <- nlsLM(Y ~ nls.func.2(X0, Xs, DFIs, CFIs),
                     data.xy,
                     control = list(tol = 1e-2, printEval = TRUE, maxiter = 1024),
                     start = list(X0 = par_init[1], Xs = par_init[2],
                                  DFIs = par_init[3], CFIs = par_init[4]),
                     weights = weight,
                     algorithm = "port",
                     lower = c(-10000,firstday+1, -10000, -10000),
                     upper = c(10000, lastday-1, 10000, 10000),
                     trace = F)
    res1 <- nlsResiduals(nls.CFI)
    res2 <- res1$resi1
    res <- res2[,2]
    AC.res <- test.nlsResiduals(res1)
    pvalues[j] <- AC.res$p.value
    
    par.list[[j]] <- coef(nls.CFI)
    par.coef[j,] <- target.coef.2(as.vector(coef(nls.CFI) )) #calculation of a, b, c and d
    par.fit[j,] <- par.list[[j]]
    
    param.aggregate[j-1,] <- cbind(par.fit[j,], par.coef[j,])
    
    #---------Check for negative residuals----------
    
    #Add filtration step order to data
    Step <- rep(j - 1, length(x))
    print(par.list[[j]])
    #create a new dataset with predicted CFI included
    pig.data.new <- data.frame(cbind(x, dfi, Y, predict.target.trajectory.2(par.list[[j]], x)[[1]], res, Step))
    names(pig.data.new) <- c("Age", "Observed_DFI","Observed_CFI", "Predicted_CFI", "Residual", "Step")
    check <- 0
    # data point with residual positive
    data.pos <- pig.data.new[!pig.data.new$Residual<0,]
    #renew necessary data for next loop
    datapoint.left <- as.numeric(dim(data.pos)[1])
    dpl <- c(dpl,datapoint.left)
    
    par_init <- par.list[[j]]
    p.value <- pvalues[j]
    j <- j+1
    
    x <- data.pos$Age
    Y<- data.pos$Observed_CFI
    dfi <- data.pos$Observed_DFI
    data.xy <- as.data.frame(cbind(x,Y))
    firstday <- x[1]
    lastday <- x[length(x)]
    
    #renew Xs.1
    if(par_init[2] -15 <= firstday){
      
      Xs.1 <- round(seq(firstday + 5, lastday - 5, len = 30), digits = 0)
    } else if(par_init[2] + 5 >= lastday){
      Xs.1 <- round(seq(par_init[2]-10, par_init[2]-1, len = 6), digits = 0)
    } else{
      Xs.1 <- round(seq(par_init[2]-5, par_init[2] + 5, len = 6), digits = 0)
    }
    left.old <- dpl[length(dpl)-1]
    p.last <- pvalues[length(pvalues)-1]
    if(all(Xs.1 == Xs.compare & datapoint.left == left.old & p.value == p.last)) {
      break
    }
    X0.1 <- rep(firstday, length(Xs.1))
    DFIs.1 <- NULL
    CFIs.1 <- NULL
    for(A in seq_along(Xs.1)){
      DFIs2 <- pig.data[pig.data$AGE == Xs.1[A],]$FEED_INTAKE
      CFIs2 <- pig.data[pig.data$AGE == Xs.1[A],]$CFI
      DFIs.1 <- c(DFIs.1, DFIs2)
      CFIs.1 <- c(CFIs.1, CFIs2)
    }
    st1 <- data.frame(cbind(X0.1, Xs.1, DFIs.1, CFIs.1))
    # for (s in seq_along(st1)) {
    #   if(st1 %in% data)
    # }
    if(firstday <= par_init[2] && lastday >=par_init[2]){
      par_init[1] <- round(par_init[1])
      par_init[2] <- round(par_init[2])
      st1 <- rbind(st1, par_init)
    }
    names(st1) <- c("X0","Xs", "DFIs","CFIs")
    
  }
  ANIMAL_ID <- rep(id, dim(param.aggregate)[1])
  FuncType <- unique(pig.data$FuncType)
  print("it's find till here")
  param.aggregate <- cbind(ANIMAL_ID,
                           param.aggregate,
                           XLAST = rep( pig.data$AGE[length(pig.data$AGE)], dim(param.aggregate)[1]),
                           pvalues[2:length(pvalues)],
                           dpl[seq_along(dpl) -1],
                           rep(FuncType, dim(param.aggregate)[1]),
                           rep(id.1, dim(param.aggregate)[1])
  )
  colnames(param.aggregate) <- c("ANIMAL_ID",
                                 "X0",
                                 "Xs",
                                 "DFIs",
                                 "CFIs",
                                 "a",
                                 "b",
                                 "c",
                                 "Xlast",
                                 "P.runs.trst",
                                 "DPL",
                                 "FuncType",
                                 "id.1")
  Slope <- NULL
  for(ii in 1:dim(param.aggregate)[1]){
    Slope.1 <- -1
    if(param.aggregate[ii,]$c < 0){
      Slope.1 <- -1
    } else {
      Slope.1 <- 1
    }
    
    Slope <- c(Slope, Slope.1)
  }
  print("while loop is good")
  param.aggregate$Slope <- Slope
  ANIMAL_ID <- rep(id, dim(pig.data.new)[1])
  id.1 <- rep(id.1, dim(pig.data.new)[1])
  pig.data.new <- cbind(ANIMAL_ID , pig.data.new, id.1)
  
  data.remain <- rbind(data.remain, pig.data.new)
  
  Age.remain.pos2 <- data.remain[, c(1:4, 8)]
  
  ITC.param.pos2 <- rbind(ITC.param.pos2 , param.aggregate)
  
}
#==============================================================================
# Regrouping the animals have negative slope in Q-L function to Linear function
#==============================================================================
#Select animals have negative slope for DFI in Q-L Function for CFI
ID <- unique(ITC.param.pos2$ANIMAL_ID)
#trạng thái cuối cùng của model sau khi lọc dữ liệu
ITC.param.p2 <- data.frame()

for(i in seq_along(ID)){
  id <- ID[i]
  param.p2.neg <- ITC.param.pos2[ITC.param.pos2$ANIMAL_ID == id,]
  param.p2.neg <- param.p2.neg[dim(param.p2.neg)[1],]
  
  
  ITC.param.p2 <- rbind(ITC.param.p2, param.p2.neg)
}
ITC.param.p2.neg <- subset(ITC.param.p2, Slope == -1)
# length(unique(ITC.param.p2.neg$ANIMAL_ID))
# 
#Remove animals have negative slope (= -1) of DFI out of dataset for Function of Linear-Plateau
ITC.param.pos2 <- subset(ITC.param.pos2, !(ANIMAL_ID %in% ITC.param.p2.neg$ANIMAL_ID))
Age.remain.pos2 <- subset(Age.remain.pos2, !(ANIMAL_ID %in% ITC.param.p2.neg$ANIMAL_ID))
#Add these animals to the group of linear function for CFI
DFI.neg1 <- c( CFI.LM, unique(ITC.param.p2.neg$ANIMAL_ID))
CFI.LM <- unique(DFI.neg1)
length(CFI.LM)

#==============================================================================
# Regrouping the animals have Xs too closed to Xlast in Q-L function to QDR function
#==============================================================================

#Select animals have XS too closed to Xlast Function type 3
ID <- unique(ITC.param.pos2$ANIMAL_ID)
# trạng thái cuối cùng của model sau khi lọc hết dữ liệu loại bỏ DFI giảm dần
ITC.param.p2 <- data.frame()

for(AA in seq_along(ID)){
  id <- ID[AA]
  param.p2.neg <- ITC.param.pos2[ITC.param.pos2$ANIMAL_ID == id,]
  param.p2.neg <- param.p2.neg[dim(param.p2.neg)[1],]
  
  
  ITC.param.p2 <- rbind(ITC.param.p2, param.p2.neg)
}
#Xs too close to Xlast
ITC.param.p2.FT2 <- subset(ITC.param.p2, Xs >= ((Xlast - X0)*0.9 + X0))
length(unique(ITC.param.p2.FT2$ANIMAL_ID))
#lọc dữ liệu có điểm Xs gần gốc khỏi tệp dữ liệu trạng thái các lần lọc
ITC.param.pos2 <- subset(ITC.param.pos2, !(ANIMAL_ID %in% ITC.param.p2.FT2$ANIMAL_ID))
#lọc dữ liệu gốc ban đầu sau sau khi chỉ còn phần dư không âm
Age.remain.pos2 <- subset(Age.remain.pos2, !(ANIMAL_ID %in% ITC.param.p2.FT2$ANIMAL_ID))

#Add these animals to the group of QDR function for CFI
DFI.pos11 <- c( CFI.QDR, unique(ITC.param.p2.FT2$ANIMAL_ID))
CFI.QDR <- unique(CFI.QDR)
#==============================================================================
# Regrouping the animals have error in Q-L function to QDR function
#==============================================================================

#The error pigs in Quadratic-linear function is identified
Last.pigs <- subset(ITC.Data.1, !(ANIMAL_ID %in% CFI.LM))
Last.pigs <- subset(Last.pigs, !(ANIMAL_ID %in% CFI.QDR))
Last.pigs <- subset(Last.pigs, !(ANIMAL_ID %in% ITC.param.pos2$ANIMAL_ID))

Last.pigs <- unique(Last.pigs$ANIMAL_ID)

#Add these animals to the group of QDR function for CFI
DFI.pos11 <- c( CFI.QDR, Last.pigs)
CFI.QDR <- unique(DFI.pos11)

#Reset the animals in group of Quadratic-linear function for CFI
CFI.QLM <- unique(Age.remain.pos2$ANIMAL_ID)

#===============================================================
#Save results to Rdata file
#===============================================================

save(ITC.param.pos2, Age.remain.pos2,
     file = "Data/JRP.DFI.pos2.RData")

save(ITC.Data.1, CFI.LM, CFI.QDR, CFI.QLM,
     file = "Data/JRPData_TTC.RData")