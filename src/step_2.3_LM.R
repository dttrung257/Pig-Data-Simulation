# Clear all variables, function, etc in Global Environment
rm(list = ls(all = TRUE))

setwd(getwd())
# Load data
# load("data/JRPData.Rdata")
load("Data/JRP.DFI.pos2.RData")
load("Data/JRP.DFI.pos1.RData")
load("data/JRPData_TTC.Rdata")

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

#Dataframe contains ITC parameters
ITC.param.neg <- data.frame(ANIMAL_ID=factor(),
                            X0=double(),
                            Y2=double(),
                            Ylast=double(),
                            a=double(),
                            b=double(),
                            c=double(),
                            stringsAsFactors=FALSE)

#Dataframe contains data points on the ITC
Data.remain <- data.frame(ANIMAL_ID=character(),
                          Age=double(),
                          obs.CFI=double(),
                          tt=double(),
                          ttt=double(),
                          stringsAsFactors=FALSE)

for (i in 1:length(CFI.LM)) {
  id <- CFI.LM[i]
  Data <- ITC.Data.1[ITC.Data.1$ANIMAL_ID == id, ]
  print(paste0("id: ", id))
  # print(as.numeric(Data$id.1))
  idc1 <- unique(as.numeric(Data$id.1))
  # print(idc1)
  
  # Create data frame of x (Age) and y (CFI)
  x <- as.numeric(Data$AGE)
  Y <- as.numeric(Data$CFI)
  Z <- as.numeric(Data$FEED_INTAKE)
  Data.xy <- as.data.frame(cbind(x,Y))
  # print(x)
  # print(Y)
  # print(Data.xy)
  
  # Initial parameteres for parameter estimation
  x0.0 <- x[1]
  xlast <- x[length(x)]
  ylast.0 <- Y[length(x)]
  # Vector contains 4 initial parameters 
  par_init <- c(x0.0, ylast.0)
  
  #--------------------------------------------
  # Create empty lists to store data after loop
  #--------------------------------------------
  par <- list()
  AC.res <- list()
  AC.pvalue <- NULL
  data2 <- list()
  param <- data.frame(rbind(par_init))
  par.abcd <- data.frame(rbind(target.coef.0(as.vector(par_init))))
  param.2 <- data.frame(X0=double(),
                        Y2=double(),
                        Ylast=double(),
                        a=double(),
                        b=double(),
                        c=double(),
                        stringsAsFactors=FALSE)
  j <- 2
  AC_pvalue <- 0
  AC.pvalue[1] <- AC_pvalue
  datapointsleft <- as.numeric(dim(Data)[1])
  dpl <- datapointsleft #vector of all dataponitsleft at each step
  print(par.abcd)
  #-----------------------------------------------------------------------------
  # Start the procedure of Non Linear Regression
  #-------------------------------------------------------------------------------
  while (AC_pvalue<=0.05 && datapointsleft >= 20){
    weight <- 1/Y^2
    #---------------- NON linear reg applied to log(Y) ---------------------------------
    
    nls.CFI <- nlsLM(Y ~ nls.func.0(X0, ylast),
                     Data.xy, 
                     control = list(tol = 1e-2, printEval = TRUE, maxiter = 50),
                     start = list(X0 = par_init[1],
                                  ylast = par_init[2]), 
                     trace = F,
                     weights = weight)
    #--------RESULTS analysis GOODNESS of fit
    # Estimate params
    par[[j]] <- coef(nls.CFI)
    par.abcd[j, ] <- target.coef.0(as.vector(coef(nls.CFI))) # calculation of a, b, c and d
    param[j, ] <- par[[j]]
    param.2[j-1, ] <- cbind(param[j, ], par.abcd[j, ])
    # summary
    # summ = overview((nls.CFI))
    #residuals
    res1 <- nlsResiduals(nls.CFI) #residuals
    res2 <- nlsResiduals(nls.CFI)$resi1
    res <- res2[, 2]
    AC.res <- test.nlsResiduals(res1)
    AC.pvalue[j] <- AC.res$p.value
    
    #---------Check for negative residuals----------
    
    # Add filtration step order to data
    Step <- rep(j - 1, length(x)) 
    # Create a new dataset with predicted CFI included
    Data.new <- data.frame(cbind(x, Z, Y, predict.target.trajectory.0(par[[j]],x)[[1]], res, Step))
    names(Data.new) <- c("Age", "Observed_DFI","Observed_CFI", "Predicted_CFI", "Residual", "Step")
    # plot(Data.new$Age, Data.new$Predicted_CFI, type = "l", col = "black",lwd = 2,
    #      ylim = c(0, max(Data.new$Predicted_CFI, Data.new$Observed_CFI)))
    # lines(Data.new$Age, Data.new$Observed_CFI, type = "p", cex = 1.5)
    #
    #remove negative res
    Data.pos <- Data.new[!Data.new$Residual < 0, ]
    # lines(Data.pos$Age, Data.pos$Predicted_CFI, type = "l", col = j-1, lwd = 2)
    # lines(Data.pos$Age, Data.pos$Observed_CFI, type = "p", col = j, cex = 1.5)
    
    #restart 
    datapointsleft <- as.numeric(dim(Data.pos)[1])
    par_init <- par[[j]]
    AC_pvalue <- AC.pvalue[j]
    j <- j+1
    x <- Data.pos$Age
    Y <- Data.pos$Observed_CFI
    Z <- Data.pos$Observed_DFI
    Data.xy <- as.data.frame(cbind(x,Y))
    dpl <- c(dpl, datapointsleft);
  }
  ANIMAL_ID <- rep(id, dim(param.2)[1])
  XLAST <- rep(xlast, dim(param.2)[1])
  FuncType <- "LM"
  param.2 <- cbind(ANIMAL_ID,
                   param.2,
                   XLAST,
                   AC.pvalue[2:length(AC.pvalue)],
                   dpl[seq_along(dpl) -1],
                   rep(FuncType, dim(param.2)[1]),
                   rep(idc1, dim(param.2)[1]))
  colnames(param.2) <- c("ANIMAL_ID",
                         "X0",
                         "Y2",
                         "Ylast",
                         "a",
                         "b",
                         "c",
                         "Xlast",
                         "P.runs.trst",
                         "DPL",
                         "FuncType",
                         "idc1")
  #Add one column about the slope of DFI to the function
  Slope <- rep(0, dim(param.2)[1])
  
  param.2$Slope <- Slope
  
  #-------------------------------------------------------------------------------
  # Give Animal ID for each animal and save them to dataframes in the loop
  #-------------------------------------------------------------------------------    
  ANIMAL_ID <- rep(id, dim(Data.new)[1])
  idc1 <- rep(idc1, dim(Data.new)[1])
  Data.new <- cbind(ANIMAL_ID , Data.new, idc1)
  
  Data.remain <- rbind(Data.remain, Data.new)
  Age.remain.neg <- Data.remain[, c(1:4, 8)]
  ITC.param.neg <- rbind(ITC.param.neg , param.2 )
} # END FOR LOOP

save(ITC.param.neg, Age.remain.neg,         # Step 2: ITC
     file = "Data/JRP.DFI.neg.RData")
