# Clear all variables, function, etc in Global Environment
rm(list = ls(all = TRUE))

setwd(getwd())

# Get working directory
wd <- getwd()

library(doBy)
library(gplots)
library(ggplot2)
library(fda)
library(plotly)
library(reshape2)
# Load functions
source("src/funcs.R")
load("Data/JRP.DFI.neg.RData")
load("Data/JRP.DFI.pos2.RData")
load("Data/JRP.DFI.pos1.RData")
load("data/JRPData_TTC.Rdata")

# Data frame about information of Spline function of the differences 
# between ITC and CFI and their derivative
fn.difp1 <- data.frame()

# Data frame about information of start and end dates of perturbations and their values
fn.pertub.table <- data.frame()

# Data frame about information of difference between CFI and ITC
fn.res <- data.frame()


#Order number of Animal_ID
IDs <- unique(as.factor(ITC.Data.1$ANIMAL_ID))

#data frame contains all information of the perturbed days
dev.df <- data.frame()
dev.save <- data.frame()
dev.df <- NULL
dev.save <- NULL

#===============================================================
# For loop to run automatically for all animals
#===============================================================
# Run For loop

# IDC <- as.numeric(5775)
# IDC = c(1: length(ID))
for (i in seq_along(IDs)){

  ID <- IDs[i] # order number of one animal
  print(ID)
  
  #-------------------------------------------------------------------------------
  # Extract data for animal i
  #-------------------------------------------------------------------------------
  
  Data <- ITC.Data.1[ITC.Data.1$ANIMAL_ID == ID,]
  
  #Extract Age and CFI of selected animal 
  Age <- Data$AGE
  CFI <- Data$CFI
  DFI <- Data$FEED_INTAKE
  IDs <- Data$ANIMAL_ID
  
  #-------------------------------------------------------------------------------
  # Calulate TTC using abcd function
  #-------------------------------------------------------------------------------
 
  #Choose the right function for each animal
  
  if(ID %in% unique(ITC.param.neg$ANIMAL_ID)){
    
    param.i <- ITC.param.neg[ITC.param.neg$ANIMAL_ID == ID,]
    FuncType <- param.i[dim(param.i)[1],]$FuncType
    Slope <- param.i[dim(param.i)[1],]$Slope
    param.i <- as.numeric(param.i[dim(param.i)[1], 4:5])
    ITC <- predict.target.trajectory.0(param.i, Age)[[1]]
    ITD <- rep(param.i[2], length(Age))
    
    Data.remain <- Age.remain.neg[Age.remain.neg$ANIMAL_ID == ID,]
    Age.remain <- Data.remain$Age
    DFI.remain <- Data.remain$Observed_DFI
    CFI.remain <- Data.remain$Observed_CFI
    
    
  } else if(ID %in% unique(ITC.param.pos1$ANIMAL_ID)){
    param.i <- ITC.param.pos1[ITC.param.pos1$ANIMAL_ID == ID,]
    FuncType <- param.i[dim(param.i)[1],]$FuncType
    Slope <- param.i[dim(param.i)[1],]$Slope
    param.i <- as.numeric(param.i[dim(param.i)[1], 5:7])
    ITC <- pred.abcd.1(param.i, Age)[[1]]
    ITD <- pred.abcd.1(param.i, Age)[[2]]
    
    Data.remain <- Age.remain.pos1[Age.remain.pos1$ANIMAL_ID == ID,]
    Age.remain <- Data.remain$Age
    DFI.remain <- Data.remain$Observed_DFI
    CFI.remain <- Data.remain$Observed_CFI
    
  } else if(ID %in% unique(ITC.param.pos2$ANIMAL_ID)){
    param.i <- ITC.param.pos2[ITC.param.pos2$ANIMAL_ID == ID,]
    FuncType <- param.i[dim(param.i)[1],]$FuncType
    Slope <- param.i[dim(param.i)[1],]$Slope
    Xs <- param.i[dim(param.i)[1],]$Xs
    param.i <- as.numeric(param.i[dim(param.i)[1], 6:8])
    ITC <- pred.abcd.2(param.i, Age)[[1]]
    ITD <- pred.abcd.2(param.i, Age)[[2]]
    
    Data.remain <- Age.remain.pos2[Age.remain.pos2$ANIMAL_ID == ID,]
    Age.remain <- Data.remain$Age
    DFI.remain <- Data.remain$Observed_DFI
    CFI.remain <- Data.remain$Observed_CFI
  } else {
    next
  }
  
  # Calculate the differences between CFI and its TTC
  res <- CFI - ITC
  #Represent differences in percentage
  percent <- res/ITC*100
  
  #==================================================================
  # Apply SPLINE FUNCTION on the diferences between CFI and TTC (percent)
  #==================================================================
  
  #number of basis functions = order + number of interior knots
  #Roughness penalty 
  lambda <- 1e-2
  pen_what <- 4 # ensure the smoothness of 2nd derivative
  norderT <- 6 # because we need the 4th derivative to be continue
  nby <- 1 #round(length(Age)/3)    # which knots? =1 all points, =2, every 2...
  rangeval <- c(min(Age), max(Age))
  
  #start the process
  # create the time step based on nby
  notknow <- seq(1,length(Age),1)
  
  idx <- c(notknow, length(Age))
  idx <- unique(idx)       # in case the last value is taken twice
  
  # Create B-Spline basis of order orderT with n knots
  #help(create.bspline.basis)
  #nbasis = norder + (number of interior knots) - 2  nbasis = 62,
  basisobj <- create.bspline.basis(rangeval , norder=norderT , breaks=Age[idx])
  fdPar <- fdPar(basisobj, pen_what, lambda)
  
  # Fit Spline to amount of differences between TTC and CFI (percentage)
  difCFI.fd <- smooth.basis(Age, percent, fdPar)
  Smooth.difCFI  <- difCFI.fd$fd
  
  #-------------------------------------------------------------------------------
  # Results of SPLINE
  #-------------------------------------------------------------------------------
  
  #evaluation days for TTC and spline functions : time interval can be changed by = 0.1, 0.01 , ...
  dexima.i <- .1 # interval number
  eval_day <- seq(min(Age), max(Age),by=dexima.i) #days for FI estimation
  
  #Evaluation of B-spline for the difference between TTC and CFI (percentage)
  val.CFI <- eval.fd(eval_day, Smooth.difCFI ) # Spline function
  vel.CFI <- eval.fd(eval_day, Smooth.difCFI, 1) # 1st derivative of Spline function
  acc.CFI <- eval.fd(eval_day, Smooth.difCFI, 2) # 2nd derivative of Spline function 
  
  # Call new names for val.CFI and vel.CFI 
  dif.CFI <- val.CFI # Spline function of difference between CFI and TTC 
  dif.DCFI <- vel.CFI # 1st derivative of Spline function (slope)
  
  #-------------------------------------------------------------------------------
  #Make data frame called "dif" only contains time, values of spline function,
  #of its slope and difference between CFI and its TTC
  #-------------------------------------------------------------------------------
  
  #For the difference
  dif <- as.data.frame(cbind(eval_day, dif.CFI, dif.DCFI))
  names(dif) <- c("eval_day","dif.CFI", "dif.DCFI")
  head(dif)
  
  #==================================================================
  # Detecting DEVIATIONS of CFI from TTC by spline function
  #==================================================================
  
  #-------------------------------------------------------------------------------
  # Detect changes in the sign of 1st derivative
  #-------------------------------------------------------------------------------
  
  # Criteria for a normal range from which everything below is considered as deviations
  crit1 <- 0 #we set this criteria is 0, so whenever actual CFI decreases from TTC we consider as deviations
  
  # Days belong to normal range
  no.pert.age.val <- eval_day[which(dif.CFI >= crit1)]
  # Determine when res is in normal range
  dif$CFIzero <- rep(1, length(eval_day))
  dif$CFIzero[eval_day %in%no.pert.age.val] <- 0
  
  #-------------------------------------------------------------------------------
  # Sign of 1st derivative of Spline function
  #-------------------------------------------------------------------------------
  
  # create a vector for sign of 1st derivative "sign.vel"
  dif$sign.vel <- rep(0, length(eval_day))
  
  # When sign of 1st derivative is possitive (sign.vel = 1)
  dif$sign.vel[which(dif.DCFI >0)] <- 1
  
  # When sign of 1st derivative is negative (sign.vel = -1)
  dif$sign.vel[which(dif.DCFI <0)] <- -1
  
  #-------------------------------------------------------------------------------
  #Calculate the variation in signs of the slope between the day before and the day after
  #EX: if the slope changes from negative to possitive from the day before to the day after
  #To do that: Create one data frames from "dif" in which: the first row is removed
  #And calculate the differences in slopes with "dif"
  #-------------------------------------------------------------------------------
  
  #Duplicate the last row so the new data frame will have equal length with "dif"
  lastrow <- cbind(Age[length(Age)] + dexima.i, dif[dim(dif)[1],][,-1])
  names(lastrow) <- c("eval_day","dif.CFI", "dif.DCFI","CFIzero", "sign.vel" )
  
  #Create the new data frame, remove first row and duplicate the last row
  difp1 <- rbind(dif[-1,], lastrow)
  difp1$ANIMAL_ID <- rep(i, dim(dif)[1])
  
  #-------------------------------------------------------------------------------
  # Determine the days in which the deviation of CFI from TTC are maximum and minimum 
  #-------------------------------------------------------------------------------
  
  difp1$maxmin <- dif$sign.vel - difp1$sign.vel
  #When the 1st derivative changes from negative to positive, sign = -1 - 1 = -2
  #therefore maxmin = -2 is min,
  #
  #When the 1st derivative changes from pos to neg, sign = 1 - (-1) = 2
  #therefore maxmin = +2 is max
  
  #-------------------------------------------------------------------------------
  # Determine the days in which CFI curves start to deviate from and come back to normal range 
  #-------------------------------------------------------------------------------
  
  difp1$end.p <- dif$CFIzero - difp1$CFIzero
  #CFIzero = 0 when CFI is equal TTC, otherwise = 1
  #Value of end.p is equal to 1: come back inside the normal range
  #Value of end.p is equal to -1: go out of the normal range 
  
  #-------------------------------------------------------------------------------
  # Because the begining time is mixing period, pigs encounter many stressors which
  # decrease feed intake. In the begining only a small reduction in FI can lead to 
  # huge difference in %, so we removed the first period
  #-------------------------------------------------------------------------------
  
  #Criteria to remove the first period of mixing group effect on pigs
  crit2 <- 7 #we remove 1st week
  
  #Load a package to manipulate data
  library(dplyr)
  
  #Find the end of the first perturbation
  normal.day <- difp1 %>% filter(end.p ==1); normal.day <- normal.day$eval_day
  #Find the begining of the first perturbation
  pertub.day <- difp1 %>% filter(end.p == -1); pertub.day <- pertub.day$eval_day
  
  #After remove the first growing period (7 days) if the 1st deviation has its magnitude larger than 10%
  #We consider it as a perturbation, thus, we include all data from first period to the data again
  
  if(normal.day[1] - difp1$eval_day[1] < crit2 &
     normal.day[1] < pertub.day[1] &
     pertub.day[1] < difp1$eval_day[1] + crit2){
    
    crit2 <- 0
  } else if(normal.day[1] - difp1$eval_day[1] < crit2 &
            normal.day[1] < pertub.day[1] &
            pertub.day[1] > difp1$eval_day[1] + crit2 ){
    
    crit2 <- 7
  } else if(normal.day[1] - difp1$eval_day[1] < crit2 &
            normal.day[1] > pertub.day[1] &
            pertub.day[2] > difp1$eval_day[1] + crit2){
    
    crit2 <- 7
  } else if(normal.day[1] - difp1$eval_day[1] < crit2 &
            normal.day[1] > pertub.day[1] &
            pertub.day[2] < difp1$eval_day[1] + crit2){
    crit2 <- 0
  } else{ 
    
    normal.day1 <- ifelse(normal.day[1] > difp1$eval_day[1] + crit2, normal.day[1], normal.day[2])
    
    #Test if after removing the first week, magnitude of deviation is still larger than 10%; we keep 1st week, otherwise we remove 1st week
    test.data <- filter(difp1, eval_day %in% seq(difp1$eval_day[1] + crit2, normal.day1, by = 0.1))
    dif.CFI1 <- test.data %>% group_by(dif.CFI) %>% dplyr::summarise(count = n()) %>% arrange(desc(count)) %>% top_n(2)
    as.numeric(dif.CFI1$dif.CFI)
    crit2 <- ifelse(dif.CFI1$dif.CFI[1] <= -10 && (normal.day1 - (difp1$eval_day[1] + crit2)) >= 5, 0, 7)
  }
  
  crit2 #if crit2 = 0, we have to include the first week to perturbation
  
  #Define the days in which actual CFI is deviated from the target CFI
  #deviated ages (which have CFI values < normal range)
  dev.age <- difp1[difp1$dif.CFI < crit1 & difp1$eval_day > eval_day[1] + crit2,]$eval_day
  #data frame contains all information of the perturbed days
  dev.df <-  difp1[difp1$eval_day %in% dev.age, ]
  
  #-------------------------------------------------------------------------------
  # Extract min , max and start and end points of the deviations
  #-------------------------------------------------------------------------------
  
  # perturbed days when res have maximum values
  max <- dev.df$eval_day[which(dev.df$maxmin == -2)  ]
  
  # values of max
  dif.max <- dif$dif.CFI[dif$eval_day%in%max]
  
  # perturbed days when res have maximum values
  min <- dev.df$eval_day[which(dev.df$maxmin == 2)]
  
  # values of min
  dif.min <- dif$dif.CFI[dif$eval_day%in%min]
  
  # start days of deviations
  start.raw <- dev.df$eval_day[which(dev.df$end.p == -1)]
  if(sum(start.raw) == 0){
    start <- dev.df$eval_day[1]
  } else{
    if(start.raw[1] == dev.age[1]){
      start <- start.raw
    } else{
      start <- c(dev.age[1], start.raw)
    }
  }
  # values of start
  dif.start <- dif$dif.CFI[dif$eval_day%in%start]
  
  # end days of deviations
  end <- difp1[difp1$end.p == 1 & difp1$eval_day > eval_day[1] + crit2,]$eval_day
  # values of end
  dif.end <- dif$dif.CFI[dif$eval_day%in%end]
  
  
  #-------------------------------------------------------------------------------
  # Make table for only start and end points of the deviations
  # Reason: calculate the duration of each deviation
  # If the duration of a deviation is larger than 5 days, we consider it as a perturbation
  #-------------------------------------------------------------------------------
  
  #Create an empty table.i which has the length equal to the number of "start" and "end"
  #per = number of deviation
  table.i <- data.frame(per=1:max(length(end),length(dif.end), length(start),length(dif.start)))
  
  #Fill in the vectors of days and values of start and end points to table
  table.i$start <- c(start, rep(NA, nrow(table.i)-length(start)))
  table.i$dif.start <- c(dif.start, rep(NA, nrow(table.i)-length(dif.start)))
  table.i$end <- c(end, rep(NA, nrow(table.i)-length(end)))
  table.i$dif.end <- c(dif.end, rep(NA, nrow(table.i)-length(dif.end)))
  
  #check
  table.i
  
  #-------------------------------------------------------------------------------
  # Modify the table for statrt the end days if there is NA in table
  #-------------------------------------------------------------------------------
  
  #Create a table.dev only contains start and end points (without values)
  dev.table <- cbind()
  
  #If CFI value is lower than TTC  in the first day of dataset,
  #involve the first day of age in the data as the start day of the deviation
  
  for( ii in seq_along(table.i$per)){
    
    #in case pig starts perturbed before scale of dataset and does not recover to normal range; i.e. i = 1
    #
    if(length(table.i$per) == 1 & sum(is.na(table.i$start)) == 0 & sum(is.na(table.i$end)) > 0){
      dev.table <- cbind(table.i$start,Age[length(Age)])
    } else if(start[1] > end[1] & sum(is.na(table.i$start)) == 0) { 
      #
      #set 1st day of age as start day of 1st deviation
      #and last day of age as end day of last deviation
      dev.table <- cbind(c(Age[1], table.i$start), c(table.i$end, Age[length(Age)]))
      
      #in case number of end days is larger than no of start days, eg. i = 8, lambda = 0.01  
    } else if(start[1] > end[1] & sum(is.na(table.i$start)) > 0){ 
      
      #Remove last day of start
      #and add 1st day of age as start day of 1st deviation
      dev.table <- cbind(c(Age[1], table.i$start[-length(table.i$start)]), table.i$end)
      
      #in case last end day is NA because pig does not recover completely  
    } else if (start[1] < end[1] & sum(is.na(table.i$end)) > 0){ # i = 45
      #
      #change last end day by last day of age
      dev.table <- cbind(table.i$start, c(table.i$end[-length(table.i$end)], Age[length(Age)]))
      
      #Normal case
      #
    } else{ 
      dev.table <- cbind(table.i$start, table.i$end) # 
    }
  }
  
  dev.table <- as.data.frame(dev.table)
  colnames(dev.table) <- c("Start", "End")
  dev.table
  
  # Add values of start and end days to table
  value.start <- dif$dif.CFI[dif$eval_day %in% dev.table$Start]
  value.end <- dif$dif.CFI[dif$eval_day %in% dev.table$End]
  dev.table <- cbind(dev.table[1], value.start, dev.table[2], value.end)
  dev.table
  
  #-------------------------------------------------------------------------------
  # Calculate the duration of each deviation
  #-------------------------------------------------------------------------------
  
  dev.table$Devia.days <- dev.table$End - dev.table$Start
  dev.table
  
  #-------------------------------------------------------------------------------
  # Check the duration of each deviation period to be considered as a perturbation 
  #-------------------------------------------------------------------------------
  
  #min number of days for a pert
  crit3 <- 5
  pertub.table <- data.frame()
  for(ii in 1:dim(dev.table)[1]){
    if(dev.table$Devia.days[ii] < crit3){ 
      next
    }
    pertub.table <- rbind(pertub.table, dev.table[ii,])
  }
  #create a table which includes all information of perturbations
  pertub.table
  
  #-------------------------------------------------------------------------------
  # Give a name to each perturbation 
  #-------------------------------------------------------------------------------
  
  Per <- NULL
  Per1<- factor()
  for(ii in 1:dim(pertub.table)[1]){
    Per1 <- paste0("P", ii)
    Per <- c(Per, Per1)
  }
  
  pertub.table <- cbind(as.factor(Per), pertub.table)
  names(pertub.table) <- c("Per", "Start", "value.start", "End", "value.end", "Devia.days")
  
  #Check the magnitude of each deviation
  #if it decreases less than 5% from the TTC, we do not consider it as a perturbation
  A <- data.frame()
  for(ii in 1:dim(pertub.table)[1]){
    A1 <- dif %>%
      filter(eval_day %in% as.numeric(seq(pertub.table$Start[ii], pertub.table$End[ii], by = dexima.i))) %>%
      # select(eval_day, dif.CFI) %>%
      arrange(dif.CFI)
    A1 <- A1[1,1:2]
    A <- rbind(A, A1)
  }
  names(A) <- c("Min.Day", "Min.perc")
  pertub.table <- cbind(pertub.table, A)
  
  #Remove the deviations which have their duration less than 10 days from the table
  pertub.table <- pertub.table %>% filter(Min.perc <= -10)
  
  # #-------------------------------------------------------------------------------
  # # Add it into dif dataframe
  # # Provide each perturbation with a lable
  # #-------------------------------------------------------------------------------
  #
  if(dim(pertub.table)[1] == 0){
    difp1$ppert <- NA
    dev.df$ppert <- NA
  } else{
    for(ii in 1:dim(pertub.table)[1]){
      pert.int <- as.character(seq(pertub.table$Start[ii], pertub.table$End[ii], by=dexima.i))
      
      difp1$ppert[as.character(difp1$eval_day) %in% pert.int] <- paste0("P", ii)
    }
    head(difp1)
    
    # Include label for dataset only contains deviations "dev.df"
    for(ii in 1:dim(pertub.table)[1]){
      pert.int <- as.character(seq(pertub.table$Start[ii], pertub.table$End[ii], by=dexima.i))
      
      dev.df$ppert[as.character(dev.df$eval_day) %in% pert.int] <- paste0("P", ii)
    }}
  head(dev.df)
  
  #Final check the table
  pertub.table
  
  #-------------------------------------------------------------------------------
  #Add pig IDs into dataframe
  #-------------------------------------------------------------------------------
  
  difp1 <- cbind(rep(i, dim(difp1)[1]), difp1)
  names(difp1) <- c("ANIMAL_ID", "eval_day", "dif.CFI", "dif.DCFI",  "CFIzero", "sign.vel", "maxmin", "end.p", "ppert")
  
  pertub.table <- cbind(rep(i, dim(pertub.table)[1]), pertub.table)
  names(pertub.table) <- c("ANIMAL_ID","ppert", "Start", "value.start", "End", "value.end",
                           "days_of_pert", "Min.Day", "Min.perc")
  
  res.data <- as.data.frame(cbind(Age, res, percent))
  res.data <- cbind(rep(i, length(res)), res.data)
  names(res.data) <- c("ANIMAL_ID", "Age", "res", "percent")
  
  #-------------------------------------------------------------------------------
  #Add daily information for all pigs
  #-------------------------------------------------------------------------------
  
  fn.difp1 <- rbind(fn.difp1, difp1)
  fn.pertub.table <- rbind(fn.pertub.table, pertub.table)
  fn.res <- rbind(fn.res, res.data)
  
  #====================================================
  #Plot perturbations
  #====================================================
  
  B <- dev.df[!is.na(dev.df$ppert),] #B contains only data of perturbations
  
  temp <- cbind(Age,res)
  
  # png(filename = paste0("Graphs/Step3_graphs/", idc, ".", ID[idc], ".png"),
  #     height = 600, width = 800, units = 'px', type="cairo-png")
  # par(mar=c(4,4.5,4,4.5))
  # Pertubation_graph <- plot(Age, res,
  #      main = paste( "Pig ID:", i, ", Lambda =", lambda, "\nDifference between CFI and TTC"),
  #      xlab = "Age (days)",
  #      ylab = "Amount of difference: CFI - TTC (kg)",
  #      type = "p", pch = 10, cex = 0.5,
  #      ylim = c(min(B$dif.CFI, res),
  #               max(B$dif.CFI, res)),
  #      cex.main = 1.5, cex.lab = 1.2, axes = F)
  # xaxis(1, at= seq(dif$eval_day[1],dif$eval_day[length(dif$eval_day)], by = 5), cex.axis = 1.1)
  # xaxis(2, at=, cex.axis = 1.1)
  # xaxis(4, at=, cex.axis = 1.1, col.axis = "blue", col = "blue")
  # mtext("Percentage of difference: CFI - TTC (%)", side=4, line = 3, cex = 1.2, col= "blue")
  # box()
  # abline(crit1,0, col = "red", lty = 2)
  # abline(0,0, col = "red")
  # if(crit2 == 7){
  # abline(v = (Age[1]+crit2), lty =2)
  # } else{}
  # points(B$eval_day, B$dif.CFI, type = "l", col = "blue", lwd = 2)
  # points(pertub.table$Start, pertub.table$value.start,
  #        col = "orange", pch=15, cex = 2)
  # points(pertub.table$End, pertub.table$value.end,
  #        col = "green", pch=17, cex = 2)
  # points(pertub.table$Min.Day, pertub.table$Min.perc,
  #        col = "purple", pch=19, cex = 2)
  # dev.off()
  
  # Pertubation_graph <- plot_ly(data.frame(temp), x = ~Age, y = ~res) %>%
  #     layout(title = paste( "Pig ID:", i, ", Lambda =", lambda, "\nDifference between CFI and TTC", plot_bgcolor = "fffff" ),
  #                     xaxis = list(title = 'Age (d)',cex.main = 1.5, cex.lab = 1.2, axes = F),
  #                     yaxis = list(title = 'Amount of difference: CFI - TTC (kg)',type = "p", pch = 10, cex = 0.5),
  #                     ylim = c(min(B$dif.CFI, res), max(B$dif.CFI, res), cex.main = 1.5, cex.lab = 1.2, axes = F),
  #                     yaxis2 = list("Percentage of difference: CFI - TTC (%)", side=4, line = 3, cex = 1.2, col= "blue"),
  #                     xaxis = list(1, at= seq(dif$eval_day[1],dif$eval_day[length(dif$eval_day)], by = 5), cex.axis = 1.1),
  #                     xaxis = list(2,  cex.axis = 1.1),
  #                     xaxis = list(4,  cex.axis = 1.1, col.axis = "blue", col = "blue"))
  # if(crit2 == 7){
  # abline(v = (Age[1]+crit2), lty =2)
  # } else{}
  # # highlight(B$eval_day, B$dif.CFI)
  # # highlight(pertub.table$Start, pertub.table$value.start,
  # #        col = "orange", pch=15, cex = 2)
  # # highlight(pertub.table$End, pertub.table$value.end,
  # #        col = "green", pch=17, cex = 2)
  # # highlight(pertub.table$Min.Day, pertub.table$Min.perc,
  # #        col = "purple", pch=19, cex = 2)
  #
  # # # layout(xaxis = list(1, at= seq(dif$eval_day[1],dif$eval_day[length(dif$eval_day)], by = 5), cex.axis = 1.1),
  # # #               # xaxis = list(2, at=, cex.axis = 1.1),
  # # #               # xaxis = list(4, at=, cex.axis = 1.1, col.axis = "blue", col = "blue"),
  # # #               abline(crit1,0, col = "red", lty = 2) ,
  # # #               abline(0,0, col = "red"))
  #
  #
  # saveWidget(Pertubation_graph, file=paste0("C:/Users/Kevin Le/PycharmProjects/Pig Data Black Box/Graphs/Step3_graphs/hi/", idc, ".", ID[idc], ".html"))
  # Copy pertubation data for furture calculation
  if (!is.null(dev.save)){
    dev.save <- rbind(dev.save, dev.df)
  } else{
    dev.save <-  dev.df
  }
}

dev.df <- dev.save
#-------------------------------------------------------------------------------
#Save results to Rdata file
#-------------------------------------------------------------------------------

save(fn.difp1,
     fn.pertub.table,
     fn.res,
     dev.df,
     file = "Data/JRP.Per.detec.RData")

write.csv2(fn.pertub.table, file = "fn.pertub.table.csv")
