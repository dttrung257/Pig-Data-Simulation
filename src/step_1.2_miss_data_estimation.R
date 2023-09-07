rm(list=ls(all=TRUE)) # Clear all variables, function, etc in Global Environment
setwd(getwd())
load("data/JRPData.Rdata")
wd <- getwd()

library(ggplot2)
library(fs)
library(dplyr)

# List unique id of Animal
unique.ids <- unique(JRP_NA$ANIMAL_ID)
# Animal id
# ids <- JRP_NA$ANIMAL_ID
# Age (days)
ages <- JRP_NA$AGE
# Daily Feed Intake
DFIs <- JRP_NA$FEED_INTAKE
# Cumulative Feed Intake
CFIs <- JRP_NA$CFI
JRP_estimate <- JRP_NA



for (i in 1:length(lol.miss.ages)) {
  id <- names(lol.miss.ages)[i]
  # missing row of pig
  pig.miss.ages <- lol.miss.ages[[i]]
  pig.data <- JRP_estimate[JRP_estimate$ANIMAL_ID == id,]
  pig.age <- pig.data$AGE
  for (k in 1:length(pig.miss.ages)) {
    #  ages before missing using for training model
    # if(length(pig.miss.ages[[k]]) < pig.age[1]+2) {
    #     nexts
    # }
    
    sub.miss.age <- pig.miss.ages[[k]]
    # if(length(sub.miss.age)!=3) {
    #   next
    # }
    # else {
    #   print(id)
    # }
    estimate.initial <- seq(pig.age[1],sub.miss.age[1]-1,1)
    # take 3 value for training
    estimate.initial <- tail(estimate.initial,3)
    # data for training
    # if(length(estimate.initial) < 3) {
    #   next
    # }
    before.miss.age <- filter(pig.data,AGE %in% estimate.initial)

    print(sub.miss.age)
    # if the length of data use for estimate  < 3 cannot apply estimate model
    if(length(before.miss.age) < 3) next
    DFI.before <- tail(before.miss.age$FEED_INTAKE,4)
    # if the data decreasing not estimate since we do not know when pertubation end.
    if(all(DFI.before == sort(DFI.before,decreasing = T))) next
    

    data.missing <- data.frame(ANIMAL_ID=id,AGE=sub.miss.age)
    # training data model base on the previous missing 
    pig.train.model <- lm(CFI ~poly(AGE,2,raw=T),data=before.miss.age)
    # predict CFI of missing day
    CFI.prediction <- predict(pig.train.model,data.missing)
    
    
    pig.estimate <- data.frame(ANIMAL_ID=id,AGE=sub.miss.age,FEED_INTAKE=NA,WEIGHT_NA=NA,WEIGHT=NA,CFI=CFI.prediction)
    print(before.miss.age)
    print(pig.estimate)
    # 90 91  miss( 92 93 94) 95
    missing.dist.CFI = max(CFI.prediction) - max(before.miss.age$CFI)
    print(missing.dist.CFI)
    last.day.miss <- tail(pig.miss.ages[[k]])
    JRP_estimate$CFI <- ifelse(JRP_estimate$AGE >sub.miss.age[length
                                                              (sub.miss.age)] & JRP_estimate$ANIMAL_ID == id,JRP_estimate$CFI+missing.dist.CFI,JRP_estimate$CFI)
    JRP_estimate <- rbind(JRP_estimate,pig.estimate)
    JRP_estimate <- JRP_estimate %>% arrange(ANIMAL_ID,AGE)
  }
}
