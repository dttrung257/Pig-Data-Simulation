# Clear all variables, function, etc in Global Environment
rm(list = ls(all = TRUE))

setwd(getwd())
# Load data
load("data/JRPData.Rdata")

# Load functions
source("src/funcs.R")

# Get working directory
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
JRP.estimate <- JRP_NA
estimate.point <- data.frame()

for (i in 1:length(lol.miss.ages)) {
  id <- names(lol.miss.ages)[i]
  # missing row of pig
  pig.miss.ages <- lol.miss.ages[[i]]
  pig.data <- JRP.estimate[JRP.estimate$ANIMAL_ID == id, ]
  pig.age <- pig.data$AGE
  for (k in 1:length(pig.miss.ages)) {
    #  ages before missing using for training model
    # if(length(pig.miss.ages[[k]]) < pig.age[1]+2) {
    #     nexts
    # }
    
    sub.miss.age <- pig.miss.ages[[k]]
    initial <- seq(pig.age[1],sub.miss.age[1]-1,1)
    if (length(initial) < 3) {
      next
    }
    # take 3 value for training
    estimate.initial <- tail(initial, 3)
    # data for training
    # if(length(estimate.initial) < 3) {
    #   next
    # }
    before.miss.age <- filter(pig.data, AGE %in% estimate.initial)
    
    # if the length of data use for estimate  < 3 cannot apply estimate model
    if(length(before.miss.age) < 3) {
      next
    }
    
    DFI.before <- tail(before.miss.age$FEED_INTAKE, 3)
    # if the data decreasing not estimate since we do not know when pertubation end.
    if(all(DFI.before == sort(DFI.before,decreasing = T))) {
      next
    }
    
    data.missing <- data.frame(ANIMAL_ID=id, AGE=sub.miss.age)
    
    # training data model base on the previous missing
    pig.train.model <- lm(CFI ~ poly(AGE, 2, raw = TRUE), data = before.miss.age)
    # Get the coefficient of the linear term (1st-degree term)
    coefficient_of_linear <- coef(pig.train.model)["poly(AGE, 2, raw = TRUE)2"]
    if (coefficient_of_linear <= 0) {
      next
    }
    # predict CFI of missing day
    CFI.prediction <- predict(pig.train.model, data.missing)
    DFI.prediction <- vector()
    if (length(CFI.prediction) == 1) {
      DFI.prediction <- CFI.prediction - before.miss.age[nrow(before.miss.age), ]$CFI
    } else {
      DFI.prediction <- CFI.prediction - c(before.miss.age[nrow(before.miss.age), ]$CFI,
                                           CFI.prediction[1:(length(CFI.prediction) - 1)])
    }
  
    pig.estimate <- data.frame(ANIMAL_ID=id, AGE=sub.miss.age, FEED_INTAKE=DFI.prediction,
                                   WEIGHT_NA=NA, WEIGHT=NA, CFI=CFI.prediction)
    
    #pig.estimate <- data.frame(ANIMAL_ID=id,AGE=sub.miss.age,FEED_INTAKE=NA,WEIGHT_NA=NA,WEIGHT=NA,CFI=CFI.prediction)
    print(before.miss.age)
    print(pig.estimate)
    # 90 91  miss( 92 93 94) 95
    missing.dist.CFI = max(CFI.prediction) - max(before.miss.age$CFI)
    print(missing.dist.CFI)
    last.day.miss <- tail(pig.miss.ages[[k]])
    JRP.estimate$CFI <- ifelse(JRP.estimate$AGE > sub.miss.age[length
                                                              (sub.miss.age)] & JRP.estimate$ANIMAL_ID == id,JRP.estimate$CFI+missing.dist.CFI,JRP.estimate$CFI)
    JRP.estimate <- rbind(JRP.estimate,pig.estimate)
    JRP.estimate <- JRP.estimate %>% arrange(ANIMAL_ID,AGE)
    
    estimate.point <- rbind(estimate.point, 
                            JRP.estimate %>% 
                              filter(ANIMAL_ID == id, AGE %in% sub.miss.age) %>% 
                              select(ANIMAL_ID, AGE))
  }
}

output.dir <- paste0(wd, '/graphs/Observation/estimate/') # Folder get output images or html files from graph

clear_data(paste0(output.dir, 'DFI_PNG/')) # clear old images in DFI_PNG folder
check_path(paste0(output.dir, 'DFI_PNG/')) # If the directory does not exist, create a new directory
clear_data(paste0(output.dir, 'CFI_PNG/')) # clear old images in CFI_PNG folder
check_path(paste0(output.dir, 'CFI_PNG/')) # If the directory does not exist, create a new directory

# for(i in 1:length(unique.ids)) {
#   id <- unique.ids[i]
#   if (id %in% estimate.point$ANIMAL_ID | id == 5525) {
#     data <- JRP.estimate[JRP.estimate$ANIMAL_ID == id, ]
#     temp_df <- data.frame(ANIMAL_ID = id, AGE = data$AGE)
#     merged_df <- merge(temp_df, estimate.point, by = c("ANIMAL_ID", "AGE"))
#     data$type <- ifelse(data$ANIMAL_ID %in% merged_df$ANIMAL_ID & data$AGE %in% merged_df$AGE, 'Estimation', 'Observation')
#     DFI.fig <- ggplot(data, aes(x = AGE, y = FEED_INTAKE, color = type)) +
#       geom_point() +
#       labs(title = paste0('Daily Feed Intake\n', 'PigID:', id), x = 'Age (days)', y = 'Daily Feed Intake, kg') +
#       scale_color_manual(values = c('Observation' = 'grey', 'Estimation' = 'blue'))
# 
#     ggsave(filename = paste0(output.dir, 'DFI_PNG/', id, ".", "DFI", ".png"), plot = DFI.fig)
# 
#     CFI.fig <- ggplot(data, aes(x = AGE, y = CFI, color = type)) +
#       geom_point() +
#       labs(title = paste0('Cumulative Feed Intake\n', 'PigID:', id), x = 'Age (d)', y = 'Cumulative Feed Intake, kg') +
#       scale_color_manual(values = c('Observation' = 'grey', 'Estimation' = 'blue'))
# 
#     ggsave(filename = paste0(output.dir, 'CFI_PNG/', id, ".", "CFI", ".png"), plot = CFI.fig)
#   }
# }

save(JRP_NA, # Step 0
     lol.miss.ages, lov.miss.ages, JRP.estimate, # Step 1
     file = "data/JRPData.Rdata")