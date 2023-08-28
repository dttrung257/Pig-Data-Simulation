rm(list=ls(all=TRUE)) # Clear all variables, function, etc in Global Environment
setwd(getwd())
load("data/JRPData.Rdata")
wd <- getwd()

library(ggplot2)
library(fs)
library(dplyr)

# Dataframe in step 1.1
JRP.step.1.1 <- data.frame()
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


# length(unique.ids)
for (i in 1:length(unique.ids)) {
  # id of pig
  id <- unique.ids[i]
  
  # data of a pig id = i 
  data <- JRP_NA[JRP_NA$ANIMAL_ID == id, ]
  
  # age (days) of pig id = i
  pig.ages <- data$AGE
  
  # DFI of pig id = i
  pig.DFIs <- data$FEED_INTAKE
  
  # CFI of pig id = i
  pig.CFIs <- data$CFI
  
  # Miss ages of pig id = i
  pig.miss.ages <- list()
  
  # Expected row (In case, don't miss data)
  expected.row <- max(pig.ages) - min(pig.ages) + 1
   
  # Observed row (In case, miss data)
  observed.row <- as.numeric(length(pig.ages))
  
  # Age difference between 2 consecutive data lines of pig id i
  diff <- pig.ages[2:length(pig.ages)] - pig.ages[1:(length(pig.ages)-1)]
  
  # Days before missing data
  before.miss.rows <- data[diff > 1, ] 
  
  # While the age difference is greater than 2, miss the data
  d <- diff[diff > 1] 
  
  # In case, miss data
  if (expected.row > observed.row) {
    # print(paste0("pig id: ", id))
    for (j in 1:length(d)) {
      pig.miss.ages[[j]] <- seq(before.miss.rows[j,]$AGE + 1, 
                                before.miss.rows[j,]$AGE + d[j] - 1, 
                                1)
    }
    
    b.m.ages <- list()
    a.m.ages <- list()
    selective.pig.ages <- pig.ages
    
    for (k in 1:length(pig.miss.ages)) {
      sub.miss.ages <- pig.miss.ages[[k]]
      L <- length(sub.miss.ages)
      # Chuỗi ngày cần phải có trước đoạn mất dữ liệu để ước tính dữ liệu.
      b.m.ages[[k]] <- seq(sub.miss.ages[1] - L - 1, 
                          sub.miss.ages[1] - 1, 
                          1)
      # Chuỗi ngày cần phải có sau đoạn mất dữ liệu để ước tính dữ liệu.
      a.m.ages[[k]] <- seq(sub.miss.ages[L] + 1,
                          sub.miss.ages[L] + L + 1, 
                          1)
      # print(sub.miss.ages)
      # print(pig.ages[1])
      # print(b.m.ages)
      # print(a.m.ages)
      
      exception <- FALSE # Xảy ra trường hợp không thể ước lượng dữ liệu thiếu
      # ===================================================================================
      # Trưởng hợp thiếu dữ liệu ở đầu chuỗi tuổi không đủ để ước lượng dữ liệu ngày thiếu
      # Ví dụ: 81 miss 83 84 ... 
      # Thiếu ngày 82 tuy nhiên cần có dữ liệu 2 ngày trước và sau để ước lượng
      # => Không xét ngày 81
      # ===================================================================================
      if (b.m.ages[[k]][1] < pig.ages[1]) {
        selective.pig.ages <- pig.ages[!pig.ages %in% 
                                         seq(pig.ages[1], b.m.ages[[k]][L + 1], 1)]
        exception <- TRUE
      }
      
      # ===================================================================================
      # Trường hợp dữ liệu giữa 2 chuỗi ngày thiếu không đủ để ước lượng dữ liệu thiếu
      # => Loại bỏ những ngày ở giữa
      # ===================================================================================
      if (length(pig.miss.ages) > 1 & k < length(pig.miss.ages)) {
        for (h in 1:(length(pig.miss.ages) - k)) {
          if (pig.miss.ages[[k + h]][1] - sub.miss.ages[L] - 1 <= L) {
            selective.pig.ages <- pig.ages[!pig.ages %in% 
                                             seq(a.m.ages[[k]][1], pig.miss.ages[[k + h]][1] - 1, 1)]
            exception <- TRUE
          }
        }
      }
      
      # ===================================================================================
      # Trưởng hợp thiếu dữ liệu ở cuối chuỗi tuổi không đủ để ước lượng dữ liệu ngày thiếu
      # => Những các dòng dữ liệu ở những ngày cuối
      # ===================================================================================
      if (k == length(pig.miss.ages) & a.m.ages[[k]][L + 1] > pig.ages[length(pig.ages)]) {
        selective.pig.ages <- pig.ages[!pig.ages %in% 
                                         seq(a.m.ages[[k]][1], pig.ages[length(pig.ages)], 1)]
        exception <- TRUE
      } 
      
      if (exception) {
        # print(paste0("pig id: ", id))
        # print(as.vector(pig.ages))
        # print(as.vector(selective.pig.ages))
      }
    }
    data <- JRP_NA %>% filter(ANIMAL_ID == id, AGE %in% selective.pig.ages)
    
  }
  JRP.step.1.1 <- rbind(JRP.step.1.1, data)
}

#Save results to .Rdata file
save(JRP_NA, JRP.step.1.1, file = "data/JRPData.Rdata")

