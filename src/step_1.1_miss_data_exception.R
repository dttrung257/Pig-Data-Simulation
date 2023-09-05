rm(list = ls(all = TRUE)) # Clear all variables, function, etc in Global Environment
setwd(getwd())
load("data/JRPData.Rdata")
wd <- getwd()

library(ggplot2)
library(fs)
library(dplyr)

# Lưu những chuỗi ngày mất dữ liệu của lợn với điều kiện < 5 ngày
miss.ages <- list()
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
miss.ages.id <- vector()



# Lặp qua tất cả các con lợn
for (i in 1:length(unique.ids)) {
  # id of pig
  id <- unique.ids[i]

  # data of a pig id = i
  pig.data <- JRP_NA[JRP_NA$ANIMAL_ID == id, ]

  # age (days) of pig id = i
  pig.ages <- pig.data$AGE

  # DFI of pig id = i
  pig.DFIs <- pig.data$FEED_INTAKE

  # CFI of pig id = i
  pig.CFIs <- pig.data$CFI

  # Miss ages of pig id = i
  pig.miss.ages <- list() # Ex: 80  90 91

  # Expected row (In case, don't miss data)
  expected.row <- max(pig.ages) - min(pig.ages) + 1

  # Observed row (In case, miss data)
  observed.row <- as.numeric(length(pig.ages))

  # Age difference between 2 consecutive data lines of pig id i
  diff <- pig.ages[2:length(pig.ages)] - pig.ages[1:(length(pig.ages) - 1)]

  # ages 89 90 94 95
  # diff 1  4  1
  # Days before missing data
  before.miss.rows <- pig.data[diff > 1, ]

  # While the age difference is greater than 2, miss the data
  d <- diff[diff > 1]

  # In case, miss data
  if (expected.row > observed.row) {
    print(paste0("pig id: ", id))
    for (j in 1:length(d)) {
      if (d[j] < 6) {
        pig.miss.ages[[j]] <- seq(
          before.miss.rows[j, ]$AGE + 1,
          before.miss.rows[j, ]$AGE + d[j] - 1,
          1
        )
      }
    }
    if (length(pig.miss.ages) > 0) {
      miss.ages[[length(miss.ages) + 1]] <- pig.miss.ages
      names(miss.ages)[length(miss.ages)] <- as.character(id)
    }
  }
}

# Save results to .Rdata file
save(JRP_NA, miss.ages, file = "data/JRPData.Rdata")
