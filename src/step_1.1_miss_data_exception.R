# Clear all variables, function, etc in Global Environment
rm(list = ls(all = TRUE))

setwd(getwd())
# Load data
load("data/JRPData.Rdata")

# Load functions
# source("src/funcs.R")

# Get working directory
wd <- getwd()

library(ggplot2)
library(fs)
library(dplyr)

# Lưu những chuỗi ngày mất dữ liệu của lợn với điều kiện < 5 ngày
lol.miss.ages <- list() # list of lists
lov.miss.ages <- list() # list of vectors
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
  l.pig.miss.ages <- list() # Ex: 80  90 91
  v.pig.miss.ages <- vector()

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
        l.pig.miss.ages[[j]] <- seq(
          before.miss.rows[j, ]$AGE + 1,
          before.miss.rows[j, ]$AGE + d[j] - 1,
          1
        )
        v.pig.miss.ages <- c(v.pig.miss.ages, seq(
          before.miss.rows[j, ]$AGE + 1,
          before.miss.rows[j, ]$AGE + d[j] - 1,
          1
        ))
      }
    }
    if (length(l.pig.miss.ages) > 0) {
      lol.miss.ages[[length(lol.miss.ages) + 1]] <- l.pig.miss.ages
      lov.miss.ages[[length(lov.miss.ages) + 1]] <- v.pig.miss.ages
      names(lol.miss.ages)[length(lol.miss.ages)] <- as.character(id)
      names(lov.miss.ages)[length(lov.miss.ages)] <- as.character(id)
    }
  }
}

print(lol.miss.ages)
print(lov.miss.ages)

# Save results to .Rdata file
save(JRP_NA, lol.miss.ages, lov.miss.ages, file = "data/JRPData.Rdata")
