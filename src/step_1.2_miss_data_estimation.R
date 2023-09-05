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

print(miss.ages)
print(as.vector(JRP_NA %>% filter(ANIMAL_ID == 5781) %>% select(AGE)))

# length(unique.ids)
for (i in 1:length(unique.ids)) {
  id <- unique.ids[i]
}

#Save results to .Rdata file

