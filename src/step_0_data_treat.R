rm(list=ls(all=TRUE)) #Clear all variables, function, vv in Global Environment

setwd(getwd())

#===============================================================
# Import the data set and attribute a name to it
#===============================================================

# Import data to R
JRP_NA.0<- read.csv("data/RFI_JRP.csv", header =TRUE, sep=",", dec=".",fileEncoding="UTF-8-BOM")
# Replace missing values by NA (not available)
#JRP_NA.0[JRP_NA.0 == '.'] <- NA
#===============================================================
# Remove NAs and correct type of data
#===============================================================

JRP_NA <- JRP_NA.0[!is.na(JRP_NA.0$FEED_INTAKE),]
#Correct type of data
JRP_NA$ANIMAL_ID <- as.factor(JRP_NA$ANIMAL_ID)
JRP_NA$AGE <- as.numeric(JRP_NA$AGE)
JRP_NA$FEED_INTAKE <- as.numeric(as.character(JRP_NA$FEED_INTAKE))
JRP_NA$CFI <- as.numeric(as.character(JRP_NA$CFI))

#===============================================================
#Save results to Rdata file
#===============================================================

save( JRP_NA, file = "data/JRPData.Rdata")
