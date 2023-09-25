# Clear all variables, function, etc in Global Environment
rm(list = ls(all = TRUE))

setwd(getwd())
#===============================================================
# Import the data set and attribute a name to it
#===============================================================

# Import data to R
JRP_NA.0<- read.csv('data/RFI_JRP.csv', header = TRUE, sep = ',', dec = '.', fileEncoding = 'UTF-8')

#===============================================================
# Remove NAs and correct type of data
#===============================================================
JRP_NA <- JRP_NA.0[!is.na(JRP_NA.0$FEED_INTAKE), ]
#Correct type of data
JRP_NA$ANIMAL_ID <- as.factor(JRP_NA$ANIMAL_ID)
JRP_NA$AGE <- as.numeric(JRP_NA$AGE)
JRP_NA$FEED_INTAKE <- as.numeric(as.character(JRP_NA$FEED_INTAKE))
JRP_NA$CFI <- as.numeric(as.character(JRP_NA$CFI))

#===============================================================
#Save results to .Rdata file
#===============================================================
save(JRP_NA, file = "data/JRPData.Rdata")
