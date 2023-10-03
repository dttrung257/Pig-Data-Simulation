# Clear all variables, function, etc in Global Environment
rm(list = ls(all = TRUE))

setwd(getwd())
# Load data
load("data/JRPData.Rdata")

# Load functions
source("src/funcs.R")

# Get working directory
wd <- getwd()

# set library for create plot
library(ggplot2)
library(htmlwidgets)
library(reticulate)
library(fs)

#Order number of Animal_ID
unique.ids <- unique(JRP_NA$ANIMAL_ID)
#Pig ID
ids <- JRP_NA$ANIMAL_ID
#Age (days)
ages <- JRP_NA$AGE
#Daily Feed Intake (kg)
DFIs <- JRP_NA$FEED_INTAKE
#Cumulative Feed Intake
CFIs <- JRP_NA$CFI

output.dir <- paste0(wd, '/graphs/Observation/') # Folder get output images or html files from graph

clear_data(paste0(output.dir, 'DFI_PNG/')) # clear old images in DFI_PNG folder
check_path(paste0(output.dir, 'DFI_PNG/')) # If the directory does not exist, create a new directory
clear_data(paste0(output.dir, 'CFI_PNG/')) # clear old images in CFI_PNG folder
check_path(paste0(output.dir, 'CFI_PNG/')) # If the directory does not exist, create a new directory

for(i in 1:length(unique.ids)) {
  id <- unique.ids[i]
  data <- JRP_NA[JRP_NA$ANIMAL_ID == id, ]
  DFI.fig <- ggplot(data, aes(x = AGE,y = FEED_INTAKE)) + geom_point(color='green') +
    labs(title = paste0('Daily Feed Intake\n','PigID:',id), x='Age (d)', y='Daily Feed Intake, kg')
  ggsave(filename = paste0(output.dir, 'DFI_PNG/', id, ".", "DFI", ".png"), 
         plot = DFI.fig, width = 8, height = 8)  
  
  CFI.fig <- ggplot(data, aes(x = AGE, y = CFI)) + geom_point(color='green') + 
    labs(title=paste0('Cumulative Feed Intake\n','PigID:',id), x='Age (d)', y='Cumulative Feed Intake, kg')
  ggsave(filename = paste0(output.dir, 'CFI_PNG/', id, ".", "CFI", ".png"), 
         plot = CFI.fig, width = 8, height = 8)  
  print(paste0("Saved plot for pig id: ", id))
}

