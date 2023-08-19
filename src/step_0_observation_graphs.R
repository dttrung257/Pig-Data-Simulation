rm(list=ls(all=TRUE))
setwd(getwd())
load("data/JRPData.Rdata")
wd <- getwd()
# set library for create plot
library(ggplot2)
library(htmlwidgets)
library(reticulate)
library(fs)

#Order number of Animal_ID
ids <- unique(JRP_NA$ANIMAL_ID)
#Pig ID
pig.ids <- JRP_NA$ANIMAL_ID
#Age (days)
ages <- JRP_NA$AGE
#Daily Feed Intake (kg)
DFIs <- JRP_NA$FEED_INTAKE
#Cumulative Feed Intake
CFIs <- JRP_NA$CFI

# If the directory does not exist, create a new directory
check_path <- function(path) {
  if (!dir_exists(path)) {
    dir_create(path)
  }
}

# Clear all directories and files in folder but don't delete folder
clear_data <- function(path) {
  if (dir_exists((path))) {
    items <- dir_ls(path)
    
    for (item in items) {
      if (is_dir(item)) {
        dir_delete(item, recursive = TRUE, force = TRUE)
      } else {
        file_delete(item)
      }
    }
  }
}

output.dir <- paste0(wd, '/graphs/Observation/')

clear_data(paste0(output.dir, 'DFI_PNG/'))
check_path(paste0(output.dir, 'DFI_PNG/'))
clear_data(paste0(output.dir, 'CFI_PNG/'))
check_path(paste0(output.dir, 'CFI_PNG/'))

for(i in 1:length(ids)) {
  id <- ids[i]
  data <- JRP_NA[JRP_NA$ANIMAL_ID == id, ]
  DFI.fig <- ggplot(data, aes(x = AGE,y = FEED_INTAKE)) + geom_point(color='blue') + 
    labs(title = paste0('Daily Feed Intake\n','PigID:',id), x='Age (d)', y='Daily Feed Intake, kg')
  
  ggsave(filename = paste0(output.dir, 'DFI_PNG/', id, ".", "DFI", ".png"),plot = DFI.fig)  
  
  CFI.fig <- ggplot(data, aes(x = AGE, y = CFI)) + geom_point(color='blue') + 
    labs(title=paste0('Cumulative Feed Intake\n','PigID:',id), x='Age (d)', y='Cumulative Feed Intake, kg')
  ggsave(filename = paste0(output.dir, 'CFI_PNG/', id, ".", "CFI", ".png"),plot = CFI.fig)  
  
}

