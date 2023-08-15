rm(list=ls(all=TRUE))
setwd("D:/Documents/Pig-Data-Simulation")
load("data/JRPData.Rdata")
# set library for create plot
library(plotly)
library(htmlwidgets)

#Order number of Animal_ID
ID <- unique(JRP_NA$ANIMAL_ID)

#Pig ID
Pig_ID <- JRP_NA$ANIMAL_ID

#Age (days)
Age <- JRP_NA$AGE
#Daily Feed Intake (kg)
DFI <- JRP_NA$FEED_INTAKE


for(i in seq_along(ID)) {
  id <- ID[i]
  Data <- JRP_NA[JRP_NA$ANIMAL_ID == id,]
  DFI.fig <- plot_ly(Data, type = 'scatter', mode = 'markers')
  DFI.fig <- DFI.fig %>%
    add_trace(
      x = Data$AGE,
      y = Data$FEED_INTAKE,
      marker = list(
        color = 'rgb(17, 157, 255)',
        size = 8,
        line = list(
          color = 'rgb(231, 99, 250)',
          width = 1
        )
      ),
      showlegend = F
    )
  DFI.fig <- DFI.fig %>% layout(title=paste0('Daily Feed Intake\n','PigID:',id), xaxis=list(title='Age (d)'), yaxis=list(title='Daily Feed Intake, kg'))
  saveWidget(ggplotly(DFI.fig), file=paste0("D:/Documents/Pig-Data-Simulation/graphs/Observation/DFI/", id, ".", "DFI", ".html"))
  
  CFI.fig <- plot_ly(Data,type = 'scatter', mode = 'markers')
  CFI.fig <- CFI.fig %>%
    add_trace(
      x = Data$AGE,
      y = Data$CFI,
      marker = list(
        color = 'rgb(17, 157, 255)',
        size = 8,
        line = list(
          color = 'rgb(231, 99, 250)',
          width = 1
        )
      ),
      showlegend = F
    )
  CFI.fig <- CFI.fig %>% layout(title=paste0('Cumulative Feed Intake\n','PigID:',id), xaxis=list(title='Age (d)'), yaxis=list(title='Cumulative Feed Intake, kg'))
  saveWidget(ggplotly(DFI.fig), file=paste0("D:/Documents/Pig-Data-Simulation/graphs/Observation/CFI/", id, ".", "CFI", ".html"))
}

