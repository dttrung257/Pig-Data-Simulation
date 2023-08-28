setwd(getwd())
data <- read.csv("data/data.csv", header = TRUE, sep = ',', fileEncoding = 'UTF-8')
print(data)