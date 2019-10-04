#Enregistrement d'une variable contenant le jeu de données

#myData <- read.table(file="online_shoppers_intention.csv", header=TRUE, sep=",")

probabilityDistribution <- function(data, attributName)  {
  data[,names(data)[0]]
}



runAnalysis <- function(fileData, indexContVar, classID) {
  myData <- read.table(file=fileData, header=TRUE, sep=",")
  
}

#barplot(prop.table(table(shop[,11])))