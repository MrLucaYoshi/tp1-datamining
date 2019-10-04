#Enregistrement d'une variable contenant le jeu de données

#myData <- read.table(file="online_shoppers_intention.csv", header=TRUE, sep=",")
################################
#Input exemples:
#discretes<- c(1,3,5,10,11,12,13,14,15,16,17)
#attToRemove<- c(12,13)
#classId<-18 #class = target ???
#runAnalysis(myData,discretes,attToRemove,classId)
#

probabilityDistribution <- function(data, attribute)  {
  barplot(prop.table(table(data[,attribute])),main = names(data)[attribute]) 
}


runAnalysis <- function(fileName, discretes, attToRemove, classId) {
  data <- read.table(file=fileName, header=TRUE, sep=",")
  
  for(d in c(1:length(data))){
    if(is.element(d,discretes)){
      #Discretes
      #P(att)
      windows()
      probabilityDistribution(data,d)
      contTable<-table(data[,d],data[,classId])
      #P(target|att)
      windows()
      barplot(prop.table(contTable),1)
    }else{
      #Continuous
      #
      mean(data[,d])
      var(data[,d])
      #(target|att)
      
    }
  }
  
  
}


#c(1:length(shop))-discretes
