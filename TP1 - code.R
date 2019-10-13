#Enregistrement d'une variable contenant le jeu de données

#mymyData <- read.table(file="online_shoppers_intention.csv", header=TRUE, sep=",")
################################
#Input exemples:
#discretes<- c(1,3,5,10,11,12,13,14,15,16,17)
#attToRemove<- c(12,13)
#classId<-18 #class = target ???
#runAnalysis(mymyData,discretes,attToRemove,classId)
#

if(.Platform$OS.type=="windows") {
  quartz<-function() windows()
}


probabilityDistribution <- function(myData, attribute)  {
  barplot(prop.table(table(myData[,attribute])),main = names(myData)[attribute])
}


runAnalysis <- function(fileName, discretes, attToRemove, classId) {
  myData <- read.table(file=fileName, header=TRUE, sep=",")
  #quartz()
  #hist(colMeans(myData[,-c(attToRemove,discretes,classId)]))
  #quartz()
  #hist(colVars(myData[,-c(attToRemove, discretes, classId)]))
  print(paste("Valeur cible-> ", names(myData)[classId],sep=" pour "))
  #Moyenne et variance des variables continues
  print("Moyennes des valeurs continues")
  print(colMeans(myData[, -discretes]))
  print("Variances des valeurs continues")
  print(diag(var(myData[, -discretes])))
  
  #Moyenne et variance des variables continues, en fonction de la variable 
  
  for(rowTarget in unique(myData[,classId])){
    print(paste("Moyennes des valeurs continues, en fonction de la variable",rowTarget,sep=" "))
    print(colMeans(myData[myData[,classId]==rowTarget, -discretes]))
    print(paste("Variances des valeurs continues, en fonction de la variable",rowTarget,sep=" "))
    print(diag(var(myData[myData[,classId]==rowTarget, -discretes])))
    #Nuage points
    quartz()
    plot(myData[,8:9],col="green")
    points(myData[myData[,classId]==rowTarget,8:9],col="red")
  }

  
  for(d in c(1:length(myData))){
    if(!is.element(d,attToRemove)){
      if(is.element(d,discretes)){
        #Discretes
        #P(att)
        quartz()
        probabilityDistribution(myData,d)
        contTable<-table(myData[,d],myData[,classId])
        #P(target|att)
        quartz()
        barplot(prop.table(contTable),
                main="Distribution target",
                col=c("blue","red","orange","yellow","purple","darkblue","black","white","pink","green"),
                legend=TRUE,
                xlab = "Revenue",
                ylab = names(myData)[d]
                )
                
      }else{
        #Continuous
        #
        #print(names(myData)[d])
        #print(paste("Moyenne :",mean(myData[,d]),sep = " "))
        #print(paste("variance :",var(myData[,d]),sep = " "))
        quartz()
        hist(prop.table(table(myData[,d])), main=names(myData)[d])
        #En fonction de target
        for(rowTarget in unique(myData[,classId])){
          quartz()
          hist(t(prop.table(table(myData[myData[,classId]==rowTarget,][,c(d,classId)]))), main=paste(rowTarget,names(myData[d]),sep=" "),  breaks = seq(from=0, to=1, by=0.1))
        }
      }
    }
  }
}


#c(1:length(shop))-discretes
#runAnalysis("online_shoppers_intention.csv",c(1,3,5,10,11,12,13,14,15,16,17,18),c(12,13),18)