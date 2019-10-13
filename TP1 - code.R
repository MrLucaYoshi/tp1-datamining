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

computeMeanContinue <- function(data, attribute) {
  mean(data[,attribute])
}

computeVarContinue <- function(data, attribute) {
  var(data[,attribute])
}



colVars <- function(x, na.rm=FALSE, dims=1, unbiased=TRUE, SumSquares=FALSE,
                    twopass=FALSE) {
  if (SumSquares) return(colSums(x^2, na.rm, dims))
  N <- colSums(!is.na(x), FALSE, dims)
  Nm1 <- if (unbiased) N-1 else N
  if (twopass) {x <- if (dims==length(dim(x))) x - mean(x, na.rm=na.rm) else
    sweep(x, (dims+1):length(dim(x)), colMeans(x,na.rm,dims))}
  (colSums(x^2, na.rm, dims) - colSums(x, na.rm, dims)^2/N) / Nm1
}


runAnalysis <- function(fileName, discretes, attToRemove, classId) {
  data <- read.table(file=fileName, header=TRUE, sep=",")
  windows()
  hist(colMeans(data[,-c(attToRemove,discretes,classId)]))
  windows()
  hist(colVars(data[,-c(attToRemove, discretes, classId)]))
  
  
  for(d in c(1:length(data))){
    if(!is.element(d,attToRemove)){
      if(is.element(d,discretes)){
        #Discretes
        #P(att)
        windows()
        probabilityDistribution(data,d)
        contTable<-table(data[,d],data[,classId])
        #P(target|att)
        windows()
        barplot(prop.table(contTable),
                main="Distribution target",
                col=c("blue","red","orange","yellow","purple","darkblue","black","white","pink","green"),
                legend=TRUE,
                xlab = "Revenue",
                ylab = names(data)[d]
                )
                
      }else{
        #Continuous
        #
        print(names(data)[d])
        windows()
        print(computeMeanContinue((data)[d]))
        print(computeVarContinue((data)[d]))
        #(target|att)
        
      }
    }
  }
}


#c(1:length(shop))-discretes
#runAnalysis("online_shoppers_intention.csv",c(1,3,5,10,11,12,13,14,15,16,17,18),c(12,13),18)