dataSet<-read.csv("hw05_data_set.csv")
#Reading and partitioning the data
dataSetE<- dataSet$eruptions
dataSetW<- dataSet$waiting
trainSet<-dataSet[c(1:150),]
testSet<-dataSet[c(151:272),]
trainE<- trainSet$eruptions
trainW<- trainSet$waiting
testE<- testSet$eruptions
testW<- testSet$waiting
#Min and max values from the data to have data interval
minimumValue<-min(dataSetE)
maximumValue<-max(dataSetE)
dataInterval <- seq(from = (minimumValue-0.1), to = (maximumValue+0.1), by = 0.01)
#Decision Tree REgression is written as a function since the pre prun parameters will be changed
#during the project
#The similar idea is used as the decisiton tree classification as in the lab session.
#Data structure is changed in a sense where it makes it easier to have average responses
#and the corresponding nodes in parallel. Also, the definition of the split score changed from
#impurity to squared loss function.
DecisionTreeReg<- function(P){
  nodeSplits <- c()
  nodeIndices <- list(1:length(trainW))
  isTerminal <- c(FALSE)
  needSplit <- c(TRUE)
  avgResponses<-c()
  while (1) {
  splitNodes <- which(needSplit)
    if (length(splitNodes) == 0) {
    break
  }
  for (splitNode in splitNodes) {
    dataIndices <- nodeIndices[[splitNode]]
    needSplit[splitNode] <- FALSE
    # check whether node is pure by the prepruning parameter, specified by the user.
    #If node is pure, average response from the node, the predicted value is collected
    #nodeSplits sturcture, holding the split points, now have value as terminal,
    #which is the change that is mentioned above
    if (length(trainW[dataIndices]) <= P) {
      nodeSplits[splitNode]<-"TERMINAL"
      avgResponses[splitNode]<-mean(trainW[dataIndices])
      isTerminal[splitNode] <- TRUE
    } else {
      isTerminal[splitNode] <- FALSE
      bestScore <- 0
      bestSplit <- 0
      uniqueValues <- sort(unique(trainE[dataIndices]))
      splitPositions <- (uniqueValues[-1] + uniqueValues[-length(uniqueValues)]) / 2
      splitScores <- rep(0, length(splitPositions))
      for (s in 1:length(splitPositions)) {
          leftIndices <- dataIndices[which(trainE[dataIndices] <= splitPositions[s])]
          rightIndices <- dataIndices[which(trainE[dataIndices] > splitPositions[s])]
          totalError <- 0
          #Here, squared loss for possible left indices of the splits are colllected
          if (length(leftIndices) > 0) {
            mean <- mean(trainW[leftIndices])
            totalError <- totalError + sum((trainW[leftIndices] - mean) ^ 2)
          }
          #Here, squared loss for possible right indices of the splits are colllected
          if (length(rightIndices) > 0) {
            mean <- mean(trainW[rightIndices])
            totalError <- totalError + sum((trainW[rightIndices] - mean) ^ 2)
          }
          #Squared losses from the left and right of the split divided by the total length
          #which is the total number in the parent node.
          splitScores[s] <- totalError / (length(leftIndices) + length(rightIndices))
        }
        #Other than the lab, we have only one minimum split score
        bestScore <- min(splitScores)
        #That split score is used to find where to split
        bestSplit <- splitPositions[which.min(splitScores)]
        #The split point is hold in the nodeSplits structure
      nodeSplits[splitNode] <- bestSplit
      # create left node using the selected split
      leftIndices <- dataIndices[which(trainE[dataIndices] <= bestSplit)]
      nodeIndices[[2 * splitNode]] <- leftIndices
      isTerminal[2 * splitNode] <- FALSE
      needSplit[2 * splitNode] <- TRUE
      # create rigth node using the selected split
      rightIndices <- dataIndices[which(trainE[dataIndices] > bestSplit)]
      nodeIndices[[2 * splitNode + 1]] <- rightIndices
      isTerminal[2 * splitNode + 1] <- FALSE
      needSplit[2 * splitNode + 1] <- TRUE
    }
  }
}
#Function return the structure of the split points: nodeSplits and the average values of the
#terminal nodes
  res<-list("Nodes"=nodeSplits,"Predictions"=avgResponses)
  return(res)
}
#Decision Tree Regression is observed with the prepruning param 25
prePrun25Reg<- DecisionTreeReg(25)
#Preidcition is basically implemented as a function such that, it starts to iterate 
#over the split points. If the current node is not terminal, it continues to iteration
#comparing its value with the split point in the current not. If the split points has value as
#"TERMINAL", it takes the index value and calls the average responses structure with that index
getPredict<-function(decisionTreeNodes,decisionTreeResponses,x){
  i<-1
  while(i<=length(decisionTreeNodes)){
    if(decisionTreeNodes[i]=="TERMINAL"){
      return(decisionTreeResponses[i])
    }
    else if(!(is.na(decisionTreeNodes[i]))){
    if(x>as.double(decisionTreeNodes[i]))
    {
      i<-2*i+1
    }
    else 
    {
        i<-2*i
    }
    }
  }
}
plot(x=trainE,y=trainW,xlab="Eruption time (min)", ylab="Waiting time to next eruption (min)",
     type = "p", main = sprintf("P = 25"),col = "purple",las = 1,pch = 19
)
#legend("topleft",legend=c("training", "test"),fill = c("purple","red"), cex=0.8,pt.cex = 0.001)
points(x=testE,y=testW, col = "red",las = 1,pch = 19)
for (b in 1:(length(dataInterval)-1)) {
  tmp1<-dataInterval[b]
  tmp2<-dataInterval[b+1]
  lines(c(dataInterval[b], dataInterval[b+1]),
        c(getPredict(prePrun25Reg$Nodes,prePrun25Reg$Predictions,tmp1), 
          getPredict(prePrun25Reg$Nodes,prePrun25Reg$Predictions,tmp2)), lwd = 2, col = "black")
}
RMSECalc<-function(testX,testY,nodes, predictions){
  preds<-c()
  for (i in 1:length(testX)) {
    preds[i]=getPredict(nodes,predictions,testX[i])
  }  
  rmse<-sqrt(sum((testY-preds)^2)/length(testY))
  return(rmse)
}
rmse25<-RMSECalc(testE,testW,prePrun25Reg$Nodes,prePrun25Reg$Predictions)
sprintf("RMSE is %g when P is 25", rmse25)
#Second Part of the homework
i<-5
#A structure that holds the rmse values are created. Than, for the multpile of five,
#decision tree regression with that value is created and the corresponding rmse
#is collected in that structure.
rmsePrePrunStruct<-c()
while (i<=50) {
  npStruct<-DecisionTreeReg(i)
  paramRmse<-RMSECalc(testE,testW,npStruct$Nodes,npStruct$Predictions)
  rmsePrePrunStruct<-rbind(rmsePrePrunStruct,cbind(i,paramRmse))
  i<-i+5
}
rmsePrePrunStruct<-as.data.frame(rmsePrePrunStruct)
plot(x=rmsePrePrunStruct$i,y=rmsePrePrunStruct$paramRmse,xlab="Pre-pruning size (P)", ylab="RMSE",
     type = "b",col = "black",las = 1,pch = 19
)

