## Loading add-on packages
pack.names <- c("rpart","rpart.plot", "pROC", "caret", "RWeka", "ROCR")
require(plyr)
sapply(pack.names,library,character.only=TRUE)
sapply(pack.names,require,character.only=TRUE)

##Formula for decision tree with all image features
formula = as.formula("label ~ markov1 + markov2 + markov3 + markov4 + markov5 +     
                     SDIntensityBG + IntensityDifference + avg.gabor.mean + avg.gabor.SD + Energy + Homogeneity + Entropy + 
                     thirdordermoment + Inversevariance + Sumaverage + Variance + Clustertendency + MaxProbability +
                     Circularity + Compactness + Eccentricity + Solidity + Extent + RadialDistanceSD + SecondMoment +
                     Area + ConvexArea + Perimeter + ConvexPerimeter + EquivDiameter + MajorAxisLength +
                     MinorAxisLength")


label.selector <- function(x,index)
{
  x <- as.vector(t(x))
  value <- x[seq(0,4*(length(index)-1),4)+index]
  return(value)
}


mode <- function(x)
{
  tabSmpl<-tabulate(x)
  ifelse((sum(tabSmpl == max(tabSmpl))>1),ceiling(mean(x)),which(tabSmpl== max(tabSmpl)))
}


rescale <- function(x)
{
  value <- ifelse(x==1|x==2,1, ifelse( x==3, 2, 3))
  return(value)
}

#1 and 2 are benign, 3 is malignant for ROC
rescale1 <- function(x)
{
  value <- ifelse(x==1|x==2,0, 1)
  return(value)
}

#1 is benign, 2&3 are malignant
rescale3 <- function(x)
{
  value <- ifelse(x==2|x==3,1, 0)
  return(value)
}

#Only works for this specific dataset
Avg.Gabor <- function( dataset )
{
  gabor.features <- dataset[, 19:42]
  
  avg.gabor.mean <- (gabor.features$gabormean_0_0 + gabor.features$gabormean_0_1 + gabor.features$gabormean_0_2 +
                       gabor.features$gabormean_1_0 + gabor.features$gabormean_1_1 + gabor.features$gabormean_1_2 +
                       gabor.features$gabormean_2_0 + gabor.features$gabormean_2_1 + gabor.features$gabormean_2_2 +
                       gabor.features$gabormean_3_0 + gabor.features$gabormean_3_1 + gabor.features$gabormean_3_2)/12
  
  avg.gabor.SD <- (gabor.features$gaborSD_0_0 + gabor.features$gaborSD_0_1 + gabor.features$gaborSD_0_2 
                   + gabor.features$gaborSD_1_0 + gabor.features$gaborSD_1_1 + gabor.features$gaborSD_1_2
                   + gabor.features$gaborSD_2_0 + gabor.features$gaborSD_2_1 + gabor.features$gaborSD_2_2
                   + gabor.features$gaborSD_3_0 + gabor.features$gaborSD_3_1 + gabor.features$gaborSD_3_2)/12
  
  avg.gabor.features <- data.frame(avg.gabor.mean, avg.gabor.SD)
  return(avg.gabor.features)
}

##Only works for this specific dataset
bal_strat <- function(labels){
  
  ##Balance
  ones <- which(labels[,4]==1) #201 24.8%
  twos <- which(labels[,4]==2) #341 42.1%
  threes <- which(labels[,4]==3) #268 33.1%
  
  #twos will be slightly undersampled 
  #so that they don't represent more than 
  #40% of the cases or 324 total
  #793 is new total case number
  
  ##Stratify 60% training, 30% testing, 10% validation
  train.ones <- sample(201, 121, replace=FALSE)
  train.twos <- sample(341, 205, replace=FALSE)
  train.threes <- sample(268, 161, replace=FALSE)
  train.index <- c(ones[train.ones], twos[train.twos], threes[train.threes])
  test.ones <- sample(seq(1:201)[-train.ones], 60, replace=FALSE)
  test.twos <- sample(seq(1:341)[-train.twos], 102, replace=FALSE)
  test.threes <- sample(seq(1:268)[-train.threes], 80, replace=FALSE)
  test.index <- c(ones[test.ones], twos[test.twos], threes[test.threes])
  valid.ones <- sample(seq(1:201)[-c(train.ones, test.ones)], 20,replace=FALSE)
  valid.twos <- sample(seq(1:341)[-c(train.twos, test.twos)], 34, replace=FALSE)
  valid.threes <- sample(seq(1:268)[-c(train.threes, test.threes)], 27, replace=FALSE)
  valid.index <- c(ones[valid.ones], twos[valid.twos], threes[valid.threes])
  index = NULL
  index$train = as.numeric(train.index)
  index$test = as.numeric(test.index)
  index$valid = as.numeric(valid.index)
  return (index)
}


#Accuracies
calcacc <- function (results, index, g){
  d = list(c("Train", "Test", "Valid"),
           rep(c(paste("I", 1:4,sep = ""), paste("M", 1:4,sep = ""), 
               paste("A", 1:4,sep = "")), each =1))
  table <- data.frame(data.frame(matrix(vector(), 3, 12,dimnames=d)))
  ii=1
  for(ii in 1:g){
  if(ii < 5){
    miss.iter <- which(results[,paste("I", ii, ".Pred", sep = "")]!=
                         results[,paste("I", ii, ".Label", sep = "")])
    miss.mode <- which(results[,paste("I", ii, ".Pred", sep = "")]!=
                         results[,"Max.Mode"])
    
  table["Train", paste("I", ii,sep = "")] <- 
    1-length(which(results[miss.iter, "Set"] == "train"))/length(index$train)
  table["Test", paste("I", ii,sep = "")] <- 
    1-length(which(results[miss.iter, "Set"] == "test"))/length(index$test)
  table["Valid", paste("I", ii,sep = "")] <- 
    1-length(which(results[miss.iter, "Set"] == "valid"))/length(index$valid)
  table["Train", paste("M", ii,sep = "")] <- 
    1-length(which(results[miss.mode, "Set"] == "train"))/length(index$train)
  table["Test", paste("M", ii,sep = "")] <- 
    1-length(which(results[miss.mode, "Set"] == "test"))/length(index$test)
  table["Valid", paste("M", ii,sep = "")] <- 
    1-length(which(results[miss.mode, "Set"] == "valid"))/length(index$valid)
} else {
  miss.mode <- which(results[,paste("A", (ii-4), ".Pred", sep = "")]!=
                       labels[,ii-4])
  
  table["Train", paste("A", (ii-4),sep = "")] <- 
    1-length(which(results[miss.mode, "Set"] == "train"))/length(index$train)
  table["Test",paste("A", (ii-4),sep = "")] <- 
    1-length(which(results[miss.mode, "Set"] == "test"))/length(index$test)
  table["Valid", paste("A", (ii-4),sep = "")] <- 
    1-length(which(results[miss.mode, "Set"] == "valid"))/length(index$valid)
}
}
return (table)
}

ms = seq(10, 250, 20)
mb = seq(2, 60, 4)

tunecontrols = expand.grid("minsplit" = ms, "minbucket" = mb, "cp" = 0.01, 
                            "maxcompete" = 4,  "maxsurrogate"	= 5,
                            "usesurrogate" = 2, "surrogatestyle"	=0,
                            "maxdepth" =50)

#Controls
ics = list(rpart.control(minsplit = 170, minbucket= 6),
            rpart.control(minsplit = 170, minbucket= 6),
           rpart.control(minsplit = 50, minbucket= 6),
           rpart.control(minsplit = 110, minbucket= 6),
           rpart.control(minsplit = 250, minbucket= 58),
           rpart.control(minsplit = 210, minbucket= 6),
           rpart.control(minsplit = 130, minbucket= 6),
           rpart.control(minsplit = 150, minbucket= 6))

ehcontrols = c(rpart.control(minsplit = 510, minbucket= 2, cp = 0.01),
            rpart.control(minsplit = 510, minbucket= 2, cp = 0.01),
            rpart.control(minsplit = 510, minbucket= 2, cp = 0.01),
            rpart.control(minsplit = 510, minbucket= 2, cp = 0.01),
            rpart.control(minsplit = 510, minbucket= 2, cp = 0.01))

            
avgacc <- function(allaccs) {
  yiah <- data.frame(
    data.frame(matrix(vector(), 6, 8, 
                      dimnames=list(c("TrainAcc","TrainErr", "TestAcc","TestErr",
                                      "ValidAcc", "ValidErr"), c ("I1","I2","I3","I4",
                                                                  "A1", "A2", "A3", "A4")))))
  for (i in 1:3){
    yiah[(i*2)-1,"I1"] = summary(sapply(allaccs, function (x) x[i, "I1"]))["Mean"]
    yiah[(i*2)-1,"I2"] = summary(sapply(allaccs, function (x) x[i, "I2"]))["Mean"]
    yiah[(i*2)-1,"I3"] = summary(sapply(allaccs, function (x) x[i, "I3"]))["Mean"]
    yiah[(i*2)-1,"I4"] = summary(sapply(allaccs, function (x) x[i, "I4"]))["Mean"]
    yiah[(i*2)-1,"A1"] = summary(sapply(allaccs, function (x) x[i, "A1"]))["Mean"]
    yiah[(i*2)-1,"A2"] = summary(sapply(allaccs, function (x) x[i, "A2"]))["Mean"]
    yiah[(i*2)-1,"A3"] = summary(sapply(allaccs, function (x) x[i, "A3"]))["Mean"]
    yiah[(i*2)-1,"A4"] = summary(sapply(allaccs, function (x) x[i, "A4"]))["Mean"]
    
    yiah[i*2,"I1"] = aerror(sapply(allaccs, function (x) x[i, "I1"]))
    yiah[i*2,"I2"] = aerror(sapply(allaccs, function (x) x[i, "I2"]))
    yiah[i*2,"I3"] = aerror(sapply(allaccs, function (x) x[i, "I3"]))
    yiah[i*2,"I4"] = aerror(sapply(allaccs, function (x) x[i, "I4"]))
    yiah[i*2,"A1"] = aerror(sapply(allaccs, function (x) x[i, "A1"]))
    yiah[i*2,"A2"] = aerror(sapply(allaccs, function (x) x[i, "A2"]))
    yiah[i*2,"A3"] = aerror(sapply(allaccs, function (x) x[i, "A3"]))
    yiah[i*2,"A4"] = aerror(sapply(allaccs, function (x) x[i, "A4"]))
  }
  
  return (yiah)
  
}

aerror <- function(x) {
  qt(0.975,df=length(x)-1)*sd(x)/sqrt(length(x))}

