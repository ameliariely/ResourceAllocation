## Loading add-on packages
pack.names <- c("rpart","rpart.plot", "pROC")
sapply(pack.names,library,character.only=TRUE)

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
  
  avg.gab.data <- data[, -(seq(from = 19, to = 42))]
  avg.gab.data <- data.frame(avg.gab.data, avg.gabor.features)
  return(avg.gab.data)
}

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
  train.twos <- sample(341, 195, replace=FALSE)
  train.threes <- sample(268, 161, replace=FALSE)
  train.index <- c(ones[train.ones], twos[train.twos], threes[train.threes])
  test.ones <- sample(seq(1:201)[-train.ones], 60, replace=FALSE)
  test.twos <- sample(seq(1:341)[-train.twos], 97, replace=FALSE)
  test.threes <- sample(seq(1:268)[-train.threes], 80, replace=FALSE)
  test.index <- c(ones[test.ones], twos[test.twos], threes[test.threes])
  valid.ones <- sample(seq(1:201)[-c(train.ones, test.ones)], 20,replace=FALSE)
  valid.twos <- sample(seq(1:341)[-c(train.twos, test.twos)], 32, replace=FALSE)
  valid.threes <- sample(seq(1:268)[-c(train.threes, test.threes)], 27, replace=FALSE)
  valid.index <- c(ones[valid.ones], twos[valid.twos], threes[valid.threes])
  index = NULL
  index$train = train.index
  index$test = test.index
  index$valid = valid.index
  return (index)
}
