library(caret)

library(mlbench)

form = as.formula("iterlabel[index] ~ markov1 + markov2 + markov3 + markov4 + markov5 +     
                     SDIntensityBG + IntensityDifference + avg.gabor.mean + avg.gabor.SD + Energy + Homogeneity + Entropy + 
                     thirdordermoment + Inversevariance + Sumaverage + Variance + Clustertendency + MaxProbability +
                     Circularity + Compactness + Eccentricity + Solidity + Extent + RadialDistanceSD + SecondMoment +
                     Area + ConvexArea + Perimeter + ConvexPerimeter + EquivDiameter + MajorAxisLength +
                     MinorAxisLength")


##Import data
stuff <- read.csv("LIDC dataset with full annotations.csv",header=TRUE)
img_fs <- stuff[,c(5:18, 43:69)]
img_fs <- data.frame(img_fs, Avg.Gabor(stuff))

##Process labels
labels <- stuff[,70:73]
labels <- t(apply(labels,1,sample))
labels <- cbind(labels[,1],apply(labels[,1:2],1,mode),
                apply(labels[,1:3],1,mode),apply(labels,1,mode))
labels <- apply(labels,c(1,2),rescale)

## Label tracker
label.tracker <- rep(1,nrow(labels))

#create partition
index <- createDataPartition(labels[,4], list = FALSE, groups =3, p= .9, times =1)

##Iterations
for(r in 1:4)
{
  set.seed(r)
  iterlabel <- label.selector(labels,label.tracker)
  training <- cbind(iterlabel[index], img_fs[index,])
  
  tc <- trainControl(method = "cv",verboseIter = TRUE, returnData = TRUE)
  grid <- expand.grid(C=seq(0.05, 0.25, 0.05))
  
  train.j48 <- train(x = training[,-1], 
                     y= training[,1], 
                     method="J48", 
                     tuneGrid = grid)

}
  