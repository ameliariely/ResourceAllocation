setwd("C://Users/Kyle/Documents/MedIX Stuff/R Work")

## Loading add-on packages
pack.names <- c("rpart","rpart.plot")
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


## Loading the data
data <- read.csv("LIDC dataset with full annotations.csv",header=TRUE)
combined.data <- Avg.Gabor(data)
img_fs <- combined.data[,5:45]
img_fs <- data.frame(img_fs, combined.data[,50:51])

##Process labels
#currently iterative labeling for both trail and test
labels <- data[,70:73]
#shuffles labels
labels <- t(apply(labels,1,sample))
#takes the mode for each iteration
labels <- cbind(labels[,1],apply(labels[,1:2],1,mode),
                apply(labels[,1:3],1,mode),apply(labels,1,mode))
labels <- apply(labels,c(1,2),rescale)
