setwd("~/Documents/013-14/Summer 2014/MedIX/ResourceAllocation")

## Loading add-on packages
pack.names <- c("rpart","rpart.plot")
sapply(pack.names,library,character.only=TRUE)

##Formula for decision tree with all image features
formula = as.formula("label ~ markov1 + markov2 + markov3 + markov4 + markov5 + 
                         SDIntensity + SDIntensityBG + IntensityDifference + gabormean_0_0 + 
                         gaborSD_0_0 + gabormean_0_1 + gaborSD_0_1 + gabormean_0_2 + gaborSD_0_2
                          + gabormean_1_0 + gaborSD_1_0 + gabormean_1_1 + gaborSD_1_1
                          + gabormean_1_2 + gaborSD_1_2 + gabormean_2_0 + gaborSD_2_0
                          + gabormean_2_1 + gaborSD_2_1 + gabormean_2_2 + gaborSD_2_2 + 
                         gabormean_3_0 + gaborSD_3_0 + gabormean_3_1 + gaborSD_3_1 + gabormean_3_2 + 
                         Energy + Homogeneity + Entropy + thirdordermoment + Inversevariance + 
                         Sumaverage + Variance + Clustertendency + MaxProbability + Circularity
                          + Compactness + Eccentricity + Solidity + Extent + RadialDistanceSD + 
                         SecondMoment + Area + ConvexArea + Perimeter + ConvexPerimeter + 
                         EquivDiameter + MajorAxisLength + MinorAxisLength")

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
  value <- ifelse(x==1|x==2,1,ifelse(x==3,2,3))
  return(value)
}

## Loading the data
data <- read.csv("LIDC dataset with full annotations.csv",header=TRUE)
img_fs <- data[,5:69]

##Process labels
#currently iterative labeling for both trail and test
labels <- data[,70:73]
#shuffles labels
labels <- t(apply(labels,1,sample))
#takes the mode for each iteration
labels <- cbind(labels[,1],apply(labels[,1:2],1,mode),
                apply(labels[,1:3],1,mode),apply(labels,1,mode))
labels <- apply(labels,c(1,2),rescale)

## Label tracker
label.tracker <- rep(1,nrow(labels))

##Separate training and testing
index <- sample(810,540,replace=FALSE)
train.img <- as.matrix(img_fs[index,])
test.img <- as.matrix(img_fs[-index,])

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

#Finally create image feature sets
train.img <- as.matrix(img_fs[train.index,])
test.img <- as.matrix(img_fs[test.index,])
valid.img <- as.matrix(img_fs[valid.index,])

total.miss <- vector(mode="list",length=4)

##Iterations
for(r in 1:4)
{
  actual.label <- label.selector(labels,label.tracker)
  train.actual.label <- actual.label[train.index]
  test.actual.label <- actual.label[test.index]
  valid.actual.label <- actual.label[valid.index]
  
  #Make dataframes work for decision trees
  train.data <- data.frame(cbind(train.actual.label, train.img))
  colnames(train.data)[1] <- "label"
  test.data <- data.frame(cbind(test.actual.label, test.img))
  colnames(test.data)[1] <- "label"
  valid.data <- data.frame(cbind(valid.actual.label, valid.img))
  colnames(valid.data)[1] <- "label"
  
  #THIS IS WHERE CLASSIFICATION ACTUALLY HAPPENS
  train.cl.model <- rpart(formula, method = "class", data = train.data)
  train.pred.label <- predict(train.cl.model, train.data, type="class")
  test.pred.label <- predict(train.cl.model, test.data, type="class")
  valid.pred.label <- predict(train.cl.model, valid.data, type="class")
  
  #Store predicted labels
  pred.label <- vector(mode="list",length(actual.label))
  pred.label[train.index] <- train.pred.label
  pred.label[test.index] <- test.pred.label
  pred.label[valid.index] <- valid.pred.label
  
  pred.label <- unlist(pred.label)
  
  ## Update the label tracker
  miss.index <- which(pred.label!= c(actual.label[train.index], 
                                     actual.label[test.index], 
                                     actual.label[valid.index]))
  total.miss[r] <-length(miss.index)
  if(r!=4)
  {
    label.tracker[miss.index] <- label.tracker[miss.index]+1
  }
  
  #cp = complexity parameter
  printcp(train.cl.model) # display the results
  plotcp(train.cl.model) # visualize cross-validation results
  summary(train.cl.model) # detailed summary of splits
  rpart.plot(train.cl.model)
}
