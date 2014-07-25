##Absolutely necessary to run initialization BEFORE this file

##Import data
data <- read.csv("LIDC dataset with full annotations.csv",header=TRUE)
img_fs <- data[,c(5:18, 43:69)]
img_fs <- data.frame(img_fs, Avg.Gabor(data))

#Df for results
col <-  c("Mode 1", "Mode 2", "Mode 3", "Max Mode", "Set", 
          "I1 Label", "I1 Pred", "I1 Label Added", "I2 Label", 
          "I2 Pred","I2 Label Added", "I3 Label", "I3 Pred",
          "I3 Label Added", "I4 Label", "I4 Pred", "Max.Pred")
results <- data.frame(data.frame(matrix(vector(), 810, 17, dimnames=list(c(), col))))

##Process labels
#currently iterative labeling for both trail and test
labels <- data[,70:73]
#shuffles labels
labels <- t(apply(labels,1,sample))
#takes the mode for each iteration
labels <- cbind(labels[,1],apply(labels[,1:2],1,mode),
                apply(labels[,1:3],1,mode),apply(labels,1,mode))
labels <- apply(labels,c(1,2),rescale)
results[1:4] <- labels

## Label tracker
label.tracker <- rep(1,nrow(labels))
labelsum <- list()

#Separate training, testing and valid
index <- bal_strat(labels)

#Get image features
train = NULL
test = NULL
valid = NULL
models = vector(mode="list",length=70)
#5 * 14
train$img <- as.matrix(img_fs[index$train,])
test$img <- as.matrix(img_fs[index$test,])
valid$img <- as.matrix(img_fs[index$valid,])
results[index$train, "Set"] <- "train"
results[index$test, "Set"] <- "test"
results[index$valid, "Set"] <- "valid"

#Controls
ics = rbind(rpart.control(minsplit = 250, minbucket= round(250/4), cp = 0.01),
            rpart.control(minsplit = 150, minbucket= round(150/2), cp = 0.01),
            rpart.control(minsplit = 250, minbucket= round(250/6), cp = 0.01),
            rpart.control(minsplit = 250, minbucket= round(250/6), cp = 0.01),
            rpart.control(minsplit = 250, minbucket= round(250/4), cp = 0.01))

##Iterations
for(r in 1:4)
{
  set.seed(r)
  #Different iterative label vector for each iteration
  iterlabel <- label.selector(labels,label.tracker)
  results[paste("I", r, ".Label", sep = "")] <- iterlabel
  train$iterl <- iterlabel[index$train]
  test$iterl <- iterlabel[index$test]
  valid$iterl <- iterlabel[index$valid]
  
  #Make dataframes work for decision trees
  train$data <- data.frame(cbind(train$iterl, train$img))
  colnames(train$data)[1] <- "label"
  test$data <- data.frame(cbind(test$iterl, test$img))
  colnames(test$data)[1] <- "label"
  valid$data <- data.frame(cbind(valid$iterl, valid$img))
  colnames(valid$data)[1] <- "label"
  
  #THIS IS WHERE CLASSIFICATION ACTUALLY HAPPENS
  model <- rpart(formula, method = "class", data = train$data, control = ics[r])
  #save this?
  results[paste("I", r, ".Pred", sep = "")] <- 
    as.integer(predict(model, img_fs, type="class"))
  
  #sum labels at used indices
  labelsum[[r]] = sum(label.tracker[c(index$train, index$test, index$valid)])
  
  ## Update the label tracker
  if(r!=4)
  {
    results[paste("I", r, ".Label.Added", sep = "")] <- FALSE
    miss.train <- which(results[index$train,paste("I", r, ".Pred", sep = "")]!=
                          results[index$train,paste("I", r, ".Label", sep = "")])
    #Different calculations of "Actual Label"
    miss.rest <- which(results[c(index$test, index$valid) ,paste("I", r, ".Pred", sep = "")]!=
                          results[c(index$test, index$valid) , "Max.Mode"])
    label.tracker[c(miss.train, miss.rest)] <- label.tracker[c(miss.train, miss.rest)]+1
    results[c(miss.train, miss.rest), paste("I", r, ".Label.Added", sep = "")] <- TRUE
  }
}

#Comparison Consensus Classification

r=5
model <- rpart(formula, method = "class", data = train$data, control = ics[r])
#save this?
results["Max.Pred"] <- 
  as.integer(predict(model, img_fs, type="class"))

trainCon = cbind(
train1 = confusionMatrix(results[index$train, "I1.Label"], 
                         results[index$train, "I1.Pred"]),
train2 = confusionMatrix(results[index$train, "I2.Label"], 
                         results[index$train, "I2.Pred"]),
train3 = confusionMatrix(results[index$train, "I3.Label"], 
                         results[index$train, "I3.Pred"]),
train4 = confusionMatrix(results[index$train, "I4.Label"], 
                         results[index$train, "I4.Pred"]),
train5 = confusionMatrix(results[index$train, "Max.Mode"], 
                         results[index$train, "Max.Pred"]))

testCon = cbind(
test1 = confusionMatrix(results[index$test, "I1.Label"], 
                         results[index$test, "I1.Pred"]),
test2 = confusionMatrix(results[index$test, "I2.Label"], 
                         results[index$test, "I2.Pred"]),
test3 = confusionMatrix(results[index$test, "I3.Label"], 
                         results[index$test, "I3.Pred"]),
test4 = confusionMatrix(results[index$test, "I4.Label"], 
                         results[index$test, "I4.Pred"]),
test5 = confusionMatrix(results[index$test, "Max.Mode"], 
                         results[index$test, "Max.Pred"]))

validCon = cbind(
valid1 = confusionMatrix(results[index$valid, "I1.Label"], 
                         results[index$valid, "I1.Pred"]),
valid2 = confusionMatrix(results[index$valid, "I2.Label"], 
                         results[index$valid, "I2.Pred"]),
valid3 = confusionMatrix(results[index$valid, "I3.Label"], 
                         results[index$valid, "I3.Pred"]),
valid4 = confusionMatrix(results[index$valid, "I4.Label"], 
                         results[index$valid, "I4.Pred"]),
valid5 = confusionMatrix(results[index$valid, "Max.Mode"], 
                         results[index$valid, "Max.Pred"]))

trainConMax = cbind(
  train1 = confusionMatrix(results[index$train, "I1.Label"], 
                           results[index$train, "Max.Mode"]),
  train2 = confusionMatrix(results[index$train, "I2.Label"], 
                           results[index$train, "Max.Mode"]),
  train3 = confusionMatrix(results[index$train, "I3.Label"], 
                           results[index$train, "Max.Mode"]),
  train4 = confusionMatrix(results[index$train, "I4.Label"], 
                           results[index$train, "Max.Mode"]),
  train5 = confusionMatrix(results[index$train, "Max.Mode"], 
                           results[index$train, "Max.Pred"]))

testConMax = cbind(
  test1 = confusionMatrix(results[index$test, "I1.Label"], 
                          results[index$test, "Max.Mode"]),
  test2 = confusionMatrix(results[index$test, "I2.Label"], 
                          results[index$test, "Max.Mode"]),
  test3 = confusionMatrix(results[index$test, "I3.Label"], 
                          results[index$test, "Max.Mode"]),
  test4 = confusionMatrix(results[index$test, "I4.Label"], 
                          results[index$test, "Max.Mode"]),
  test5 = confusionMatrix(results[index$test, "Max.Mode"], 
                          results[index$test, "Max.Pred"]))

validConMax = cbind(
  valid1 = confusionMatrix(results[index$valid, "I1.Label"], 
                           results[index$valid, "I1.Pred"]),
  valid2 = confusionMatrix(results[index$valid, "I2.Label"], 
                           results[index$valid, "Max.Mode"]),
  valid3 = confusionMatrix(results[index$valid, "I3.Label"], 
                           results[index$valid, "Max.Mode"]),
  valid4 = confusionMatrix(results[index$valid, "I4.Label"], 
                           results[index$valid, "Max.Mode"]),
  valid5 = confusionMatrix(results[index$valid, "Max.Mode"], 
                           results[index$valid, "Max.Pred"]))


acc.col = c("I1","M1","I2","M2","I3","M3","I4","M4","Max")
acc = data.frame(data.frame(matrix(vector(), 3, 9, dimnames=list(c(), acc.col))),
                 row.names= c("Train", "Test","Valid"))

for (j in 1:5){
  acc["Train",(j*2)-1] = trainCon[,j]$overall["Accuracy"] 
  acc["Test",(j*2)-1] = testCon[,j]$overall["Accuracy"]
  acc["Valid",(j*2)-1] = validCon[,j]$overall["Accuracy"]
  
  if(j!=5){
    acc["Train",(j*2)] = trainConMax[,j]$overall["Accuracy"] 
    acc["Test",(j*2)] = testConMax[,j]$overall["Accuracy"]
    acc["Valid",(j*2)] = validConMax[,j]$overall["Accuracy"] 
  }
}
