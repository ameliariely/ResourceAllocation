##Absolutely necessary to run initialization BEFORE this file

##Import data
data <- read.csv("LIDC dataset with full annotations.csv",header=TRUE)
img_fs <- data[,c(5:18, 43:69)]
img_fs <- data.frame(img_fs, Avg.Gabor(data))

#Df for results
col <-  c("Mode 1", "Mode 2", "Mode 3", "Max Mode", "Set", 
          "I1 Label", "I1 Pred", "I1 Label Num", "I2 Label", 
          "I2 Pred","I2 Label Num", "I3 Label", "I3 Pred",
          "I3 Label Num", "I4 Label", "I4 Pred", "A1.Pred",
          "A2.Pred","A3.Pred","A4.Pred")

cont = rpart.control(minsplit = 250, minbucket= 58, maxdepth = 5)

t = 20

allaccs <- vector(mode="list",length=t)
allresults <- vector(mode="list",length=t)
allmodels <- vector(mode="list",length=t)
labelorder <- vector(mode="list",length=t)

for (k in 1:t){
  set.seed(k)

results <- data.frame(data.frame(matrix(vector(), 810, 20, dimnames=list(c(), col))))

##Process labels
#currently iterative labeling for both trail and test
labels <- data[,70:73]
#shuffles labels
labels <- t(apply(labels,1,sample))
labelorder[[k]] = apply(labels,c(1,2),rescale)
#this may not be perfect since the actual labels 
#are rescaled after the mode is taken
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
models = vector(mode="list",length=4)
train$img <- as.matrix(img_fs[index$train,])
test$img <- as.matrix(img_fs[index$test,])
valid$img <- as.matrix(img_fs[index$valid,])
results[index$train, "Set"] <- "train"
results[index$test, "Set"] <- "test"
results[index$valid, "Set"] <- "valid"

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
  model <- rpart(formula, method = "class", data = train$data, control = cont)
  models[[r]] <- model
  results[paste("I", r, ".Pred", sep = "")] <- 
    as.integer(predict(model, img_fs, type="class"))
  
  #sum labels at used indices
  labelsum[[r]] = sum(label.tracker[c(index$train, index$test, index$valid)])
  
  ## Update the label tracker
  if(r!=4)
  {
    miss.iter <- which(results[,paste("I", r, ".Pred", sep = "")]!=
                         results[,paste("I", r, ".Label", sep = "")])
    label.tracker[miss.iter] <- label.tracker[miss.iter]+1
    results[paste("I", r, ".Label.Num", sep = "")] <- label.tracker
  }
  
}

#Comparison Consensus Classification

for(a in 1:4){
#Different iterative label vector for each iteration
conslabel <- label.selector(labels,rep(a, times = length(labels[,1])))
train$data <- data.frame(cbind(conslabel[index$train], train$img))
colnames(train$data)[1] <- "label"

model <- rpart(formula, method = "class", data = train$data, control = cont)
results[paste("A", a, ".Pred", sep = "")] <- 
  as.integer(predict(model, img_fs, type="class"))
models[[a+4]] = model
}

allaccs[[k]] = calcacc(results, index, g=8)
allmodels[[k]] = models
allresults[[k]] = results
}

test4accs <- (lapply(allaccs, function(x) x["Test", "I4"]))
deviance <- (lapply(test4accs, function(x, y = mean(as.numeric(test4accs))) abs(x-y)))
best.trial <- max(which(deviance==min(as.numeric(deviance))))

View(allresults[[best.trial]])
best = allresults[[best.trial]]
save(best, file = "best.Rda")
bestorder = labelorder[[best.trial]]
save(bestorder, file = "bestorder.Rda")

agg  = avgacc(allaccs)
View(agg)


