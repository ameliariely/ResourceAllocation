##Absolutely necessary to run initialization BEFORE this file

##Import data
data <- read.csv("LIDC dataset with full annotations.csv",header=TRUE)
img_fs <- data[,c(5:18, 43:69)]
img_fs <- data.frame(img_fs, Avg.Gabor(data))

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
texp$misscases$cons <- list()
texp$misscases$iter <- list()
curve <- list()

#New objects
#texp = total experiment (doesn't include unsampled cases)
train = NULL
test = NULL
valid = NULL
texp = NULL

#Separate training, testing and valid
index <- bal_strat(labels)

#Get image features
train$img <- as.matrix(img_fs[index$train,])
test$img <- as.matrix(img_fs[index$test,])
valid$img <- as.matrix(img_fs[index$valid,])

#consl = consensus label
train$consl <- labels[index$train, 4]
test$consl <- labels[index$test, 4]
valid$consl <- labels[index$valid, 4]
texp$consl <- c(train$consl, test$consl, valid$consl)

##Iterations
for(r in 1:4)
{
  
  #Different iterative label vector for each iteration
  iterlabel <- label.selector(labels,label.tracker)
  train$iterl <- iterlabel[index$train]
  test$iterl <- iterlabel[index$test]
  valid$iterl <- iterlabel[index$valid]
  texp$iterl <- c(train$iterl, test$iterl, valid$iterl)
  
  #Make dataframes work for decision trees
  train$data <- data.frame(cbind(train$iterl, train$img))
  colnames(train$data)[1] <- "label"
  test$data <- data.frame(cbind(test$iterl, test$img))
  colnames(test$data)[1] <- "label"
  valid$data <- data.frame(cbind(valid$iterl, valid$img))
  colnames(valid$data)[1] <- "label"
  
  #THIS IS WHERE CLASSIFICATION ACTUALLY HAPPENS
  texp$model <- rpart(formula, method = "class", data = train$data)
  train$predl <- unlist(predict(texp$model, train$data, type="class"))
  test$predl <- unlist(predict(texp$model, test$data, type="class"))
  valid$predl <- unlist(predict(texp$model, valid$data, type="class"))
  texp$predl <- c(train$predl, test$predl, valid$predl)
  
  ##Analyze
  texp$misscases$iter[[r]] <- which(texp$predl != texp$iterl)
  texp$misscases$cons[[r]] <- which(texp$predl != texp$consl)
  
  #Binarize in both directions for ROC
  texp$one$predl <- rescale1(texp$predl)
  texp$one$consl <- rescale1(texp$consl)
  texp$one$iterl <- rescale1(texp$iterl)
  texp$three$predl <- rescale3(texp$predl)
  texp$three$consl <- rescale3(texp$consl)
  texp$three$iterl <- rescale3(texp$iterl)
  
  curve[[r]] <- roc(response = texp$one$consl, predictor = texp$one$predl, plot = TRUE)
  
  ## Update the label tracker
  if(r!=4)
  {
    label.tracker[texp$misscases$iter[[r]]] <- label.tracker[texp$misscases$iter[[r]]]+1
  }
}


