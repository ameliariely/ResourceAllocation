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
labelsum <- list()
texp$misscases$cons <- list()
texp$misscases$iter <- list()

#New objects
#texp = total experiment (doesn't include unsampled cases)
train = NULL
test = NULL
valid = NULL
texp = NULL
results = NULL
comp = NULL

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

train.Acc <- list()
test.Acc <- list()
valid.Acc <- list()

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
  train$predl <- as.integer(predict(texp$model, train$data, type="class"))
  test$predl <- as.integer(predict(texp$model, test$data, type="class"))
  valid$predl <- as.integer(predict(texp$model, valid$data, type="class"))
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
  
  #Calculate ROC
  results$texp$cons$one[[r]] <- roc(response = texp$one$consl, predictor = texp$one$predl, 
                                     auc =TRUE,  plot = TRUE, ci = TRUE)
  results$texp$cons$three[[r]] <- roc(response = texp$three$consl, predictor = texp$three$predl, 
                                       auc =TRUE,  plot = TRUE, ci = TRUE)
  results$texp$iter$one[[r]] <- roc(response = texp$one$iterl, predictor = texp$one$predl, 
                                     auc =TRUE,  plot = TRUE, ci = TRUE)
  results$texp$iter$three[[r]] <- roc(response = texp$three$iterl, predictor = texp$three$predl, 
                                       auc =TRUE,  plot = TRUE, ci = TRUE)
  results$texp$cons$multi[[r]] <- multiclass.roc(response = texp$consl, predictor = texp$predl)
  results$texp$iter$multi[[r]] <- multiclass.roc(response = texp$iterl, predictor = texp$predl)
  
  #Repeat with train
  train$one$predl <- rescale1(train$predl)
  train$one$consl <- rescale1(train$consl)
  train$one$iterl <- rescale1(train$iterl)
  train$three$predl <- rescale3(train$predl)
  train$three$consl <- rescale3(train$consl)
  train$three$iterl <- rescale3(train$iterl)
  results$train$cons$one[[r]] <- roc(response = train$one$consl, predictor = train$one$predl, 
                                     auc =TRUE,  plot = TRUE, ci = TRUE)
  results$train$cons$three[[r]] <- roc(response = train$three$consl, predictor = train$three$predl, 
                                       auc =TRUE,  plot = TRUE, ci = TRUE)
  results$train$iter$one[[r]] <- roc(response = train$one$iterl, predictor = train$one$predl, 
                                     auc =TRUE,  plot = TRUE, ci = TRUE)
  results$train$iter$three[[r]] <- roc(response = train$three$iterl, predictor = train$three$predl, 
                                       auc =TRUE,  plot = TRUE, ci = TRUE)
  results$train$cons$multi[[r]] <- multiclass.roc(response = train$consl, predictor = train$predl)
  results$train$iter$multi[[r]] <- multiclass.roc(response = train$iterl, predictor = train$predl)
  
  #Repeat with test
  test$one$predl <- rescale1(test$predl)
  test$one$consl <- rescale1(test$consl)
  test$one$iterl <- rescale1(test$iterl)
  test$three$predl <- rescale3(test$predl)
  test$three$consl <- rescale3(test$consl)
  test$three$iterl <- rescale3(test$iterl)
  results$test$cons$one[[r]] <- roc(response = test$one$consl, predictor = test$one$predl, 
                                     auc =TRUE,  plot = TRUE, ci = TRUE)
  results$test$cons$three[[r]] <- roc(response = test$three$consl, predictor = test$three$predl, 
                                       auc =TRUE,  plot = TRUE, ci = TRUE)
  results$test$iter$one[[r]] <- roc(response = test$one$iterl, predictor = test$one$predl, 
                                     auc =TRUE,  plot = TRUE, ci = TRUE)
  results$test$iter$three[[r]] <- roc(response = test$three$iterl, predictor = test$three$predl, 
                                       auc =TRUE,  plot = TRUE, ci = TRUE)
  results$test$cons$multi[[r]] <- multiclass.roc(response = test$consl, predictor = test$predl)
  results$test$iter$multi[[r]] <- multiclass.roc(response = test$iterl, predictor = test$predl)
  
  #Repeat with valid
  valid$one$predl <- rescale1(valid$predl)
  valid$one$consl <- rescale1(valid$consl)
  valid$one$iterl <- rescale1(valid$iterl)
  valid$three$predl <- rescale3(valid$predl)
  valid$three$consl <- rescale3(valid$consl)
  valid$three$iterl <- rescale3(valid$iterl)
  results$valid$cons$one[[r]] <- roc(response = valid$one$consl, predictor = valid$one$predl, 
                               auc =TRUE,  plot = TRUE, ci = TRUE)
  results$valid$cons$three[[r]] <- roc(response = valid$three$consl, predictor = valid$three$predl, 
                                 auc =TRUE,  plot = TRUE, ci = TRUE)
  results$valid$iter$one[[r]] <- roc(response = valid$one$iterl, predictor = valid$one$predl, 
                                auc =TRUE,  plot = TRUE, ci = TRUE)
  results$valid$iter$three[[r]] <- roc(response = valid$three$iterl, predictor = valid$three$predl, 
                                  auc =TRUE,  plot = TRUE, ci = TRUE)
  results$valid$cons$multi[[r]] <- multiclass.roc(response = valid$consl, predictor = valid$predl)
  results$valid$iter$multi[[r]] <- multiclass.roc(response = valid$iterl, predictor = valid$predl)
  
  train.Acc[[r]] <- confusionMatrix(train$iterl, train$predl)$overall
  test.Acc[[r]] <- confusionMatrix(test$consl, test$predl)$overall
  valid.Acc[[r]] <- confusionMatrix(valid$consl, valid$predl)$overall
  
  #sum labels at used indices
  labelsum[[r]] = sum(label.tracker[c(index$train, index$test, index$valid)])
  
  ## Update the label tracker
  if(r!=4)
  {
    label.tracker[texp$misscases$iter[[r]]] <- label.tracker[texp$misscases$iter[[r]]]+1
  }
}

#Make dataframes work for decision trees
train$data <- data.frame(cbind(train$consl, train$img))
colnames(train$data)[1] <- "label"
test$data <- data.frame(cbind(test$consl, test$img))
colnames(test$data)[1] <- "label"
valid$data <- data.frame(cbind(valid$consl, valid$img))
colnames(valid$data)[1] <- "label"

##Comparison against consensus build model
texp$compm <- rpart(formula, method = "class", data = train$data)
train$compl <- as.integer(predict(texp$compm, train$data, type="class"))
test$compl <- as.integer(predict(texp$compm, test$data, type="class"))
valid$compl <- as.integer(predict(texp$compm, valid$data, type="class"))
texp$compl <- c(train$compl, test$compl, valid$compl)

texp$one$compl <- rescale1(texp$compl)
texp$three$compl <- rescale3(texp$compl)

#Calculate ROC
comp$texp$one <- roc(response = texp$one$consl, predictor = texp$one$compl, 
                             auc =TRUE,  plot = TRUE, ci = TRUE)
comp$texp$three <- roc(response = texp$three$consl, predictor = texp$three$compl, 
                               auc =TRUE,  plot = TRUE, ci = TRUE)
comp$texp$multi <- multiclass.roc(response = texp$consl, predictor = texp$compl)

#Repeat with train
train$one$compl <- rescale1(train$compl)
train$three$compl <- rescale3(train$compl)
comp$train$one <- roc(response = train$one$consl, predictor = train$one$compl, 
                                   auc =TRUE,  plot = TRUE, ci = TRUE)
comp$train$three <- roc(response = train$three$consl, predictor = train$three$compl, 
                                     auc =TRUE,  plot = TRUE, ci = TRUE)
comp$train$multi <- multiclass.roc(response = train$consl, predictor = train$compl)

#Repeat with test
test$one$compl <- rescale1(test$compl)
test$three$compl <- rescale3(test$compl)
comp$test$one <- roc(response = test$one$consl, predictor = test$one$compl, 
                                  auc =TRUE,  plot = TRUE, ci = TRUE)
comp$test$three <- roc(response = test$three$consl, predictor = test$three$compl, 
                                    auc =TRUE,  plot = TRUE, ci = TRUE)
comp$test$multi <- multiclass.roc(response = test$consl, predictor = test$compl)

#Repeat with valid
valid$one$compl <- rescale1(valid$compl)
valid$three$compl <- rescale3(valid$compl)
comp$valid$one <- roc(response = valid$one$consl, predictor = valid$one$compl, 
                                   auc =TRUE,  plot = TRUE, ci = TRUE)
comp$valid$three <- roc(response = valid$three$consl, predictor = valid$three$compl, 
                                     auc =TRUE,  plot = TRUE, ci = TRUE)
comp$valid$multi <- multiclass.roc(response = valid$consl, predictor = valid$compl)

train.Acc[[5]] <- confusionMatrix(train$consl, train$compl)$overall
test.Acc[[5]] <- confusionMatrix(test$consl, test$compl)$overall
valid.Acc[[5]] <- confusionMatrix(valid$consl, valid$compl)$overall
