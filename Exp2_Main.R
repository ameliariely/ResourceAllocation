##Absolutely necessary to run initialization BEFORE this file

#New objects
train = NULL
test = NULL
valid = NULL

#Separate training, testing and valid
index <- bal_strat(labels)

#Get image features
train$img <- as.matrix(img_fs[index$train,])
test$img <- as.matrix(img_fs[index$test,])
valid$img <- as.matrix(img_fs[index$valid,])