##Absolutely necessary to run initialization BEFORE this file

##Import data
data <- read.csv("LIDC dataset with full annotations.csv",header=TRUE)
img_fs <- data[,c(5:18, 43:69)]
img_fs <- data.frame(img_fs, Avg.Gabor(data))

#Df for results
col <-  c("Mode 1", "Mode 2", "Mode 3", "Max Mode", "Set", 
          "I1 Label", "I1 Pred", "I1 Label Num", "I2 Label", 
          "I2 Pred","I2 Label Num", "I3 Label", "I3 Pred",
          "I3 Label Num", "I4 Label", "I4 Pred", "Max.Pred")

t = 20

allaccs <- vector(mode="list",length=t)
allresults <- vector(mode="list",length=t)
allmodels <- vector(mode="list",length=t)
tables <- vector(mode="list")

for (k in 1:t){
  set.seed(k)
  
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
  models = vector(mode="list",length=4)
  train$img <- as.matrix(img_fs[index$train,])
  test$img <- as.matrix(img_fs[index$test,])
  valid$img <- as.matrix(img_fs[index$valid,])
  results[index$train, "Set"] <- "train"
  results[index$test, "Set"] <- "test"
  results[index$valid, "Set"] <- "valid"
  
  g=1
  ##Iterations
  for(r in 1:g)
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
    
    #THIS IS WHERE CLASSIFICATION ACTUALLY HAPPENS
    
    if (r != g){
    model <- rpart(formula, method = "class", data = train$data, control = ics[r])
    models[[r]] <- model
    results[paste("I", r, ".Pred", sep = "")] <- 
      as.integer(predict(model, img_fs, type="class"))
    
    #sum labels at used indices
    labelsum[[r]] = sum(label.tracker[c(index$train, index$test, index$valid)])
    }else{
      table <- data.frame(data.frame(matrix(vector(), 195, 3, 
                                            dimnames=list(c(), c("train",
                                                                 "test", "diff")))))
      for(i in 1:195){
      model <- rpart(formula, method = "class", data = train$data, control = tunecontrols[i,])
      results[paste("I", r, ".Pred", sep = "")] <- 
        as.integer(predict(model, img_fs, type="class"))
      
      miss.iter <- which(results[,paste("I", r, ".Pred", sep = "")]!=
                           results[,paste("I", r, ".Label", sep = "")])
      miss.mode <- which(results[,paste("I", r, ".Pred", sep = "")]!=
                           results[,"Max.Mode"])
      
      table[i,"train"] <- 1-length(which(results[miss.iter, "Set"] == "train"))/length(index$train)
      table[i,"test"] <- 1-length(which(results[miss.iter, "Set"] == "test"))/length(index$test)
      }
      table["diff"] = table["train"]-table["test"]
      tables[[k]] <- table
    }
    
    
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
  
  #Different iterative label vector for each iteration
  conslabel <- label.selector(labels,rep(4, times = length(labels[,4])))
  train$data <- data.frame(cbind(conslabel[index$train], train$img))
  colnames(train$data)[1] <- "label"
  
  if (g == 5){
    r=5
    table <- data.frame(data.frame(matrix(vector(), 195, 4, 
                                          dimnames=list(c(), c("train","test","diff", "valid")))))
    for(i in 1:195){
      model <- rpart(formula, method = "class", data = train$data, control = tunecontrols[i,])
      results["Max.Pred"] <- 
        as.integer(predict(model, img_fs, type="class"))

      miss.mode <- which(results[,"Max.Pred"]!=
                           results[,"Max.Mode"])
      
      table[i,"train"] <- 1-length(which(results[miss.mode, "Set"] == "train"))/length(index$train)
      table[i,"valid"] <- 1-length(which(results[miss.mode, "Set"] == "valid"))/length(index$valid)
      table[i,"test"] <- 1-length(which(results[miss.mode, "Set"] == "test"))/length(index$test)
    }
    table["diff"] = table["train"]-table["test"]
    tables[[k]] <- table
  }
  
  allaccs[[k]] = calcacc(results, index, g)
  allmodels[[k]] = models
  allresults[[k]] = results
}

tables2 = llply(tables, function(df) df[,sapply(df, is.numeric)]) # strip out non-numeric cols
avg  = Reduce("+", tables2)/length(tables2)
View(avg)