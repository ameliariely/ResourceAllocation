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

#Controls
{  ms = 20
   table [1:3,1] = ms
   c1 <- rpart.control(minsplit = ms, minbucket= round(ms/6), cp = 0.01)
   c2 <- rpart.control(minsplit = ms, minbucket= round(ms/4), cp = 0.01)
   c3 <- rpart.control(minsplit = ms, minbucket= round(ms/2), cp = 0.01)
   ms = 30
   table [4:6,1] = ms
   c4 <- rpart.control(minsplit = ms, minbucket= round(ms/6), cp = 0.01)
   c5 <- rpart.control(minsplit = ms, minbucket= round(ms/4), cp = 0.01)
   c6 <- rpart.control(minsplit = ms, minbucket= round(ms/2), cp = 0.01)
   ms = 40
   table [7:10,1] = ms
   c7 <- rpart.control(minsplit = ms, minbucket= round(ms/8), cp = 0.01)
   c8 <- rpart.control(minsplit = ms, minbucket= round(ms/6), cp = 0.01)
   c9 <- rpart.control(minsplit = ms, minbucket= round(ms/4), cp = 0.01)
   c10 <- rpart.control(minsplit = ms, minbucket= round(ms/2), cp = 0.01)
   ms = 50
   table [11:14,1] = ms
   c11 <- rpart.control(minsplit = ms, minbucket= round(ms/8), cp = 0.01)
   c12 <- rpart.control(minsplit = ms, minbucket= round(ms/6), cp = 0.01)
   c13 <- rpart.control(minsplit = ms, minbucket= round(ms/4), cp = 0.01)
   c14 <- rpart.control(minsplit = ms, minbucket= round(ms/2), cp = 0.01)
   ms = 60
   table [15:18,1] = ms
   c15 <- rpart.control(minsplit = ms, minbucket= round(ms/8), cp = 0.01)
   c16 <- rpart.control(minsplit = ms, minbucket= round(ms/6), cp = 0.01)
   c17 <- rpart.control(minsplit = ms, minbucket= round(ms/4), cp = 0.01)
   c18 <- rpart.control(minsplit = ms, minbucket= round(ms/2), cp = 0.01)
   ms = 70
   table [19:22,1] = ms
   c19 <- rpart.control(minsplit = ms, minbucket= round(ms/8), cp = 0.01)
   c20 <- rpart.control(minsplit = ms, minbucket= round(ms/6), cp = 0.01)
   c21 <- rpart.control(minsplit = ms, minbucket= round(ms/4), cp = 0.01)
   c22 <- rpart.control(minsplit = ms, minbucket= round(ms/2), cp = 0.01)
   ms = 100
   table [23:26,1] = ms
   c23 <- rpart.control(minsplit = ms, minbucket= round(ms/8), cp = 0.01)
   c24 <- rpart.control(minsplit = ms, minbucket= round(ms/6), cp = 0.01)
   c25 <- rpart.control(minsplit = ms, minbucket= round(ms/4), cp = 0.01)
   c26 <- rpart.control(minsplit = ms, minbucket= round(ms/2), cp = 0.01)
   ms = 150
   table [27:30,1] = ms
   c27 <- rpart.control(minsplit = ms, minbucket= round(ms/8), cp = 0.01)
   c28 <- rpart.control(minsplit = ms, minbucket= round(ms/6), cp = 0.01)
   c29 <- rpart.control(minsplit = ms, minbucket= round(ms/4), cp = 0.01)
   c30 <- rpart.control(minsplit = ms, minbucket= round(ms/2), cp = 0.01)
   ms = 200
   table [31:34,1] = ms
   c200 = rbind (rpart.control(minsplit = ms, minbucket= round(ms/8), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/6), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/4), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/2), cp = 0.01))
   ms = 250
   table [35:38,1] = ms
   c250 = rbind (rpart.control(minsplit = ms, minbucket= round(ms/8), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/6), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/4), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/2), cp = 0.01))
   ms = 300
   table [39:42,1] = ms
   c300 = rbind (rpart.control(minsplit = ms, minbucket= round(ms/8), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/6), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/4), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/2), cp = 0.01))
   ms = 350
   table [43:46,1] = ms
   c350 = rbind (rpart.control(minsplit = ms, minbucket= round(ms/8), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/6), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/4), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/2), cp = 0.01))
   ms = 400
   table [47:50,1] = ms
   c400 = rbind (rpart.control(minsplit = ms, minbucket= round(ms/8), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/6), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/4), cp = 0.01),
                 rpart.control(minsplit = ms, minbucket= round(ms/2), cp = 0.01))
   controls = rbind(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14,
                    c15, c16, c17, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27,
                    c28, c29, c30, c200, c250, c300, c350, c400)
   
   table[,2] = c(rep(c("/6", "/4", "/2"), 2), rep(c("8","6", "4", "2"), 11))}

#Controls
ics = rbind(rpart.control(minsplit = 250, minbucket= round(250/4), cp = 0.01),
            rpart.control(minsplit = 150, minbucket= round(150/2), cp = 0.01),
            rpart.control(minsplit = 250, minbucket= round(250/6), cp = 0.01),
            rpart.control(minsplit = 250, minbucket= round(250/6), cp = 0.01),
            rpart.control(minsplit = 250, minbucket= round(250/4), cp = 0.01))

allaccs <- vector(mode="list",length=t)
allresults <- vector(mode="list",length=t)
allmodels <- vector(mode="list",length=t)
tables <- vector(mode="list",length=t)

t = 20
g = 1

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
  models = vector(mode="list",length=70)
  #5 * 14
  train$img <- as.matrix(img_fs[index$train,])
  test$img <- as.matrix(img_fs[index$test,])
  valid$img <- as.matrix(img_fs[index$valid,])
  results[index$train, "Set"] <- "train"
  results[index$test, "Set"] <- "test"
  results[index$valid, "Set"] <- "valid"
  
  ## Temporary output
  train.temp.output <- vector(mode="list",length=4)
  test.temp.output <- vector(mode="list",length=4)
  
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
    test$data <- data.frame(cbind(test$iterl, test$img))
    colnames(test$data)[1] <- "label"
    valid$data <- data.frame(cbind(valid$iterl, valid$img))
    colnames(valid$data)[1] <- "label"
    
    #THIS IS WHERE CLASSIFICATION ACTUALLY HAPPENS
    
    if (r != g){
    model <- rpart(formula, method = "class", data = train$data, control = ics[r])
    allmodels[[k]] <- model
    results[paste("I", r, ".Pred", sep = "")] <- 
      as.integer(predict(model, img_fs, type="class"))
    }else{
      table <- data.frame(data.frame(matrix(vector(), 50, 5, 
                                            dimnames=list(c(), c("trI","trM",
                                                                 "teI", "teM", "diff")))))
      for (i in 1:50){
      model <- rpart(formula, method = "class", data = train$data, control = controls[i])
      results[paste("I", r, ".Pred", sep = "")] <- 
        as.integer(predict(model, img_fs, type="class"))
      
      miss.iter <- which(results[,paste("I", r, ".Pred", sep = "")]!=
                            results[,paste("I", r, ".Label", sep = "")])
      miss.mode <- which(results[,paste("I", r, ".Pred", sep = "")]!=
                            results[,"Max.Mode"])
      
      table[i,"trI"] <- length(which(results[miss.iter, "Set"] == "train"))/length(index$train)
      table[i,"trM"] <- length(which(results[miss.mode, "Set"] == "train"))/length(index$train)
      table[i,"teI"] <- length(which(results[miss.iter, "Set"] == "test"))/length(index$test)
      table[i,"teM"] <- length(which(results[miss.mode, "Set"] == "test"))/length(index$test)
    }
    table["diff"] = table["trI"]-table["teI"]
    tables[[t]] <- table
    }
    
    #sum labels at used indices
    labelsum[[r]] = sum(label.tracker[c(index$train, index$test, index$valid)])
    
    ## Update the label tracker
    if(r!=4)
    {
      results[paste("I", r, ".Label.Added", sep = "")] <- FALSE
      miss.iter <- which(results[,paste("I", r, ".Pred", sep = "")]!=
                            results[,paste("I", r, ".Label", sep = "")])
      label.tracker[miss.iter] <- label.tracker[miss.iter]+1
      results[miss.iter, paste("I", r, ".Label.Added", sep = "")] <- TRUE
    }
    
  }
  
  #Comparison Consensus Classification
  
  if (g==5){
  
  r=5
  model <- rpart(formula, method = "class", data = train$data, control = ics[r])
  #save this?
  results["Max.Pred"] <- 
    as.integer(predict(model, img_fs, type="class"))
  }
  
  
  
  allaccs[[k]] = acc
  allresults[[k]] = results
}

tables2 = llply(tables, function(df) df[,sapply(df, is.numeric)]) # strip out non-numeric cols
avg  = Reduce("+", tables2)/length(tables2)
View(avg)

allaccs2 = llply(allaccs, function(df) df[,sapply(df, is.numeric)]) # strip out non-numeric cols
avgaccs  = Reduce("+", allaccs2)/length(allaccs2)
View(avgaccs)


