#Basic stuff
{##Absolutely necessary to run initialization BEFORE this file

##Import data
data <- read.csv("LIDC dataset with full annotations.csv",header=TRUE)
img_fs <- data[,c(5:18, 43:69)]
img_fs <- data.frame(img_fs, Avg.Gabor(data))

#Df for results
col <-  c("Mode 1", "Mode 2", "Mode 3", "Max Mode", "Set",
          "I1 Label", "I1 Pred", "I1 Label Added", "I2 Label", 
          "I2 Pred","I2 Label Added", "I3 Label", "I3 Pred",
          "I3 Label Added", "I4 Label", "I4 Pred")
results <- data.frame(data.frame(matrix(vector(), 810, 16, dimnames=list(c(), col))))

## Label tracker
label.tracker <- rep(1,nrow(data))
labelsum <- list()

#Null objects
train = NULL
test = NULL
valid = NULL

tables <- vector(mode="list",length=20)}

#Tuned controls per iteration
ics = rbind(rpart.control(minsplit = 250, minbucket= round(250/4), cp = 0.01),
            rpart.control(minsplit = 150, minbucket= round(150/2), cp = 0.01),
            rpart.control(minsplit = 250, minbucket= round(250/6), cp = 0.01),
            rpart.control(minsplit = 250, minbucket= round(250/6), cp = 0.01))

for (t in 1:20){
  
  set.seed(t)
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
  
  #Separate training, testing and valid
  index <- bal_strat(labels)
  
  #Get image features
  train$img <- as.matrix(img_fs[index$train,])
  test$img <- as.matrix(img_fs[index$test,])
  valid$img <- as.matrix(img_fs[index$valid,])
  
  table <- data.frame(data.frame(matrix(vector(), 50, 7, 
                                        dimnames=list(c(), c("np","nc","tr","teI", 
                                                             "diff", "trM","teM")))))
  #Controls
{ms = 20
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
 
 table[,2] = c(rep(c("6", "4", "2"), 2), rep(c("8","6", "4", "2"), 11))}

#Get label
{

##Iterations
#change end of loop depending on iteration being tuned
for(r in 1:3)
{
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

#Trials for parameter tuning
r = 4

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
colnames(valid$data)[1] <- "label"}

for (i in 1:50){

  #THIS IS WHERE CLASSIFICATION ACTUALLY HAPPENS
  model <- rpart(formula, method = "class", data = train$data, control = controls[i,])
  results[paste("I", r, ".Pred", sep = "")] <- as.integer(predict(model, img_fs, type="class"))
  
  miss.train <- which(results[index$train,paste("I", r, ".Pred", sep = "")]!=
                        results[index$train,paste("I", r, ".Label", sep = "")])
  miss.trM <- which(results[index$train,paste("I", r, ".Pred", sep = "")]!=
                      results[index$train , "Max.Mode"])
  miss.teM <- which(results[index$test ,paste("I", r, ".Pred", sep = "")]!=
                       results[index$test , "Max.Mode"])
  miss.teI <- which(results[index$test ,paste("I", r, ".Pred", sep = "")]!=
                      results[index$test,paste("I", r, ".Label", sep = "")])
  
  avg <- data.frame(data.frame(matrix(vector(), 50, 5, 
                                        dimnames=list(c(), c("tr","trM",
                                                             "teI", "teM", "diff")))))
  
  table[i, "tr"] = 1-length(miss.train)/length(index$train)
  table[i, "trM"] = 1-length(miss.trM)/length(index$train)
  table[i, "teI"] = 1-length(miss.teI)/length(index$test)
  table[i, "teM"] = 1-length(miss.teM)/length(index$test)
  
}
table["diff"] = table["tr"]-table["teI"]
tables[[t]] <- table
}
tables2 = llply(tables, function(df) df[,sapply(df, is.numeric)]) # strip out non-numeric cols
avg  = Reduce("+", tables2)/length(tables2)
View(avg)
View(tables)
View(results)
