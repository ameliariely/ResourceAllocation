##Import data
data <- read.csv("LIDC dataset with full annotations.csv",header=TRUE)
img_fs <- data[,c(5:18, 43:69)]
img_fs <- data.frame(img_fs, Avg.Gabor(data))

load("best.Rda")

hrand = sample(1:length(best[,1]), length(best[,1])/2, replace = FALSE)

iterror = cbind(abs(best[,"I1.Label"]-best[,"I1.Pred"]),
                abs(best[,"I2.Label"]-best[,"I2.Pred"]),
                abs(best[,"I3.Label"]-best[,"I3.Pred"]),
                abs(best[,"I4.Label"]-best[,"I4.Pred"]))
itvar = (apply(((iterror - apply(iterror, 1, mean))^2), 1, sum))/length(iterror[1,])

conerror = cbind(abs(best[,"I1.Label"]-best[,"Max.Mode"]),
                abs(best[,"I2.Label"]-best[,"Max.Mode"]),
                abs(best[,"I3.Label"]-best[,"Max.Mode"]),
                abs(best[,"I4.Label"]-best[,"Max.Mode"]))
convar = (apply(((conerror - apply(conerror, 1, mean))^2), 1, sum))/length(conerror[1,])

best$H0 <- 1
best[which(itvar==0), "H0"] <- 0
#483 Es
hard <- which(best["H0"]==1)
easy <- which(best["H0"]==0)

#t is number of trials
t =20
  
  haccs <- vector(mode="list",length=t)
  allresults <- vector(mode="list",length=t)
  eaccs <- vector(mode="list",length=t)
  
  for(k in 1:t){
  
  results <- data.frame(data.frame(matrix(
      vector(), 810, 3, dimnames=list(c(), c("Set","EvH","Pred")))))
    
  #Hard
  index = NULL
  index$train = sample(hard, length(hard)*0.6, replace = FALSE)
  index$test = sample(hard[!hard %in% index$train], length(hard)*0.3, replace = FALSE)
  index$valid = hard[! hard %in% c(index$train, index$test)]
  
  #Get image features
  train = NULL
  test = NULL
  valid = NULL
  alldata = data.frame(cbind(best["Max.Mode"],img_fs))
  colnames(alldata)[1] <- "label"
  results[index$train, "Set"] <- "train"
  results[index$test, "Set"] <- "test"
  results[index$valid, "Set"] <- "valid"
  train$data = alldata[index$train,]
  test$data = alldata[index$test,]
  valid$data = alldata[index$valid,]
  
  #THIS IS WHERE CLASSIFICATION ACTUALLY HAPPENS
  model <- rpart(formula, method = "class", data = train$data, control = ehcontrols[1])
  results[hard,"Pred"] <- 
    as.integer(predict(model, img_fs[hard,], type="class"))
  
  haccs[[k]] = length(which(results[hard,"Pred"]==best[hard,"Max.Mode"]))/length(hard)
  
  #Easy
  index = NULL
  index$train = sample(easy, length(easy)*0.6, replace = FALSE)
  index$test = sample(easy[!easy %in% index$train], length(easy)*0.3, replace = FALSE)
  index$valid = easy[! easy %in% c(index$train, index$test)]
  
  #Get image features
  results[index$train, "Set"] <- "train"
  results[index$test, "Set"] <- "test"
  results[index$valid, "Set"] <- "valid"
  train$data = alldata[index$train,]
  test$data = alldata[index$test,]
  valid$data = alldata[index$valid,]
  
  #THIS IS WHERE CLASSIFICATION ACTUALLY HAPPENS
  model <- rpart(formula, method = "class", data = train$data, control = ehcontrols[1])
  results[easy,"Pred"] <- 
    as.integer(predict(model, img_fs[easy,], type="class"))
  
  eaccs[[k]] = length(which(results[easy,"Pred"]==best[easy,"Max.Mode"]))/length(easy)
  
  allresults[[k]] = results
  
}

tab = cbind(as.numeric(eaccs), as.numeric(haccs))
avg = data.frame(rownames = c("E mean", "H mean"),apply(tab, 2, mean))
View(avg)
