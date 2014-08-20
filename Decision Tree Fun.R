# Classification Tree with rpart
library(rpart)
install.packages("rpart.plot")
require(rpart.plot)

#load iris data
iris = read.table(file = "iris.csv", header = TRUE, sep=",")

#make decision tree with no added parameters
iristree = rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, method = "class", data = iris)

#cp = complexity parameter
printcp(iristree) # display the results
plotcp(iristree) # visualize cross-validation results
summary(iristree) # detailed summary of splits

rpart.plot(iristree)

