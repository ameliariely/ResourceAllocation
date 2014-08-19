setwd("~/Documents/013-14/Summer 2014/MedIX")

## Loading add-on packages
pack.names <- c("MASS","CCA","cluster","kernlab","irr",
                "scales","e1071","phyclust")
sapply(pack.names,library,character.only=TRUE)

## Defined function
mode <- function(x)
{
  tabSmpl<-tabulate(x)
  ifelse((sum(tabSmpl == max(tabSmpl))>1),ceiling(mean(x)),which(tabSmpl== max(tabSmpl)))
}

rescale <- function(x)
{
  value <- ifelse(x==1|x==2,1,ifelse(x==3,2,3))
  return(value)
}

as.dummy <- function(x)
{
  if(x==1)
  {
    value <- c(1,0,0)
  }
  else if(x==2)
  {
    value <- c(0,1,0)
  }
  else
  {
    value <- c(0,0,1)
  }
  return(value)
}

rvi <- function(x)
{
  center <- ifelse(is.na(mode(x)),ceiling(mean(x)),mode(x))
  value <- mean((x-center)^2)
  return(value)
}

label.selector <- function(x,index)
{
  x <- as.vector(t(x))
  value <- x[seq(0,4*(length(index)-1),4)+index]
  return(value)
}

iter.mean.calculator <- function(x,iter.index)
{
  if(iter.index==4) iter.index <-0
  x <- x[(x[,1] %% 4)==iter.index,]
  value <- apply(x,2,mean)
  return(value)
}

iter.rand.ci.calculator <- function(x,iter.index)
{
  if(iter.index==4) iter.index <- 0
  x <- x[(x[,1] %% 4)==iter.index,]
  value <- as.vector(t.test(x[,3])$conf.int)
  return(value)
}

iter.kappa.ci.calculator <- function(x,iter.index)
{
  if(iter.index==4) iter.index <- 0
  x <- x[(x[,1] %% 4)==iter.index,]
  value <- as.vector(t.test(x[,4])$conf.int)
  return(value)
}
