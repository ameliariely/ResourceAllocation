##########################################################################
#Program: annealtest
#Version: 1
#Author: sort of BJ, but mostly http://www.theprojectspot.com/tutorial-post/simulated-annealing-algorithm-for-beginners/6
#Description:
#	Adapts code from http://www.theprojectspot.com/tutorial-post/simulated-annealing-algorithm-for-beginners/6
# to R to plot the annealing solution to the traveling salesman problem
#		
#Notes: runshow() will plot the tours
##########################################################################

##########################################################################
#Set up functions
##########################################################################

#make a random city. Not used at the moment.
mkcity<-function(){
	v1=sample(1:200,1)
	v2=sample(1:200,1)
	return(c(v1,v2))
	}
#run like:
#cities<-lapply(1:20,function(x){mkcity()})
#but for now...
#copy cities from webpage
cities=list()
cities[[1]]=c(60,200);
cities[[2]]=c(180,200);
cities[[3]]=c(80,180);
cities[[4]]=c(140,180);
cities[[5]]=c(20,160);
cities[[6]]=c(100,160);
cities[[7]]=c(200,160);
cities[[8]]=c(140,140);
cities[[9]]=c(40,120);
cities[[10]]=c(100,120);
cities[[11]]=c(180,100);
cities[[12]]=c(60,80);
cities[[13]]=c(120,80);
cities[[14]]=c(180,60);
cities[[15]]=c(20,40);
cities[[16]]=c(100,40);
cities[[17]]=c(200,40);
cities[[18]]=c(20,20);
cities[[19]]=c(60,20);
cities[[20]]=c(160,20);

#distance function. This is faster than dist() and I dont need the structure of dist
getdist<-function(c1,c2){
	sqrt((c1[1]-c2[1])^2+(c1[2]-c2[2])^2)
}

#this calculates the total tour distance, setting the last stop as the first stop
totaldist<-function(tour){
	totdist=sum(sapply(1:(length(tour)-1), function(i){getdist(cities[[tour[i]]],cities[[tour[i+1]]])}))
	totdist=totdist+getdist(cities[[tour[length(tour)]]],cities[[tour[1]]])
	totdist
}

#This swaps 2 cities in the tour and returns the new tour
swap<-function(tour){
	verts=sample(1:length(tour),2)
	temp1=tour[verts[1]]
	temp2=tour[verts[2]]
	tour[verts[1]]=temp2
	tour[verts[2]]=temp1
	tour	
}

#This is the probability function for accepting a worse solution
getprob<-function(old,new,temp){
	prob=exp((old-new)/temp)
}

##########################################################################
#Main Function
#
#The goal here is to start with a random tour then apply the algorithm
#1) create a new tour by swapping 2 cities
#2) calculate the total tour distance for the new tour and old tour
#3) if the new tour is better, keep it, else get the prob of keeping the tour
#   for the current temp
#4) if prob>runinf(1) keep it, else, stay with the old tour
#5) cool things down
##########################################################################
runanneal<-function(cities, temp, coolrate,showplot=F){
  #showplot makes things slower, but it is neat, shows the current path
  if(showplot){plot(matrix(unlist(cities),ncol=2,byrow=T),col="red",xlab="",ylab="")}

  #set up random tour
  runtour=sample(1:length(cities),length(cities))
  rundist=totaldist(runtour)
  
  #while hot
  while(temp>1){

    #get new tour by swapping 2 cities
    newtour=swap(runtour)
    newdist=totaldist(newtour)
   
    #check if it is better, if so, keep it
    if(newdist<rundist){
      runtour=newtour;
      rundist=newdist;
    }else{
      #if not, then check the prob function and decide
      if(getprob(rundist, newdist,temp)>runif(1)){
        runtour=newtour;
        rundist=newdist;
      }		
    }	
  #plot current path
  if(showplot){
	plot(matrix(unlist(cities),ncol=2,byrow=T),col="red",main=paste("temp:",round(temp,0)),xlab="",ylab="")
	tempmat=matrix(unlist(cities),ncol=2,byrow=T)[runtour,]
  	tempmat=rbind(tempmat,cities[[runtour[1]]])
  	lines(tempmat,col="blue")}

  #cool the system 
  temp=temp*(1-coolrate)
  }
  #report results
  cat("Final distance: ",rundist,"\n")
  #plot final path
  plot(matrix(unlist(cities),ncol=2,byrow=T),col="red",xlab="",ylab="")
  tempmat=matrix(unlist(cities),ncol=2,byrow=T)[runtour,]
  tempmat=rbind(tempmat,cities[[runtour[1]]])
  lines(tempmat,col="blue")
  #return path for fun
  return(tempmat)
}

#run the algorithm
#runanneal(cities, 10000, .003)

#run with showplot to see paths
showrun=function(){runanneal(cities, 10000, .003,TRUE)}

#plot cities for demo
plotblank<-function(){runtour=sample(1:length(cities),length(cities))
	plot(matrix(unlist(cities),ncol=2,byrow=T),col="red",xlab="",ylab="")}

#plot random path for demo
plotrand<-function(){
	runtour=sample(1:length(cities),length(cities))
	plot(matrix(unlist(cities),ncol=2,byrow=T),col="red",xlab="",ylab="")
	tempmat=matrix(unlist(cities),ncol=2,byrow=T)[runtour,]
	tempmat=rbind(tempmat,cities[[runtour[1]]])
	lines(tempmat,col="blue")
	rundist=totaldist(runtour)
	cat("Total distance: ",rundist,"\n")
	
}

