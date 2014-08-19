#Jillianna Farietta

#1) look at the anscombe dataset by entering the name in the interpreter
anscombe;
#2) look at the first few rows
head(anscombe);
#3) run the linear model y1~x1 (data=anscombe)
lm(anscombe$y1~anscombe$x1);
#4) look at the summary of the model
#5) plot x1,y1 and add a title and x and y labels. 
	#Plot this as points colored blue. Remember to use data=anscombe
plot(anscombe$x1,anscombe$y1, main="Titular", xlab="Xed", ylab="Why?",type="p", col = "blue")
	
#6) use a loop or similar to construct a vector with strings that look like:
#"y1~x1" for 1 to 4 e.g.(y4~x4)
#7) use your vector in a loop to run all 4 linear models
#8) do you see anything interesting?
#9) use apply to get summary stats for each column
#10) create a canvas with 4 panels 2x2 using par(mfrow())
#11) use your vector in a loop to plot all 4 models using as.formula() around your string.
#12) save the results of y1~x1 to a variable
#13) plot the lm
#14) look at ?plot.lm