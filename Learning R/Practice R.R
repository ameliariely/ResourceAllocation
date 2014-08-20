#Cards 1:
#1) Create a vector of suits called "suits"
suits = c("spades", "hearts", "diamonds", "clubs")

#2) Create a vector of denominations (2,3,4,jack,etc) and call it "denom"
denom = c(2:10, "jack", "queen", "king", "ace")

#3) Look up help for the paste function
?paste

#4) Create vectors for each suit using paste to add the suit to the end of the denom vector e.g. 2H, 
spade = c(paste(suits[1],denom))
heart = c(paste(suits[2],denom))
dia = c(paste(suits[3],denom))
club = c(paste(suits[4],denom))

#5) Combine the vectors into 1 vector called "deck"
deck = c (spade, heart, dia, club)

#6) How many cards are in "deck"?
length(deck)

#6) Find help on the sample command
?sample

#7) Create vector called "hand" that has 5 cards sampled from "deck"
hand = sample(deck, 5,)

#8) Find help for "expand.grid" and use that (with stringsAsFactors=FALSE), paste (hint: you need to use vectors not a matrix), and the vectors "suits" and "denom" to make the deck quicker.
?expand.grid
expand.grid(denom, suits, stringsAsFactors = FALSE)
#v confused

#Fizzbuzz:
#Write a solution to fizzbuzz in R. 
#Write a program that prints the numbers from 1 to 100. 
#But for multiples of three print “Fizz” instead of the number 
#and for the multiples of five print “Buzz”. 
#For numbers which are multiples of both three and five print “FizzBuzz”.

num = seq (1:100)
ifelse(num%%5 != 0, 
       ifelse(num%%3 != 0, print(num), print("Fizz")), 
            ifelse (num%%3 ==0, print("FizzBuzz"), print("Buzz")))


#Cards 2:
#1) Find help on the rep() command
?rep

#2) Create a vector called denomrep with 4 repetitions of the denoms vector
denomrep = rep(denom, 4)

#3) Create a vector called suitsrep that is the c() of 13 "C"s, 13 "S", 13 "H", and 13 "D". Use rep to do this.
suitrep = rep(suits, each = 13)

#4) Create a dataframe caled deckdf with 2 columns, denom and suit, and fill it with the vectors
deckdf = data.frame("denom"= denomrep, "suit"= suitrep)

#5) Create a vector "handindex" that is a sample of size 5 of integers from 1 to the number of rows in deckdf
handindex = sample(1:nrow(deckdf), 5)

#6) Extract the sub-dataframe called "handdf" using the "handindex" vector
handdf = deckdf[handindex,]

#7) Find help on the function duplicated()

#8) use duplicated to see if you have at least a pair in your handdf
anyDuplicated(handdf)


#Cards 3:
#1) Write a function to generate a hand from deckdf.

genHand<- function(deckdf){
  return(deckdf[sample(1:nrow(deckdf), 5),])
}

#2) Write a function to test if that hand is a flush (all cards the same suit).

flush <- function(handdf){
  if (is.null(unique(handdf[,2]))){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

#3) Use your function to generate a list of 10,000 hands

allHands = list()
for(i in 1:10){allHands[[i]]=genHand(deckdf)}

#4) Use sapply along with your flush detector to get a vector of TRUE/FALSE from your hand list.

allHandsF = sapply (allHands, flush)

#5) Use sum() to see how many flushes you got

sum(allHandsF)

#6) What is your calculated probability of getting a flush in 5 card stud?
sum(allHandsF)/length(allHands)

#7) What information can you get from your hand by using table(table(handdf[,1]))?

#How many different denoms are represented of the 13 and how many aren't
