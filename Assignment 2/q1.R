# (a)
oneRound<-function(){
  die8 <- sample(1:8, 1)
  die10 <- sample(1:10, 1)
  die12 <- sample(1:12, 1)
  
  result <- c(die8, die10, die12)
  
  names(result) <-c("die8", "die10", "die12")
  result <- sort(result)
  result <- append(result, sum(result))
  names(result)[4] <- "sum"
  result
}

# (b)
nRounds <- function(n){
  dummyFunction <- function(dummyN){
    oneRound()
  }
  lapply(1:n, dummyFunction)
}


# (c)
fourteen <- function(n){
  sum(sapply(nRounds(n), '[', 4) <= 14) / n
  
  # I am extracting the 4th element (which is the sum) of each list (each simulation)
  # Then I am checking if the sums <= 14
  # I am left out with an array of bools
  # If the element is T, the sum is less or equal to 14; it is F otherwise
  # Then I take the sum of the collection of bools
  # R is smart enough to coerce the bools to ints
  # T becomes 1 and F becomes 0 which does not affect our sum
  # In the end, I am left with the count of 1s (True) 
  # Finally I take the division of the count and the length of the sample space, n
}

# (d)
fourteen(1000000)
run5Times <- function(){
  
  dummyF <- function(n){
    N <- 1000000
    fourteen(N)
  }
  mean(sapply(1:5, dummyF))
}

# (e)
# Looks like the probability is about 0.354 (3 s.f)

# (f)
sevens <- function(n){
  sum(apply( sapply(nRounds(n), '[', 1:3), 2, prod ) == 7^3) / n
  
  # Very similar approach to (c)
  # This time I take the product of the first 3 elements (the rolls) of the lists
  # Then I compare each element with 7^3. 
  # The only combination capable of producing a product of 7^3 is (7, 7, 7)
  
}

# (g)
sevens(1000000)
run5Times_ <- function(){
  
  dummyF <- function(n){
    N <- 1000000
    sevens(N)
  }
  mean(sapply(1:5, dummyF))
}

# (h)
# Looks like it's about 0.00104 (3 s.f)

# (i)
game <- function(){
  
  repeat{
    result <- oneRound()
    # print(result)
    if (Reduce('&&',(result[1:3] == c(7,7,7)))){
      return (1)
    }
    
    else if(sum(result[1:3]) <= 14){
    
      next 
    }
    return (0)
    
    
  }
  
}

# (j)
oddOfWinningGame <- function(){
  n <- 5000
  dummyFunction <- function(dummyN){
    game()
  }
  sum (sapply( lapply(1:n, dummyFunction) , '[', 1)) / n
}

# After running about 5000 simulations several times, it looks like the probability of winning the game
# is about 0.001
# The odds of winning is low.



