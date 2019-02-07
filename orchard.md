Orchard
================

For Christmas my daughters received the game First Orchard. The premise
of the game is as follows:

There is an orchard of 4 unique fruit trees, each of which has 4 pieces
of fruit (each tree contains only one type of fruit, so there are 4
different types of fruit in the game).

There is a raven trying to make it to the orchard before all the fruit
is harvested into a fruit basket.

Each turn of the game, a player rolls a six sided die. Four of the sides
of the die correspond to the four different fruit trees in the orchard.
One side of the die shows a fruit basket, and the remaining side shows a
raven. If a player rolls a fruit, they select one fruit from that tree
and place it in the fruit basket. If the tree that was rolled doesn’t
have any fruit left, nothing happens and the turn ends. If a player
rolls the fruit basket, they choose a single fruit from any tree and
place it in the fruit basket. If a player rolls the raven, the raven
moves one step closer to the orchard.

The game ends when one of two things happen: Either all of the fruit is
harvested into the fruit basket or the raven reaches the orchard. The
raven must be rolled 6 times in order to each the orchard. If the raven
reaches the orchard before all the fruit is harvested, the game is
considered lost.

Most of this game is luck based only on the roll of the die. However,
there is some small strategy involved when the fruit basket is rolled.
We’ll consider the following two strategies:

Highest: When the fruit basket is rolled, the player choose a fruit from
the tree with the most remaining fruits and places it in the fruit
basket.

Random: When the fruit basket is rolled, the player randomly selects one
of the remaining fruit and places it in the fruit basket.

Create a function called orchard. This function should take a single
argument strategy that is either “random” or “highest”, indicating which
of the above strategies will be used. The function should simulate
playing the game one time using the given strategy and return either
TRUE or FALSE based on whether or not the players won the game. In other
words, if all the fruit is harvested, the function should return TRUE.
If the raven advances six spaces before all the fruit is harvested the
function should return FALSE. (20 points)

Using the orchard function from the previous step, simulate playing this
game with each strategy 10,000 times and store the results. (15 points)

Based on the results obtained in step 2, calculate an estimate for the
likelihood of winning (collecting all the fruit before the raven reaches
the orchard) for each strategy. (10 points)

Build a 95% confidence interval on the estimates you calculated in step
3. (10 points)

Based on your estimates and associated confidence intervals, is one
strategy better than the other? Explain your reasoning. (5 points)

-----

``` r
library(tidyverse)
```

Building ‘orchard’ function.

``` r
orchard <- function(strategy) {

  # set number of fruit on each tree
  tree1 <- 4
  tree2 <- 4
  tree3 <- 4
  tree4 <- 4

  # raven starts at zero
  raven <- 0

  # set the values on the die
  die <- c(1, 2, 3, 4, 0, 9)

  # create while loop that runs until the game ends
  while (raven < 6 & sum(tree1, tree2, tree3, tree4) > 0) {

    # roll the die
    roll <- sample(die, 1)

    # conditional on rolling a fruit value, remove the fruit
    if (roll == 1 & tree1 > 0) {
      
      tree1 <- tree1 - 1
  
    } else if (roll == 2 & tree2 > 0) {
  
      tree2 <- tree2 - 1
  
    } else if (roll == 3 & tree3 > 0) {
  
      tree3 <- tree3 - 1
  
    } else if (roll == 4 & tree4 > 0) {
  
      tree4 <- tree4 - 1
  
      # if strategy is random, when you roll a zero, randomly choose a tree to remove
      # fruit from
    } else if (roll == 0 & strategy == 'random') {
  
      trees <- list(tree1, tree2, tree3, tree4)
  
      tree_choices <- which(trees > 0)
  
      choice <- sample(tree_choices, 1)
  
      # remove a fruit based on the random tree choice
    if (choice == 1 & tree1 > 0) {
    
      tree1 <- tree1 - 1
    
    } else if (choice == 2 & tree2 > 0) {
    
      tree2 <- tree2 - 1
    
    } else if (choice == 3 & tree3 > 0) {
    
      tree3 <- tree3 - 1
    
    } else if (choice == 4 & tree4 > 0) {
    
      tree4 <- tree4 - 1
    
    }
  
      # if strategy is highest, when you roll a zero, choose the tree with the highest
      # number of fruit left and remove one fruit
    } else if (roll == 0 & strategy == 'highest') {
  
      most_fruit <- which.max(c(tree1, tree2, tree3, tree4))
  
      # remove a fruit from the tree with the highest amount of fruit
      if (most_fruit == 1) {
    
        tree1 <- tree1 - 1
    
      } else if (most_fruit == 2) {
    
        tree2 <- tree2 - 1
    
      } else if (most_fruit == 3) {
    
        tree3 <- tree3 - 1
    
      } else if (most_fruit == 4) {
    
        tree4 <- tree4 - 1
    
    }
  
      # if you roll a 9, the raven moves forward one space
    } else if (roll == 9) {
  
      raven <- raven + 1

    }  
    
  }

  # if the raven gets to 6, or if all the fruit is removed, the game ends
  if (raven == 6) {
  
    return(FALSE)
  
  } else {
  
    return(TRUE)
  
  }
  
}
```

Simulate playing the game under each strategy 10,000 times and store in
‘high/rand’\_sim. Then calculate the likelihood of winning under each
strategy and store the likelihoods in ‘high/rand’\_like.

``` r
high_sim <- map_lgl(1:10000, function(x) orchard('highest'))
rand_sim <- map_lgl(1:10000, function(x) orchard('random'))

high_like <- sum(high_sim) / length(high_sim)
rand_like <- sum(rand_sim) / length(rand_sim)
```

Display the estimate of the likelihood of winning using the ‘highest’
strategy as well as a 95% confidence interval for the likelihood
estimate.

``` r
data_frame(
  Lower = high_like - qnorm(0.975) * sqrt(high_like * (1 - high_like) / length(high_sim)),
  Estimate = high_like,
  Upper = high_like + qnorm(0.975) * sqrt(high_like * (1 - high_like) / length(high_sim))
)
```

    ## # A tibble: 1 x 3
    ##   Lower Estimate Upper
    ##   <dbl>    <dbl> <dbl>
    ## 1 0.755    0.764 0.772

Display the estimate of the likelihood of winning using the ‘random’
strategy as well as a 95% confidence interval for the likelihood
estimate.

``` r
data_frame(
  Lower = rand_like - qnorm(0.975) * sqrt(rand_like * (1 - rand_like) / length(rand_sim)),
  Estimate = rand_like,
  Upper = rand_like + qnorm(0.975) * sqrt(rand_like * (1 - rand_like) / length(rand_sim))
)
```

    ## # A tibble: 1 x 3
    ##   Lower Estimate Upper
    ##   <dbl>    <dbl> <dbl>
    ## 1 0.681    0.690 0.699

The ‘highest’ strategy is better than the ‘random’ strategy, since the
likelihood of winning under the ‘highest’ strateyg is about .75 and the
likelihood of winning under the ‘random’ strategy is about .70.
