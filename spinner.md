Spin the Wheel
================

You are at a carnival playing a “Spin the Wheel” type game. Eager to win
a stuffed rat, you wait over an hour in line to play. The rules of the
game are as follows:

You pay 1 ticket to play You spin a fair spinner divided into 10 parts
You keep spinning until you land on a number other than 1, 5, 7 or until
you’ve spun 50 times If the first spin results in number other than 1,
5, or 7, you lose and win nothing If you land on a 1, 5, or 7 you win 5
tickets and spin again If the second spin results in a number other than
1, 5, or 7 you lose and take your 5 tickets. Otherwise, you win
(5∗2)+1=11 tickets. This pattern of earnings up to spin 50 can be
represented with the following equation:

``` 
                            earnings=5n+(n−1)n∈{1,2,3,...,50}
```

According to this formula, the maximum tickets you could earn would be
299. The stuffed rat you have your eye on costs only 20 tickets. Using a
simulation study, determine the likelihood that you’ll be able to afford
the stuffed rat after playing the game.

Create a function called spinner. This function should simulate playing
this carnival game 1 time and return the number of tickets you won. (15
points) Using the function defined above, simulate the game 10,000 times
and store the results. (10 points) Using the values calculated in step
2, calculate an estimate of the likelihood that you win enough tickets
to get the stuffed rat. (10 points) Build a 95% confidence interval on
the estimate you calculated. (10 points)

-----

Load library.

``` r
library(tidyverse)
```

Create spinner function.

``` r
spinner <- function() {
  
  # set spinner outcomes
  spinner_options <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  # store 50 spins
  spins <- sample(spinner_options, 50, replace = TRUE)

  # start number of successful spins at 0
  successful_spins <- 0

  # loop over spins
  for (i in seq_along(spins)) {
  
    if (spins[i] %in% c(1, 5, 7)) {
    
      # increase number of successful spins by 1
      successful_spins <- successful_spins + 1
    
      # if all 50 spins are successful, win max 299 tickets
      if (i == 50) {
      
        return(data_frame(Number_Tickets_Won = 299))
      }
    
      # if it is the first spin you fail on, win nothing
    } else if (i == 1) {
    
      return(data_frame(Number_Tickets_Won = 0))
    
      # for every spin past the first, if you fail on it you get the tickets up to the
      # previous spin
    } else {
    
      num_tickets <- 5*(i - 1) + (i - 2)
      
      return(data_frame(Number_Tickets_Won = num_tickets))
    }
  }
}
```

Test the ‘spinner’ function.

``` r
spinner()
```

    ## # A tibble: 1 x 1
    ##   Number_Tickets_Won
    ##                <dbl>
    ## 1                  0

Simulate the spinner game 10,000 times.

``` r
games <- map_df(1:10000, function(x) spinner())
```

Report an estimate for the likelihood of winning the rat along with
upper and lower 95% confidence bounds.

``` r
games %>%
  summarize(Win_Rat = sum(Number_Tickets_Won >= 20) / n(),
            Lower = Win_Rat - qnorm(0.975) * sqrt(Win_Rat * (1 - Win_Rat) / n()),
            Upper = Win_Rat + qnorm(0.975) * sqrt(Win_Rat * (1 - Win_Rat) / n()))
```

    ## # A tibble: 1 x 3
    ##   Win_Rat   Lower  Upper
    ##     <dbl>   <dbl>  <dbl>
    ## 1  0.0087 0.00688 0.0105
