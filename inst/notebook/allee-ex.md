
Here we consider the Partially Observed Markov Decision Process


First, we will load the libraries needed for this example.  The MDPtoolbox provides simple routines for solving 


```r
library("MDPtoolbox")
```

```
## Loading required package: Matrix
```

```
## Loading required package: linprog
```

```
## Loading required package: lpSolve
```

```r
library("appl")
knitr::opts_chunk$set(cache = TRUE)
```


## Problem definition

Our problem is defined by a state space, `states`, representing the true fish stock size (in arbitrary units), 
and an action space, `actions` representing the number of fish that will be harvested (or attempted to harvest).  
For simplicitly, we will permit any action from 0 harvest to the maximum possible state size.  

A stock recruitment function, `f` describes the expected future state given the current state.  The true future
state will be a stochastic draw with this mean.

A reward function determines the value of taking action of harvesting `h` fish when stock size is `x` fish;
for simplicity this example assumes a fixed price per unit harvest, with no cost on harvesting effort. 
Future rewards are discounted.


```r
states <- 0:60
actions <- states

f <- function(x, h, r = 1, K = 50, C = 20){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) * (s - C) / K)
}

sigma_g <- sqrt(log(1 + 0.5 / 6)) # Scale the log-standard-deviation to result in similar variance to a uniform distribution of width 0.5

reward_fn <- function(x,h) pmin(x,h)
discount <- 0.95
```

## Exact / semi-analytic solution

For comparison, we note that an exact solution to the deterministic or low-noise problem comes from Reed 1979, which proves that a constant escapement
policy $S^*$ is optimal, with $\tfrac{df}{dx}|_{x = S^*} = 1/\gamma$ for discount $\gamma$,


```r
fun <- function(x) -f(x,0) + x / discount
out <- optimize(f = fun, interval = c(min(states),max(states)))
S_star <- round(out$minimum)
exact_policy <- sapply(states, function(x) if(x < S_star) 0 else x - S_star)
```

# Generate Matrices

When the state is observed without error, the problem is a Markov Decision Process (MDP) and can be solved by 
stochastic dynamic programming (e.g. policy iteration) over the discrete state and action space. To do so, we need
matrix representations of the above transition function and reward function:


```r
n_s <- length(states)
n_a <- length(actions)
transition <- array(0, dim = c(n_s, n_s, n_a))
reward <- array(0, dim = c(n_s, n_a))

for (k in 1:n_s) {
  for (i in 1:n_a) {
    nextpop <- f(states[k], actions[i])
    if(nextpop <= 0)
      transition[k, , i] <- c(1, rep(0, n_s - 1))
    else if(sigma_g > 0){
      x <- dlnorm(states, log(nextpop), sdlog = sigma_g)    # transition probability densities
      N <- plnorm(states[n_s], log(nextpop), sigma_g)       # CDF accounts for prob density beyond boundary
      x <- x * N / sum(x)                                   # normalize densities to  = cdf(boundary)
      x[n_s] <- 1 - N + x[n_s]                              # pile remaining probability on boundary
      transition[k, , i] <- x                             # store as row of transition matrix
    } else {
     stop("sigma_g not > 0")
    }
    reward[k, i] <- reward_fn(states[k], actions[i])
  }
}
```

## Numerical SDP Solution


```r
mdp <- MDPtoolbox::mdp_policy_iteration(transition, reward, discount)
```

```
## Note: method with signature 'Matrix#matrix' chosen for function '-',
##  target signature 'ddiMatrix#matrix'.
##  "ddiMatrix#ANY" would also be valid
```

```
## Note: method with signature 'ddiMatrix#dMatrix' chosen for function '-',
##  target signature 'ddiMatrix#dtCMatrix'.
##  "diagonalMatrix#triangularMatrix" would also be valid
```


## POMDP problem

In the POMDP problem, the true state is unknown, but measured imperfectly.  We introduce
an observation matrix to indicate the probabilty of observing a particular state $y$ given
a true state $x$. In principle this could depend on the action taken as well, though for 
simplicity we assume only a log-normal measurement error independent of the action chosen.


```r
sigma_m <- sigma_g
observed_states <- states
n_z <- length(observed_states)

observation <- array(0, dim = c(n_s, n_z, n_a))
for (k in 1:n_a) {
  if(sigma_m <= 0){
    observation[, , k] <- diag(n_s)
  } else {
    for (i in 1:n_s) {
      if(states[i] <= 0){ ## cannot do dlnorm with mu = log(0) = -Inf.  Cannot solve if belief has already converged
        x <- dlnorm(observed_states, -1, sigma_m)
        observation[i, , k] <- x / sum(x)
      } else {
        x <- dlnorm(observed_states, log(states[i]), sdlog = sigma_m)    # transition probability densities
        ## Normalize using CDF
        N <- plnorm(observed_states[n_s], log(states[i]), sigma_m)       # CDF accounts for prob density beyond boundary
        x <- x * N / sum(x)                                   # normalize densities to  = cdf(boundary)
        x[n_s] <- 1 - N + x[n_s]                              # pile remaining probability on boundary
        observation[i, , k] <- x                             # store as row of transition matrix
      }
    }
  }
}
```


With the transition matrix, observation matrix, reward matrix, and discount factor in hand, we have now fully specified the POMDP problem and are ready to solve.

Note that unlike the MDP algorithm used above, this solution is approximate, and some care must be taken to ensure the solution has converged appropriately.  Here
we set a desired precision limit, but also set a memory limit which will halt the algroithm early rather than permit any node to exceed this memory allocation.




```r
system.time( soln <- appl::pomdp(transition, observation, reward, discount, mc.cores = 2, precision = 5, memory = 7750) )
```

```
##     user   system  elapsed 
## 3790.455   10.263 3975.999
```

```r
soln
```

```
## $value
##  [1]  0.000000  0.547619  2.302326  3.366279  4.500000  5.611765  6.742690
##  [8]  7.857988  8.946108 10.076923 11.205882 12.358824 13.443787 14.511765
## [15] 15.758824 16.822485 17.888235 19.065476 20.127168 22.071768 24.632534
## [22] 25.542037 27.470579 29.276386 30.838187 32.487216 34.025401 35.215851
## [29] 36.895384 37.844237 39.015765 40.077113 41.182476 41.717809 42.330695
## [36] 43.497639 44.337957 45.253937 46.284044 46.589985 47.462920 47.832997
## [43] 48.590414 49.461372 50.253525 50.519447 51.440770 51.799470 52.455761
## [50] 52.679871 53.408492 53.805050 54.100156 54.716798 55.051141 55.352436
## [57] 55.812866 55.817947 56.655197 56.839903 58.782334
## 
## $policy
##  [1]  0  6  6  7  9 11 13 15 17 19 21 23 25 27 29 31 33 35 36  0  0  0  0
## [24]  0  0  0  0  0  0  0  0  0  0  1  1  2  3  4  5  6  7  7  8  9  9 10
## [47] 10 11 11 11 12 12 12 13 13 14 14 14 15 15 17
## 
## $diagnostics
##       load_time_sec init_time_sec run_time_sec final_precision
##  [1,] 0.88          11.47         11.47        0              
##  [2,] 0.83          11.49         11.49        0              
##  [3,] 0.85          11.53         11.53        0              
##  [4,] 0.87          11.93         11.93        0              
##  [5,] 0.91          11.76         11.76        0              
##  [6,] 0.87          11.74         11.74        0              
##  [7,] 0.9           11.96         11.96        0              
##  [8,] 0.88          11.85         11.85        0              
##  [9,] 0.93          11.85         11.86        0              
## [10,] 0.87          12.1          12.11        0              
## [11,] 0.93          11.7          11.71        0              
## [12,] 0.92          11.85         11.85        0              
## [13,] 0.98          12.04         12.1         0.544595       
## [14,] 0.91          11.85         11.92        1.36766        
## [15,] 0.92          11.74         11.81        2.11059        
## [16,] 0.92          11.76         11.83        2.94392        
## [17,] 0.9           11.73         11.81        3.83229        
## [18,] 0.9           11.72         11.79        4.70813        
## [19,] 0.91          11.67         15.71        4.66531        
## [20,] 0.93          11.72         23.48        3.96481        
## [21,] 0.91          11.66         20.55        4.98086        
## [22,] 0.91          11.84         24.61        4.79747        
## [23,] 0.92          11.71         25.25        4.59739        
## [24,] 0.94          11.92         28.71        4.83226        
## [25,] 0.91          11.87         28.37        4.63786        
## [26,] 0.91          11.9          33.91        4.82854        
## [27,] 0.84          11.06         27.04        4.87005        
## [28,] 0.93          11.91         35.29        4.77659        
## [29,] 0.91          11.66         36.66        4.85834        
## [30,] 0.91          11.69         41.15        4.73178        
## [31,] 0.91          11.76         31.88        4.93798        
## [32,] 0.91          11.24         45.42        4.75927        
## [33,] 0.91          11.72         72.19        4.98625        
## [34,] 0.84          11.58         147.22       4.94143        
## [35,] 0.91          11.78         114.52       4.82762        
## [36,] 0.83          11.11         57.89        4.90423        
## [37,] 0.92          11.94         56.16        4.96175        
## [38,] 0.84          10.99         46.38        4.78656        
## [39,] 0.96          11.85         52.52        4.94252        
## [40,] 0.9           11.8          72.68        4.77777        
## [41,] 0.92          11.77         80.74        4.90656        
## [42,] 0.92          11.67         92.58        4.94207        
## [43,] 0.91          11.66         63.18        4.92095        
## [44,] 0.93          11.75         55.45        4.72456        
## [45,] 0.92          11.84         53.94        4.98803        
## [46,] 0.92          11.8          59.48        4.8072         
## [47,] 0.91          11.8          68.31        4.71492        
## [48,] 0.93          11.69         41.47        4.9463         
## [49,] 0.92          11.48         37.23        4.97319        
## [50,] 0.9           11.67         47.73        4.72222        
## [51,] 0.9           11.72         39.86        4.72483        
## [52,] 0.93          11.65         45.41        4.75391        
## [53,] 0.93          11.69         62.77        4.76809        
## [54,] 0.91          11.68         46.02        4.8986         
## [55,] 0.91          11.86         44.44        4.83723        
## [56,] 0.91          11.69         102.56       4.7743         
## [57,] 0.91          11.78         105.03       4.96981        
## [58,] 0.93          11.78         137.58       4.65609        
## [59,] 0.94          11.68         79.42        4.98558        
## [60,] 0.88          11.37         80.35        4.89945        
##       end_condition               
##  [1,] "  target precision reached"
##  [2,] "  target precision reached"
##  [3,] "  target precision reached"
##  [4,] "  target precision reached"
##  [5,] "  target precision reached"
##  [6,] "  target precision reached"
##  [7,] "  target precision reached"
##  [8,] "  target precision reached"
##  [9,] "  target precision reached"
## [10,] "  target precision reached"
## [11,] "  target precision reached"
## [12,] "  target precision reached"
## [13,] "  target precision reached"
## [14,] "  target precision reached"
## [15,] "  target precision reached"
## [16,] "  target precision reached"
## [17,] "  target precision reached"
## [18,] "  target precision reached"
## [19,] "  target precision reached"
## [20,] "  target precision reached"
## [21,] "  target precision reached"
## [22,] "  target precision reached"
## [23,] "  target precision reached"
## [24,] "  target precision reached"
## [25,] "  target precision reached"
## [26,] "  target precision reached"
## [27,] "  target precision reached"
## [28,] "  target precision reached"
## [29,] "  target precision reached"
## [30,] "  target precision reached"
## [31,] "  target precision reached"
## [32,] "  target precision reached"
## [33,] "  target precision reached"
## [34,] "  target precision reached"
## [35,] "  target precision reached"
## [36,] "  target precision reached"
## [37,] "  target precision reached"
## [38,] "  target precision reached"
## [39,] "  target precision reached"
## [40,] "  target precision reached"
## [41,] "  target precision reached"
## [42,] "  target precision reached"
## [43,] "  target precision reached"
## [44,] "  target precision reached"
## [45,] "  target precision reached"
## [46,] "  target precision reached"
## [47,] "  target precision reached"
## [48,] "  target precision reached"
## [49,] "  target precision reached"
## [50,] "  target precision reached"
## [51,] "  target precision reached"
## [52,] "  target precision reached"
## [53,] "  target precision reached"
## [54,] "  target precision reached"
## [55,] "  target precision reached"
## [56,] "  target precision reached"
## [57,] "  target precision reached"
## [58,] "  target precision reached"
## [59,] "  target precision reached"
## [60,] "  target precision reached"
```



```r
policies <- data.frame(states = states,
                       exact = states - exact_policy,
                       mdp = states - actions[mdp$policy],
                       pomdp = states - soln$policy)

library("tidyr")
```

```
## 
## Attaching package: 'tidyr'
```

```
## The following object is masked from 'package:Matrix':
## 
##     expand
```

```r
library("ggplot2")
tidyr::gather(policies, soln, escapement, -states) %>%
  ggplot2::ggplot(ggplot2::aes(states, escapement, col = soln)) + ggplot2::geom_point()
```

![](allee-ex_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

