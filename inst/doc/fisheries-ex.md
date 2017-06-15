Fisheries Example using POMDP
================
Carl Boettiger
2016-07-01

Here we compare the Markov Decision Process (MDP) solution of the classic optimal harvest problem in fisheries (Reed 1979) to the corresponding solution under measurment uncertainty, the Partially Observed Markov Decision Process (POMDP) problem. The classic problem can be solved exactly for a discrete model using Stochastic Dynamic Programming. Here we demonstrate a computationally efficient approximate solution using the point-based SARSOP algorithm for POMDP, implemented in C++ in by the [APPL]() software and provided here as an R package. We will first set up the problem, then present the analytic solution to deterministic problem, followed by the MDP solution to the stochastic problem. As Reed proved in 1979, these solutions are identical as long as the stochasticity is small enough for the population to meet the self-sustaining criterion. We then introduce measurement uncertainty and illustrate the resulting POMDP solution, discussing some of issues the user should be aware of when utilizing these approximate algorithms.

First, we will load the libraries needed for this example. The `MDPtoolbox` provides simple routines for solving MDP problems, while the `appl` library provides the `POMDP` routines.

``` r
library("MDPtoolbox")
library("sarsop")
```

Problem definition
------------------

Our problem is defined by a state space, `states`, representing the true fish stock size (in arbitrary units), and an action space, `actions` representing the number of fish that will be harvested (or attempted to harvest).
For simplicitly, we will permit any action from 0 harvest to the maximum possible state size.

A stock recruitment function, `f` describes the expected future state given the current state. The true future state will be a stochastic draw with this mean.

A reward function determines the value of taking action of harvesting `h` fish when stock size is `x` fish; for simplicity this example assumes a fixed price per unit harvest, with no cost on harvesting effort. Future rewards are discounted.

``` r
states <- 0:15
actions <- states

f <- function(x, h, r = 1, K = 12){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}

sigma_g <- sqrt(log(1 + 0.5 / 6)) # Scale the log-standard-deviation to result in similar variance to a uniform distribution of width 0.5

reward_fn <- function(x,h) pmin(x,h)
discount <- 0.95
```

Exact / semi-analytic solution
------------------------------

For comparison, we note that an exact solution to the deterministic or low-noise problem comes from Reed 1979, which proves that a constant escapement policy \(S^*\) is optimal, with \(\tfrac{df}{dx}|_{x = S^*} = 1/\gamma\) for discount \(\gamma\),

``` r
fun <- function(x) -f(x,0) + x / discount
out <- optimize(f = fun, interval = c(min(states),max(states)))
S_star <- round(out$minimum)
exact_policy <- sapply(states, function(x) if(x < S_star) 0 else x - S_star)
```

Generate Matrices
=================

When the state is observed without error, the problem is a Markov Decision Process (MDP) and can be solved by stochastic dynamic programming (e.g. policy iteration) over the discrete state and action space. To do so, we need matrix representations of the above transition function and reward function:

``` r
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
      x <- dlnorm(states, log(nextpop), sdlog = sigma_g) 
      # Normalize, pile on boundary
      N <- plnorm(states[n_s], log(nextpop), sigma_g)
      x <- x * N / sum(x)
      x[n_s] <- 1 - N + x[n_s]
      transition[k, , i] <- x
    } else {
     stop("sigma_g not > 0")
    }
    reward[k, i] <- reward_fn(states[k], actions[i])
  }
}
```

Numerical SDP Solution
----------------------

``` r
mdp <- MDPtoolbox::mdp_policy_iteration(transition, reward, discount)
```

POMDP problem
-------------

In the POMDP problem, the true state is unknown, but measured imperfectly. We introduce an observation matrix to indicate the probabilty of observing a particular state \(y\) given a true state \(x\). In principle this could depend on the action taken as well, though for simplicity we assume only a log-normal measurement error independent of the action chosen.

``` r
sigma_m <- sigma_g
observed_states <- states
n_z <- length(observed_states)

observation <- array(0, dim = c(n_s, n_z, n_a))
for (k in 1:n_a) {
  if(sigma_m <= 0){
    observation[, , k] <- diag(n_s)
  } else {
    for (i in 1:n_s) {
      if(states[i] <= 0){ 
        ## cannot do dlnorm with mu = log(0) = -Inf. 
        x <- dlnorm(observed_states, -1, sigma_m)
        observation[i, , k] <- x / sum(x)
      } else {
        x <- dlnorm(observed_states, log(states[i]), sdlog = sigma_m)
        ## Normalize using CDF
        N <- plnorm(observed_states[n_s], log(states[i]), sigma_m)  
        x <- x * N / sum(x) 
        x[n_s] <- 1 - N + x[n_s]
        observation[i, , k] <- x
      }
    }
  }
}
```

With the transition matrix, observation matrix, reward matrix, and discount factor in hand, we have now fully specified the POMDP problem and are ready to solve.

Note that unlike the MDP algorithm used above, this solution is approximate, and some care must be taken to ensure the solution has converged appropriately. Here we set a desired precision limit, but also set a memory limit which will halt the algroithm early rather than permit any node to exceed this memory allocation.

``` r
system.time( soln <- pomdp(transition, observation, reward, discount, mc.cores = 2, precision = 5, memory = 2000, timeout = 1000) )
```

    ##    user  system elapsed 
    ##  14.996   1.312   6.849

Check diagnostics for each state to make sure each state reaches the target precision rather than hitting the memory limit or timeout.

``` r
soln$diagnostics
```

    ##       load_time_sec init_time_sec run_time_sec final_precision
    ##  [1,] 0.02          0.04          0.15         4.69589        
    ##  [2,] 0.03          0.09          0.58         4.76728        
    ##  [3,] 0.02          0.04          0.41         4.83825        
    ##  [4,] 0.02          0.07          0.38         4.96542        
    ##  [5,] 0.02          0.05          0.62         4.87065        
    ##  [6,] 0.01          0.05          0.53         4.82343        
    ##  [7,] 0.01          0.05          0.38         4.90328        
    ##  [8,] 0.02          0.05          0.37         4.89249        
    ##  [9,] 0.02          0.04          0.34         4.95437        
    ## [10,] 0.02          0.05          0.3          4.9856         
    ## [11,] 0.02          0.05          0.29         4.90585        
    ## [12,] 0.03          0.08          0.32         4.90693        
    ## [13,] 0.01          0.05          0.27         4.98272        
    ## [14,] 0.02          0.05          0.3          4.82748        
    ## [15,] 0.01          0.05          0.33         4.95673        
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

(Note that pomdp not run on state `0` since the probability collapses, hence 15 states are shown.)

``` r
df <- data.frame(states = states,
                 exact = states - exact_policy,
                 mdp = states - actions[mdp$policy],
                 pomdp = states - soln$policy)

plot(df$states, df$exact, type = "l", ylim = .6 * range(states), xlab = "state", ylab= "escapement" )
points(df$states, df$mdp, col = 1)
points(df$states, df$pomdp, col = 2, pch = 2)
```

![](/home/rstudio/Documents/code/cboettig/appl/inst/doc/fisheries-ex_files/figure-markdown_github/unnamed-chunk-11-1.png)

We can also confirm that policy has converged by comparing against a higher precision:

``` r
system.time( soln2 <- pomdp(transition, observation, reward, discount, mc.cores = 2, precision = 3, memory = 2000, timeout = 1000) )
```

    ##    user  system elapsed 
    ##  17.340   6.268  13.084

Check diagnostics again:

``` r
soln2$diagnostics
```

    ##       load_time_sec init_time_sec run_time_sec final_precision
    ##  [1,] 0.02          0.06          0.32         2.91585        
    ##  [2,] 0.03          0.11          1.32         2.97573        
    ##  [3,] 0.02          0.07          1.37         2.98653        
    ##  [4,] 0.02          0.07          1.17         2.97452        
    ##  [5,] 0.03          0.1           1.9          2.94671        
    ##  [6,] 0.02          0.06          1.6          2.99831        
    ##  [7,] 0.04          0.1           1.54         2.97876        
    ##  [8,] 0.03          0.1           1.32         2.9648         
    ##  [9,] 0.02          0.06          1.32         2.98551        
    ## [10,] 0.03          0.11          1.52         2.96894        
    ## [11,] 0.01          0.07          0.97         2.99167        
    ## [12,] 0.02          0.06          1.05         2.97207        
    ## [13,] 0.01          0.06          0.63         2.99438        
    ## [14,] 0.02          0.06          0.77         2.97625        
    ## [15,] 0.01          0.06          0.77         2.95794        
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

``` r
identical(df$pomdp, states - soln2$policy)
```

    ## [1] TRUE
