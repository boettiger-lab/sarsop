library("MDPtoolbox")

## MDP Problem definition
states <- 1:30
actions <- states
f <- function(x, h, r = 1, K = 25){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}
sigma_g <- 0.1
reward_fn <- function(x,h) pmin(x,h)
discount <- 0.95

# Generate Matrices
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


## Numerical SDP Solution
mdp <- MDPtoolbox::mdp_policy_iteration(transition, reward, discount)

## Plot Solution
plot(states, states - actions[mdp$policy], col="blue", lty=2)


## Exact / semi-analytic solution
fun <- function(x) - f(x,0) + x / discount
out <- optimize(f = fun, interval = c(min(states),max(states)))
S_star <- round(out$minimum)
exact_policy <- sapply(states, function(x) if(x < S_star) 0 else x - S_star)

lines(states, states - exact_policy, col = "red")
testthat::expect_equal(actions[mdp$policy], exact_policy)


## POMDP
sigma_m = 0.1
observed_states <- states
n_z = length(observed_states)

observation <- array(0, dim = c(n_s, n_z, n_a))
  for (k in 1:n_a) {
    if(sigma_m <= 0){
      observation[, , k] <- diag(n_s)
    } else {
      for (i in 1:n_s) {
        x <- dlnorm(observed_states, log(states[i]), sdlog = sigma_m)    # transition probability densities

        ## Normalize using CDF
        N <- plnorm(observed_states[n_s], log(states[i]), sigma_m)       # CDF accounts for prob density beyond boundary
        x <- x * N / sum(x)                                   # normalize densities to  = cdf(boundary)
        x[n_s] <- 1 - N + x[n_s]                              # pile remaining probability on boundary
        observation[i, , k] <- x                             # store as row of transition matrix

      }
    }
  }

#pomdp(transition, observation, reward, discount)


