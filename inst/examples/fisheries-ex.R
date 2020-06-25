states <- 0:20
actions <- states
observed_states <- states
reward_fn <- function(x,h) pmin(x,h)
discount <- 0.95

# Re-scale a sigma = 0.1 for lognormal
sigma_g <- sqrt(log(1 + 0.1 / 6))
sigma_m <- sigma_g
f <- function(x, h, r = 1, K = 20){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}

m <- fisheries_matrices(states, actions, observed_states, reward_fn, f, sigma_g, sigma_m)


  ## Transition and Reward Matrices
  n_s <- length(states)
  n_a <- length(actions)
  n_z <- length(observed_states)

  transition <- array(0, dim = c(n_s, n_s, n_a))
  reward <- array(0, dim = c(n_s, n_a))
  observation <- array(0, dim = c(n_s, n_z, n_a))

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

  for (k in 1:n_a) {
    if(sigma_m <= 0){
      observation[, , k] <- diag(n_s)
    } else {
      for (i in 1:n_s) {
        if(states[i] <= 0){ ## treat observed 0 as real 0, (dlnorm cannot have log-mu of 0)
          observation[i, , k] <- c(1, rep(0, n_z - 1))
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
