ricker <- function(x, h, r = 1, K = 20){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}

# FIXME consider generating these matrices from a NIMBLE style model declaration (plus reward function)

#' fisheries_matrices
#'
#' initialize the transition, observation, and reward matrices given a transition function, reward function, and state space
#' @param states sequence of possible states
#' @param actions sequence of possible actions
#' @param observed_states sequence of possible observations
#' @param reward_fn function of x and a that gives reward for tacking action a when state is x
#' @param f transition function of state x and action a.
#' @param sigma_g log-sd for log-normal shock to f
#' @param sigma_m log-sd for log-normal measurement y of state x
#' @return list of transitition matrix, observation matrix, and reward matrix
#' @details assumes log-normally distributed observation errors and process errors
#' @export
fisheries_matrices <- function(states = 0:23,
         actions = states,
         observed_states = states,
         reward_fn = function(x,a) pmin(x,a),
         f = ricker,
         sigma_g = 0.3,
         sigma_m = sigma_g){

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
  list(transition = transition, observation = observation, reward = reward)
}
