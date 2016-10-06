ricker <- function(x, h, r = .1, K = 20){
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
#' @param sigma_g log-sd for log-normal shock to f or half-width of uniform shock
#' @param sigma_m log-sd for log-normal measurement y of state x, or half-width of of uniform
#' @param type distribution for noise, "lognormal" or "uniform"
#' @return list of transitition matrix, observation matrix, and reward matrix
#' @details assumes log-normally distributed observation errors and process errors
#' @importFrom stats dlnorm plnorm dunif punif
#' @export
fisheries_matrices <- function(states = 0:23,
         actions = states,
         observed_states = states,
         reward_fn = function(x,a) pmin(x,a),
         f = ricker,
         sigma_g = 0.1,
         sigma_m = sigma_g,
         type = c("lognormal", "uniform")){

  type <- match.arg(type)
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
      if(nextpop <= 0){
        transition[k, , i] <- c(1, rep(0, n_s - 1))
      } else if(sigma_g > 0){
        transition[k, , i] <- prob(states, nextpop, sigma_g)
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
          observation[i, , k] <- prob(observed_states, states[i], sigma_m)
        }
      }
    }
  }
  list(transition = transition, observation = observation, reward = reward)
}

prob <- function(states, mu, sigma, type = "lognormal"){
  n_s <- length(states)
  if(type == "lognormal"){
    x <- dlnorm(states, log(mu), sdlog = sigma)
    N <- plnorm(states[n_s], log(mu), sigma)
  } else if(type == "uniform"){
    x <- dunif(states, mu + sigma, mu - sigma)
    N <- punif(states[n_s], mu + sigma, mu - sigma)
  }
  if(sum(x) == 0){  ## nextpop is computationally zero, would create NAs
    x <- c(1, rep(0, n_s - 1))
  } else { ## Normalize
    x <- x * N / sum(x)         # normalize densities to  = cdf(boundary)
    x[n_s] <- 1 - N + x[n_s]    # pile remaining probability on boundary
  }
  x
}
