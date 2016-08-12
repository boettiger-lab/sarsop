#' sim_pomdp
#'
#' @param transition Transition matrix, dimension n_s x n_s x n_a
#' @param observation Observation matrix, dimension n_s x n_z x n_a
#' @param reward Reward matrix, dimension n_s x n_a
#' @param discount the discount factor
#' @param policy the policy to be simulated. Should be a vector of length n_s where
#'  the ith element gives the action (index of the action) for an observation of the ith state,
#'  (i.e. as returned by pomdp or appl functions)
#' @param x0 initial state
#' @param a0 initial action (default is action 1, e.g. can be arbitrary
#' if the observation process is independent of the action taken)
#' @param Tmax duration of simulation
#'
#' @details simulation assumes the following order of updating: For system in state[t] at
#' time t, an observation of the system obs[t] is made, and then action[t] is based on that
#' observation and the given policy, returning (discounted) reward[t].
#' @return a data frame with columns for time, state, obs, action, and (discounted) value.
#' @export
#' @examples
#' \dontrun{ ## Takes > 5s
#' ## Use example code to generate matrices for pomdp problem:
#' source(system.file("examples/fisheries-ex.R", package = "appl"))
#' alpha <- sarsop(transition, observation, reward, discount, precision = 10)
#' sim <- sim_pomdp(transition, observation, reward, discount,
#'                      x0 = 5, Tmax = 20, alpha = alpha)

#'
#' }
#'
sim_pomdp <- function(transition, observation, reward, discount, state_prior = NULL,
                      x0, a0 = 1, Tmax = 20,
                      alpha = NULL, verbose = TRUE, mc.cores = 1L, ...){

    n_states <- dim(observation)[1]
    n_obs <- dim(observation)[2]
    value <- obs <- action <- state <- numeric(Tmax+1)
    state_posterior <- array(NA, dim = c(Tmax+1, n_states))
    if(is.null(state_prior))  state_prior <- rep(1, n_states) / n_states
    state[2] <- x0
    action[1] <- a0  # only relevant if action influences observation process
    state_posterior[2,] <- state_prior

    if(is.null(alpha)){
      if(verbose) message("alpha not provided, recomputing them from SARSOP algorithm at each time step. This can be very slow!")
      update_alpha <- TRUE
    }

    for(t in 2:Tmax){
      if(update_alpha) alpha <- run_pomdp(transition, observeration, reward, discount, state_posterior[t,], verbose, mc.cores, ...)
      out <- compute_policy(alpha, models, model_posterior[t,], state_posterior[t,], action[t-1])
      obs[t] <- sample(1:n_obs, 1, prob = true_observation[state[t], , action[t-1]])
      action[t] <- out$policy[obs[t]]
      value[t] <- true_utility[state[t], action[t]] * discount^(t-1)
      state[t+1] <- sample(1:n_states, 1, prob = true_transition[state[t], , action[t]])
      state_posterior[t+1,] <- update_belief(state_posterior[t,], model_posterior[t,], models, obs[t], action[t-1])
    }
    df <- data.frame(time = 0:Tmax, state, obs, action, value)[2:Tmax,]
    list(df = df, model_posterior = model_posterior[2:(Tmax+1),], state_posterior = state_posterior[2:(Tmax+1),])
  }


  update_belief <- function(state_prior, transition, observation, z0, a0){
    belief <-
      vapply(1:length(state_prior), function(i){
            state_prior %*% transition[, i, a0] * observation[i, z0, a0]
      }, numeric(1))
    belief / sum(belief)
  }


