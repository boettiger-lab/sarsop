#' simulate a POMDP
#'
#' Simulate a POMDP given the appropriate matrices.
#' @inheritParams sarsop
#' @inheritParams compute_policy
#' @param x0 initial state
#' @param a0 initial action (default is action 1, e.g. can be arbitrary
#' if the observation process is independent of the action taken)
#' @param Tmax duration of simulation
#' @param policy Simulate using a pre-computed policy (e.g. MDP policy) instead of POMDP
#' @param reps number of replicate simulations to compute
#' @param ... additional arguments to mclapply
#' @details simulation assumes the following order of updating: For system in state[t] at
#' time t, an observation of the system obs[t] is made, and then action[t] is based on that
#' observation and the given policy, returning (discounted) reward[t].
#' @return a data frame with columns for time, state, obs, action, and (discounted) value.
#' @export
#' @importFrom parallel mclapply
#' @examples
#' m <- fisheries_matrices()
#' discount <- 0.95
#' \donttest{ ## Takes > 5s
#' if(assert_has_appl()){
#' alpha <- sarsop(m$transition, m$observation, m$reward, discount, precision = 10)
#' sim <- sim_pomdp(m$transition, m$observation, m$reward, discount,
#'                  x0 = 5, Tmax = 20, alpha = alpha)
#'
#' }}
#'
sim_pomdp <- function(transition, observation, reward, discount,
                      state_prior = rep(1, dim(observation)[[1]]) / dim(observation)[[1]],
                      x0, a0 = 1, Tmax = 20, policy = NULL,
                      alpha = NULL, reps = 1, ...){
  ## permit parallelized replicates
  if(reps > 1){
    sims <- parallel::mclapply(1:reps,
                    function(i){
                      sim <- sim_pomdp_1(transition, observation, reward, discount,
                                  state_prior, x0, a0, Tmax, policy, alpha)
                      sim$df$rep <- i
                      # cbind(sim$state_posterior, i) ## matrix, rep not explicit
                      sim
                    },
                    ...)
    list(             df = do.call(rbind, lapply(sims, `[[`, "df")),
         state_posterior = do.call(rbind, lapply(sims, `[[`, "state_posterior")))
  } else {
    sim_pomdp_1(transition, observation, reward, discount,
                state_prior, x0, a0, Tmax, policy, alpha)
  }
}

  sim_pomdp_1 <- function(transition, observation, reward, discount,
                        state_prior = rep(1, dim(observation)[[1]]) / dim(observation)[[1]],
                        x0, a0 = 1, Tmax = 20, policy = NULL,
                        alpha = NULL, ...){
    n_states <- dim(observation)[1]
    n_obs <- dim(observation)[2]

    value <- obs <- action <- state <- numeric(Tmax+1)
    state_posterior <- array(NA, dim = c(Tmax+1, n_states))
    if(is.null(state_prior))  state_prior <- rep(1, n_states) / n_states
    state[2] <- x0
    action[1] <- a0
    state_posterior[2,] <- state_prior



    for(t in 2:Tmax){

      ## FIXME compute_policy solves for all possible observations; faster if we solved policy just for obs[t]
      ## simulate an MDP policy
      if(!is.null(alpha) && is.null(policy)){
        out <- compute_policy(alpha, transition, observation, reward, state_posterior[t,], action[t-1])
      } else {
        out <- list(policy = policy)
      }

      obs[t] <- sample(1:n_obs, 1, prob = observation[state[t], , action[t-1]])
      action[t] <- out$policy[obs[t]]
      value[t] <- reward[state[t], action[t]] * discount^(t-1)
      state[t+1] <- sample(1:n_states, 1, prob = transition[state[t], , action[t]])
      state_posterior[t+1,] <- update_belief(state_posterior[t,], transition, observation, obs[t], action[t-1])
    }
    df <- data.frame(time = 0:Tmax, state, obs, action, value)[2:Tmax,]
    list(df = df, state_posterior = state_posterior[2:(Tmax+1),])
  }

  ## FIXME compute_policy duplicates this code, but for all possible observations.  Avoid code duplication.
  update_belief <- function(state_prior, transition, observation, z0, a0){
    belief <-
      vapply(1:length(state_prior), function(i){
            state_prior %*% transition[, i, a0] * observation[i, z0, a0]
      }, numeric(1))
    belief / sum(belief)
  }


