## Theoretically the observation can depend on the action.  So to initialize we must either specify an initial
## action, and then use it to make an observation of the state, or we must specify an intial observed state.
## Note that in our fisheries example (and many others), observation process is independent of the action
##  and any action could have been chosen.

#' sim_pomdp
#'
#' sim_pomdp
#' @param transition Transition matrix, dimension n_s x n_s x n_a
#' @param observation Observation matrix, dimension n_s x n_z x n_a
#' @param reward Reward matrix, dimension n_s x n_a
#' @param discount the discount factor
#' @param policy the policy to be simulated. Should be a vector of length n_s where
#'  the ith element gives the action (index of the action) for an observation of the ith state,
#'  (i.e. as returned by pomdp or appl functions)
#' @param x0 initial state
#' @param Tmax duration of simulation
#' @param a0 initial action (default is action 1, e.g. can be arbitrary
#' if the observation process is independent of the action taken)
#' @details simulation assumes the following order of updating: For system in state[t] at
#' time t, an observation of the system obs[t] is made, and then action[t] is based on that
#' observation and the given policy, returning (discounted) reward[t].
#' @return a data frame with columns for time, state, obs, action, and (discounted) value.
#' @export
sim_pomdp <- function(transition, observation, reward, discount, policy, x0, Tmax, a0 = 1){
  state <- action <- value <- obs <- numeric(Tmax+1)
  n_s <- dim(transition)[1]
  n_z <- dim(observation)[2]
  state[1] <- x0
  state[2] <- x0
  action[1] <- a0
  # assume observation is based on the action of the previous year, if it does depend on action

  for(t in 2:(Tmax+1)){
    obs_prob <- observation[state[t], , action[t-1]]
    obs[t] <- sample(1:n_z, 1, prob = obs_prob)
    action[t] <- policy[obs[t]]
    value[t] <- reward[state[t], action[t]] * discount^(t-1)
    trans_prob <- transition[state[t], , action[t]]
    state[t+1] <- sample(1:n_s, 1, prob = trans_prob)


  }

  df <- data.frame(time = 0:Tmax, state = state[1:(Tmax+1)], obs, action, value)
  df[2:(Tmax+1),] ## ignore year 0, which is just a reference year so obs[1] can be made
}
