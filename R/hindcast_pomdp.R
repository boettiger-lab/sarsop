#' hindcast_pomdp
#'
#' Compare historical actions to what pomdp recommendation would have been.
#' @inheritParams sarsop
#' @inheritParams compute_policy
#' @param obs a given sequence of observations
#' @param action the corresponding sequence of actions
#' @return a list, containing: a data frame with columns for time, obs, action, and optimal action,
#' and an array containing the posterior belief distribution at each time t
#' @export
#' @examples
#' m <- fisheries_matrices()
#' \donttest{ ## Takes > 5s
#' if(assert_has_appl()){
#' alpha <- sarsop(m$transition, m$observation, m$reward, 0.95, precision = 10)
#' sim <- hindcast_pomdp(m$transition, m$observation, m$reward, 0.95,
#'                      obs = rnorm(21, 15, .1), action = rep(1, 21),
#'                      alpha = alpha)
#'
#' }}
#' @aliases hindcast_pomdp compare_pomdp
hindcast_pomdp <- function(transition, observation, reward, discount, obs, action,
                      state_prior = rep(1, dim(observation)[[1]]) / dim(observation)[[1]],
                      alpha = NULL, ...){

    Tmax <- length(obs)
    n_states <- dim(observation)[1]
    optimal <- numeric(Tmax)
    optimal[1] <- NA
    state_posterior <- array(NA, dim = c(Tmax, n_states))
    if(is.null(state_prior))  state_prior <- rep(1, n_states) / n_states
    state_posterior[1,] <- state_prior

    for(t in 2:Tmax){
      out <- compute_policy(alpha, transition, observation, reward, state_posterior[t-1,], action[t-1])
      optimal[t] <- out$policy[obs[t]]
      state_posterior[t,] <- update_belief(state_posterior[t-1,], transition, observation, obs[t], action[t-1])
    }

    list(df = data.frame(time = 1:Tmax, obs, action, optimal),
         state_posterior = as.data.frame(state_posterior))
  }


compare_pomdp <- hindcast_pomdp
