#' compute_policy
#'
#' Derive the corresponding policy function from the alpha vectors
#' @inheritParams sarsop
#' @param alpha the matrix of alpha vectors returned by \code{\link{sarsop}}
#' @param a_0 previous action. Belief in state depends not only on observation, but on prior belief of the state and subsequent action that had been taken.
#' @return a data frame providing the optimal policy (choice of action) and corresponding value of the action for each possible belief state
#' @importMethodsFrom Matrix %*%
#' @export
#' @examples
#'
#' m <- fisheries_matrices()
#' \donttest{ ## Takes > 5s
#' if(assert_has_appl()){
#' alpha <- sarsop(m$transition, m$observation, m$reward, 0.95, precision = 10)
#' compute_policy(alpha, m$transition, m$observation, m$reward)
#' }
#' }
#'
compute_policy <- function(alpha, transition, observation, reward,
                           state_prior =  rep(1, dim(observation)[[1]]) / dim(observation)[[1]], a_0 = 1){

  n_states <- dim(observation)[[1]]
  n_obs <- dim(observation)[[2]]
  n_actions <- dim(observation)[[3]]

  ## n_states x n_obs array
  belief <- vapply(1:n_obs,
                   function(i){
                     b <- state_prior %*% transition[, , a_0] * observation[, i, a_0]
                     if(sum(b) == 0) numeric(n_states) ## observed state i is impossible
                     else b / sum(b)
                   },
                   numeric(n_states))

  # Sum over alpha vectors
  A <- t(belief) %*% alpha$vectors

  ## Determine value and policy
  value <- apply(A, 1, max)
  policy <- apply(A, 1, function(x) alpha$action[which.max(x)])

  data.frame(policy, value, state = 1:n_obs)
}
