#' compute_policy
#'
#' @inheritParams sarsop
#' @param alpha the matrix of alpha vectors returned by \code{\link{sarsop}}
#' @param a_0 previous action. Belief in state depends not only on observation, but on prior belief of the state and subsequent action that had been taken.
#' @return a data frame providing the optimal policy (choice of action) and corresponding value of the action for each possible belief state
#' @export
#' @examples
#' \dontrun{ ## Takes > 5s
#' ## Use example code to generate matrices for pomdp problem:
#' source(system.file("examples/fisheries-ex.R", package = "appl"))
#' alpha <- sarsop(transition, observation, reward, discount, precision = 10)
#' compute_policy(alpha, transition, observation, reward)
#' }
#'
compute_policy <- function(alpha, transition, observation, reward,
                           state_prior =  rep(1, dim(observation)[[1]]) / dim(observation)[[1]], a_0 = 1){

  n_states <- dim(observation)[[1]]
  n_obs <- dim(observation)[[2]]
  n_actions <- dim(observation)[[3]]

  belief <- vapply(1:n_obs,
                   function(i){
                     b <- state_prior %*% transition[, , a_0] * observation[, i, a_0]
                     if(sum(b) == 0) numeric(n_states) ## observed state i is impossible
                     else b / sum(b)
                   },
                   numeric(n_states))

  V <- t(belief) %*% alpha
  value <- apply(V, 1, max)
  policy <- apply(V, 1, function(x) which.max(x))
  data.frame(policy, value, state = 1:n_obs)
}
