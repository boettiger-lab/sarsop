#' pomdp
#'
#' pomdp
#' @param T Transition matrix, dimension n_s x n_s x n_a
#' @param O Observation matrix, dimension n_s x n_z x n_a
#' @param R Reward matrix, dimension n_s x n_a
#' @param GAMMA the discount factor
#' @param initial initial belief state, optional, defaults to uniform over states
#' @return optimal value and corresponding policy
#' @details Dimensions are given as number of states (n_s), number of observed states n_z, number of actions n_a
#' @export
pomdp <- function(T, O, R, GAMMA, initial = NULL){

  Num_s <- dim(O)[1]
  Num_z <- dim(O)[2]
  Num_a <- dim(O)[3]

  if(is.null(initial))
    initial <- rep(1/Num_s, Num_s)

  out = vector("numeric", length = Num_z)
  value = vector("numeric", length = Num_z)
  policy = vector("numeric", length = Num_z)

  ## Consider moving into write_pomdp. No need to support semantically named actions(?)
  actions = paste0("a", 1:Num_a)

  ## fixme: why 800?  this should probably be inside write_pomdp anyhow
  XX = paste0("a", 1:800)

  for (i in 1:Num_z) {
    belief = initial * t(O[, i, 1])

    ## Consider more robust normalization.  Check write-out precision in write_pomdp
    belief = normalize(belief)
    belief = round(belief,4) / sum(round(belief,4))

    ## Why test this?  Better syntax would be: if(any(is.nan(beleif)))
    if(is.nan(sum(belief))){
      value[i] = 0
      policy[i] = 0
    } else {

      ## function is basically just these three lines.  Consider arguments to pomdpsol being top-level arguments
      appl::write_pomdp(T, O, R, GAMMA, belief, Num_s, Num_a, Num_z, actions, XX)
      appl::pomdpsol("input.pomdp", "output.policy", precision = 1, timeout = 25, stdout = FALSE)
      out = read_policy(belief, file = "output.policy")

      value[i] = out[[1]]
      policy[i] = out[[2]]
    }
  }

  list(value, policy)
}





normalize <- function(A){
  z = sum(A)
  s = z + (z==0)
  A / s
}
