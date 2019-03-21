
#' Matrices for the ecosystem services model
#'
#' initialize the transition, observation, and reward matrices
#'  given a transition function, reward function, and state space
#' @param S_0 total / maximum number of species possible
#' @param V value of the ecosystem services
#' @param C cost incurred in protecting the ecosystem
#' @param K number of critical species
#' @param E the expected number of extinctions per year
#' @param A number of possible actions. Default is 2 (all or nothing)
#' @param Q cost curvature, default 1. Q > 1 increasingly hard to protect all
#' @param discount the discount factor.
#' @param obs_prob probability of observing a species given that it is in the species pool
#' @return list of  transition matrix, observation matrix, and reward matrix
#' @importFrom stats dbinom dpois
#' @importFrom Matrix Matrix rowSums
#' @examples
#' m <- es_matrices()
#' @export
es_matrices <-
  function(S_0 = 40, # total/maximum number of species
             V = 90, # value of ES
             C = 80, # costs incurred by protection
             K = 10, # number of critical species
             E = 1,  # Expected number of extinctions per year
             A = 2,  # Number of possible actions
             Q = 1,  # cost curvature
      discount = 0.95,  # Discount factor
      obs_prob = 0.5
  ){

  states <- 0:S_0
  actions <- seq(0, 1, length = A)
  obs <- 0:(S_0-2)

  U <- utility_matrix(states, actions, V, C, K, Q)
  P <- transition_matrix(states, actions, K, E)
  transition <- list_matrix(P) ## Reshape things
  observation <- observation_matrix(states, actions, obs, prob_obs = obs_prob)
  list("transition" = transition, "observation" = observation, "reward" = U)
}

## turn 3D array to list of 2D matrices:
list_matrix <- function(P){
  A <- dim(P)[3]
  m <- dim(P)[1]
  P <- lapply(1:A, function(i) rowNorm(Matrix::Matrix( P[,,i]) ))
  transition <- array(dim = c(m, m, A))
  for(i in 1:A){
    transition[,,i] <- as.matrix(P[[i]])
  }
  transition
}

utility_matrix <- function(states, actions, V, C, K, Q){
  S_0 <- max(states)
  A <- length(actions)
  ## state space is 2*n: we have 1:n species, & either have all critical species or we not
  n = length(states)
  ss <- 1:(2*n)

  ### Calculate Transition Matrix (P) and Utility Matrix (U)
  m <- length(ss)
  U <- array(dim=c(m, A))
  for(i in 1:m){
    for(k in 1:A){
      U[i,k] <- ss_utility(ss[i], actions[k],
                           V, C, K, S_0, Q)
    }
  }
  U
}


transition_matrix <- function(states, actions, K, E){
  S_0 <- max(states)
  A <- length(actions)
  ## state space is 2*n: we have 1:n species, & either have all critical species or we not
  n = length(states)
  ss <- 1:(2*n)
  m <- length(ss)
  P <- array(dim=c(m, m, A))
  for(i in 1:m){
    for(k in 1:A){
      for(j in 1:m){
        P[i,j,k] <- f_ss(ss[i], ss[j], actions[k],
                   S_0, states, K, E)
      }
    }
  }
  P
}



# prob state -> next_state given action
prob_transition <- function(state, next_state, action,
                            E){
  ## Exactly one species is lost every time no action is taken
  ##next_state == pmax(state - (1 - action), 0)

  ## species gone are gone forever
  if(state == 0 & next_state == 0)
    return(1)

  ## Cannot gain species!
  if(state < next_state)
    return(0)

  n_lost <- max(state - next_state,0)

  ## Assumes all species have equal  prob extinction.
  ## Trivial since this becomes (1-E)^K
  # dbinom(n_lost, state, E)

  dpois(n_lost, E * (1 - action))


  ## Dee et al assumes deterministic loss rate:
  ##  as.numeric(n_lost == 1)

  ## Alternative forms
  ## fixed probabilities of losing 0, 1, or 2 species
  #  switch (as.character(n_lost),
  #    "0" = .10,
  #    "1" = .70,
  #    "2" = 0.10,
  #    "3" = 0.10,
  #    0)
}

# probability we have critical species after this transition
prob_critical <- function(state, next_state, action, K){
  ## Compared to the Dee et al formulation, this function
  ## has been generalized to transitions that lose multiple species
  n_lost <- max(state - next_state, 0)
  max( ((state - K) / state) ^ n_lost , 0)

}


## Observation matrix
observation_matrix <- function(states, actions, obs = states,
                               prob_obs = 0.5, tol = 1e-6){

  n_s <- length(states)
  n_o <- length(obs)


  obs1 <- array(0, dim = c(n_s, n_o))

  for(i in 1:n_s){
    #   obs1[i,] <- c(1, rep(0, n-1))
    ## if there are really Y species, and I have prob_obs of seeing each
    ## one, what is the probability I observe X species?
    obs1[i,] <- dbinom(obs, states[i], prob_obs)
  }


  obs1 <- rowNorm(obs1)
  zeros <- array(0,dim = c(n_s,n_o))
  obs_ss <- cbind(
    rbind(obs1, zeros),
    rbind(zeros, obs1))

  ## validation, with some tolerance
  stopifnot( sum( (rowSums(obs_ss) - 1)) < tol )  ## some tolerance

  obs <- array(dim = c(2*n_s,2*n_o,length(actions)))

  for(i in 1:length(actions)){
    obs[,,i] <- obs_ss
  }
  obs
}


## Define a 1-D state-space from all combinations
# of number of species + binary indicator of critical species present
parse_ss <- function(x, S_0){
  n <- S_0 + 1
  if(x <= n) return(list(s = x, r = TRUE))
  else return(list(s = x - n, r = FALSE))
}

ss_utility <- function(s_t, a_t, V, C, K, S_0, Q) {

  S <- parse_ss(s_t, S_0)
  if (a_t > 0) { ## "Protect"
    cost <- C * a_t ^ Q
    if (S$s < K) {
      return(- cost )
    } else {
      return(V * S$r - cost )
    }
  } else if (a_t == 0) {  ## Do nothing
    if (S$s < K) {
      return(0)
    } else {
      ## Do we get the benefit that year?
      #return(V * S$r)
      return((  (S$s - K) / S$s) * V * S$r )
    }
  }
}

## Transitions work on idices, not state values
f_ss <- function(ST, ST1, a_t,
                 S_0, states, K, E){
  #if(a_t == 1){
  #  return(as.numeric(ST == ST1))
  #} else {

    st = parse_ss(ST, S_0)
    st1 = parse_ss(ST1, S_0)

    P_trans <- prob_transition(states[st$s], states[st1$s], a_t, E)
    P_crit <- prob_critical(states[st$s], states[st1$s], a_t, K)

    if(st$r){ ## Currently have all critical species
      if(st1$r) ## keep critical species with prob
        out <- P_trans * P_crit
      else
        out <- P_trans * (1 - P_crit)
    } else {    ## Already lost a critical species
      if(st1$r) ## Cannot get them back
        out <- 0
      else ## Dynamics
        out <- P_trans
    }
    as.numeric(out)
  #}
}

rowNorm <- function(M){
  N <- Matrix::rowSums(Matrix::Matrix(M))
  i <- N != 0
  M[i,] <- M[i,] / N[i]
  M
}
