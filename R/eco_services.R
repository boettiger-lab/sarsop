# FIXME consider generating these matrices from a NIMBLE style model declaration (plus reward function)

#' fisheries_matrices
#'
#' initialize the transition, observation, and reward matrices given a transition function, reward function, and state space
#' @param states sequence of possible states
#' @param actions sequence of possible actions
#' @param observed_states sequence of possible observations
#' @param reward_fn function of x and a that gives reward for tacking action a when state is x

#' @export
es_matrices <-
  function(states = 0:20,
           actions = states,
           observed_states = states,
           reward_fn = function(x,a) pmin(x,a),
           f = ricker(1,15),
           sigma_g = 0.1,
           sigma_m = 0.1,
           noise = c("rescaled-lognormal", "lognormal", "uniform", "normal")
           ){

  noise <- match.arg(noise)
  n_s <- length(states)
  n_a <- length(actions)
  n_z <- length(observed_states)
  transition <- array(0, dim = c(n_s, n_s, n_a))
  reward <- array(0, dim = c(n_s, n_a))
  observation <- array(0, dim = c(n_s, n_z, n_a))


  list(transition = transition, observation = observation, reward = reward)
}






# prob state -> next_state given action
prob_transition <- function(state, next_state, action){
  ## Exactly one species is lost every time no action is taken
  ##next_state == pmax(state - (1 - action), 0)

  ## no protection means no losses
  if(action == 1)
    return(as.numeric(state == next_state))

  ## species gone are gone forever
  if(state == 0 & next_state == 0)
    return(1)

  ## Cannot gain species!
  if(state < next_state)
    return(0)

  n_lost <- max(state - next_state,0)

  # dbinom(n_lost, state, E)  ## Assumes all species have equal prob extinction.  Trivial since this becomes (1-E)^K
  dpois(n_lost, E)

  ## Dee et al assumes deterministic loss rate
  #as.numeric(n_lost == 1)  ## same as deterministic loss of 1 speices per time

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
prob_critical <- function(state, next_state, action){
  ## Compared to the Dee et al formulation, this function
  ## has been generalized to transitions that lose multiple species
  n_lost <- max(state - next_state, 0)
  max( ((state - K) / state) ^ n_lost , 0)

}





## state space is 2*n: we have 1:n species, & either have all critical species or we not
n = length(states)
ss <- 1:(2*n)
parse_ss <- function(x){
  if(x <= n) return(list(s = x, r = TRUE))
  else return(list(s = x - n, r = FALSE))
}

### Calculate Transition Matrix (P) and Utility Matrix (U)

  m = length(ss)
  P <- array(dim=c(m,m,2))
  U <- array(dim=c(m,2))
  for(i in 1:m){
    for(k in 1:length(actions)){
      U[i,k] <- ss_utility(ss[i], actions[k], V, C, K)
      for(j in 1:m){
        P[i,j,k] <- f_ss(ss[i], ss[j], actions[k])
      }
    }
  }



  P <- list(rowNorm( Matrix(P[,,1]) ), rowNorm( Matrix(P[,,2])))

  tran <- array(dim = c(m,m,2))
  for(i in 1:length(actions)){
    tran[,,i] <- as.matrix(P[[i]])
  }

state_prior = rep(1, m) / m # initial belief





## Observation matrix
observation_matrix <- function(n_states, n_actions, prob_obs = 0.5){

  n <- n_states
  m <- n_actions
  obs1 <- array(0,dim = c(n,n))

  for(i in 1:n){
    #   obs1[i,] <- c(1, rep(0, n-1))
    obs1[,i] <- dbinom(states, states[i]+10, prob_obs)
  }

  obs1 <- rowNorm(obs1)
  zeros <- array(0,dim = c(n,n))
  obs_ss <- cbind(
    rbind(obs1, zeros),
    rbind(zeros, obs1))
  stopifnot( sum( (rowSums(obs_ss) - 1)) < 1e-9 )  ## some tolerance

  obs <- array(dim = c(m,m,2))
  for(i in 1:length(actions)){
    obs[,,i] <- obs_ss
  }
  obs
}


## Define a 1-D state-space from all combinations of # of species + binary indicator of critical species present

ss_utility <- function(s_t, a_t, V, C, K) {

  S <- parse_ss(s_t)
  if (a_t == 1) { ## "Protect"
    if (S$s < K) {
      return(-C)
    } else {
      return(V * S$r - C)
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
f_ss <- function(ST, ST1, a_t){
  if(a_t == 1){
    return(as.numeric(ST == ST1))
  } else {

    st = parse_ss(ST)
    st1 = parse_ss(ST1)

    P_trans <- prob_transition(states[st$s], states[st1$s], a_t)
    P_crit <- prob_critical(states[st$s], states[st1$s], a_t)

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
  }
}

rowNorm <- function(M){
  N <- rowSums(M)
  M / N
}
