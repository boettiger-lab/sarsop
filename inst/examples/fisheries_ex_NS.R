states <- 0:20
actions <- states
discount <- 0.95
temp <- seq(10,80,by = 5) # changes in temperature

# Re-scale a sigma = 0.5 for lognormal
sigma_g <- sqrt(log(1 + 0.5 / 6))
sigma_m <- sigma_g
f <- function(x, h, r = 1, K = 20){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}
climate_f <- function(x){
  K <- round((-0.01*x^2) + (0.9 * x) - 4)
  K
}
reward_fn <- function(x,h) pmin(x,h)

n_s <- length(states)
n_a <- length(actions)
n_fs <- length(temp)
K_T <- climate_f(temp) # correspondence between capacity and temperature

# Transition of partially observable states

transition_par <- array(0,dim = c(n_s,n_s,n_a,n_fs))

for(i in 1:n_fs){
  for(j in 1:n_s){
    for(k in 1:n_a){
      nextpop <- f(states[j],actions[k],r = 1,K_T[i])
      if(nextpop <= 0){
        transition_par[j,,k,i] <- c(1,rep(0, n_s - 1))
        } else if(sigma_g > 0){
          x <- dlnorm(states, log(nextpop), sdlog = sigma_g)    # transition probability densities
          N <- plnorm(states[n_s], log(nextpop), sigma_g)       # CDF accounts for prob density beyond boundary
          x <- x * N / sum(x)                                   # normalize densities to  = cdf(boundary)
          x[n_s] <- 1 - N + x[n_s]                              # pile remaining probability on boundary
          transition_par[j, ,k,i] <- x                              # store as row of transition matrix
        } else{
          stop("sigma_g not > 0")
        }
      
    }
  }
}


# Transition of fully observable states

transition_full <- array(0,dim = c(n_fs,n_fs,n_a))
states_full <- 1:n_fs
for(i in 1:n_a){
  for(j in 1:n_fs){
    x <- dlnorm(states_full,log(states_full[j]), sdlog = sigma_g)
    N <- plnorm(states_full[n_fs], log(states_full[j]), sigma_g)
    x <- x * N / sum(x)
    x[n_fs] <- 1 - N + x[n_fs]
    transition_full[j,,i] <- x
  }
}

# belief of the fully observable state
initial_cond = 8 # index of initial temperature
b_full = array(0,dim = n_fs)
b_full[initial_cond] = 1

# Reward

reward <- array(0, dim = c(n_s,n_a,n_fs))

for(i in 1:n_fs){
  for(j in 1:n_s){
    for(k in 1:n_a){
      reward[j,k,i] <- reward_fn(states[j],actions[k])
    }
  }
}

# Emission

observations <- states
n_z = length(observations)

emission <- array(0,dim = c(n_s,n_z,n_a,n_fs))

for(i in 1:n_fs){
  for(j in 1:n_a){
    if(sigma_m <= 0){
      emission[,,j,i] <- diag(n_s)
    } else{
      for(k in 1:n_s){
        if(states[k] <= 0){
          x <- dlnorm(observations, -1, sigma_m)
          emission[k, , j,i] <- x / sum(x)
        } else{
          x <- dlnorm(observations, log(states[k]), sdlog = sigma_m)    # transition probability densities
          ## Normalize using CDF
          N <- plnorm(observations[n_s], log(states[k]), sigma_m)       # CDF accounts for prob density beyond boundary
          x <- x * N / sum(x)                                           # normalize densities to  = cdf(boundary)
          x[n_s] <- 1 - N + x[n_s]                                      # pile remaining probability on boundary
          emission[k, , j,i] <- x                                      # store as row of transition matrix
        }
      }
    }
  }
}