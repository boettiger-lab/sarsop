K <- 150      # state space limit
states <- 0:K # Vector of all possible states
actions <- states   # Vector of actions: harvest

## Deterministic skeleton of recruitment function (for transistion matrix)
p <- c(6,0.05)
f <- function(x, h){
  A <- p[1]
  B <- p[2]
  s <- pmax(x-h, 0)
  A * s/(1 + B * s)
}

# Reward function
reward_fn <- function(x,h) {
  pmin(x,h)
}

## lognormal log-sd parameter
sigma_g <- 0.1


# Initialize
n_s <- length(states)
n_a <- length(actions)
transition <- array(0, dim = c(n_s, n_s, n_a))
reward <- array(0, dim = c(n_s, n_a))

# Fill in the transition and reward matrix, Looping over all states & actions
for (k in 1:n_s) {
  # Loop on all actions
  for (i in 1:n_a) {

    # Calculate the transition state at the next step, given the
    # current state k and the harvest actions[i]
    nextpop <- f(states[k], actions[i])
    if(nextpop <= 0)
      transition[k, , i] <- c(1, rep(0, n_s - 1))
    # Implement demographic stochasticity by drawing probability from a density function
    else if(sigma_g > 0){

      x <- dlnorm(states, log(nextpop), sdlog = sigma_g)    # transition probability densities
      N <- plnorm(states[n_s], log(nextpop), sigma_g)       # CDF accounts for prob density beyond boundary
      x <- x * N / sum(x)                                   # normalize densities to  = cdf(boundary)
      x[n_s] <- 1 - N + x[n_s]                              # pile remaining probability on boundary
      transition[k, , i] <- x                             # store as row of transition matrix

    } else {
     stop("sigma_g not > 0")
    }

    # Compute reward matrix
    reward[k, i] <- reward_fn(states[k], actions[i])

  } # end of action loop
} # end of state loop


# Discount factor
discount <- 0.95

## Solve! Policy-convergence is usually faster.  We care only about the policy converging when we aren't comparing expected values.
mdp <- mdp_policy_iteration(transition, reward, discount)
#mdp <- mdp_value_iteration(transition, reward, discount)


## MDP
plot(states, states - actions[mdp$policy], col="blue", lty=2)


## Exact / semi-analytic solution
fun <- function(x) - f(x,0) + x / discount
out <- optimize(f = fun, interval = c(0,K))
S_star <- out$minimum
S_star <- round(S_star) ## S* not on grid, so adjust

## Use S* to determine const-escapement policy fn:
exact_policy <- sapply(states,
                       function(x)
                         if(x < S_star) 0
                       else x - S_star)


## Compare to numeric solution
lines(states, states - exact_policy, col = "red")
testthat::expect_equal(actions[mdp$policy], exact_policy)




observed_states <- states
n_z = length(observed_states)

observation <- array(0, dim = c(n_s, n_z, n_a))
  for (k in 1:n_a) {
    if(sigma_m <= 0){
      observation[, , k] <- diag(n_s)
    } else {
      for (i in 1:n_s) {
        x <- dlnorm(observed_states, log(states[i]), sdlog = sigma_m)    # transition probability densities

        ## Normalize using CDF
        N <- plnorm(observed_states[n_s], log(states[i]), sigma_m)       # CDF accounts for prob density beyond boundary
        x <- x * N / sum(x)                                   # normalize densities to  = cdf(boundary)
        x[n_s] <- 1 - N + x[n_s]                              # pile remaining probability on boundary
        observation[i, , k] <- x                             # store as row of transition matrix

      }
    }
  }




## Emission matrix function
create_O <- function(Num_s,Num_a,Num_z,sigma_m){

  O = array(0, dim = c(Num_s, Num_z, Num_a))

  if(sigma_m == 0){
    for (j in 1:Num_a){
      O[ , ,j] = diag(Num_s)
    }
  } else {
    for (i in 1:Num_s){
      for (j in 1:Num_a){
        for (ii in 1:Num_z){
          if(i == 1){
            O[i,1,j] = 1  ## No fish means really no fish?
          } else {
            O[i,ii,j] = dlnorm(ii / i, log(1), sigma_m)
          }
        }
      }
    }
  }

  for (i in 1:dim(O)[1]) {
    for (j in 1:dim(O)[3]) {
      O[i, , j] = normalize(O[i, , j])
    }
  }
  for(i in 1:dim(O)[3]){
    O[,,i] = round_Milad(O[,,i])
  }

  output <- O
}































# Hmmmm...
N <- plnorm(states[K+1], log(nextpop), sigma_g)
x <- dlnorm(states, log(nextpop), sdlog = sigma_g)
x <- x * N / sum(x)
x[K+1] <- 1 - N + x[K+1]
sum(x)
y <- x

fine_states <- seq(min(states), 20 * max(states), by = states[2]-states[1])
N <- sum(dlnorm(fine_states, log(nextpop), sdlog = sigma_g))
x <- dlnorm(states, log(nextpop), sdlog = sigma_g) / N
x[K+1] <- max(1 - sum(x[-(K+1)]), 0)

library(ggplot2)
df = data.frame(s = seq_along(x), x = x, y = y)
ggplot(df) + geom_line(aes(s,x)) + geom_line(aes(s,y), col='red')
