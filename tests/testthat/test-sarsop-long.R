context("longer tests")

test_that("we can run longer test successfully", {

  assert_testing()

states <- seq(0,1, length=50)
actions <- states
obs <- states
sigma_g <- 0.1
sigma_m <- 0.5
reward_fn <- function(x,h) pmin(x,h) # - .001*h
discount <- 0.95

r <- 1
K <- 0.75

f <- function(x, h){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}



m <- fisheries_matrices(states, actions, obs, reward_fn, f,
                        sigma_g, sigma_m, noise = "lognormal")

system.time({
alpha <- sarsop(m$transition, m$observation, m$reward,
                discount, precision = .1, timeout = 200, log_dir = ".")
})
df <- compute_policy(alpha, m$transition, m$observation, m$reward)



system.time({
  alpha <- sarsop(m$transition, m$observation, m$reward,
                  discount, precision = .1, timeout = 200, log_dir = ".")
})
df <- compute_policy(alpha, m$transition, m$observation, m$reward)



})
