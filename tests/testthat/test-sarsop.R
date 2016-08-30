
states <- 0:20
actions <- states
obs <- states
sigma_g <- 0.1
sigma_m <- sigma_g
reward_fn <- function(x,h) pmin(x,h)
discount <- 0.95

r <- 0.5
K <- 15
f <- function(x, h){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}

m <- fisheries_matrices(states, actions, obs, reward_fn, f, sigma_g, sigma_m)

alpha <- sarsop(m$transition, m$observation, m$reward, discount, precision = .1)

df <- compute_policy(alpha, m$transition, m$observation, m$reward)

sim <- sim_pomdp(m$transition, m$observation, m$reward, discount,
                      x0 = 5, Tmax = 20, alpha = alpha)


## Check logging works
log <- tempdir()
log_data <- data.frame(model = "ricker", r = r, K = K, C = NA, sigma_g = sigma_g, sigma_m = sigma_m)
alpha <- sarsop(m$transition, m$observation, m$reward, discount, precision = .1,
                log_dir = log, log_data = log_data)

meta <- meta_from_log(parameters = data.frame(model = "ricker", r = r), log_dir = log)[1,]

## Note, these return a list since meta may have multiple models
stored_alpha <- alphas_from_log(meta, log_dir = log)
stored_model <- models_from_log(meta)


testthat::expect_identical(alpha, stored_alpha[[1]])
testthat::expect_identical(m, stored_model[[1]])

stored_fs <- f_from_log(meta)
sapply(stored_fs, testthat::expect_is, "function")
