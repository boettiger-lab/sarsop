
states <- 0:100
actions <- states
obs <- states
sigma_g <- 0.1
sigma_m <- 0.1
reward_fn <- function(x,h) pmin(x,h) # - .001*h
discount <- 0.95

r <- 1
K <- 15
f <- function(x, h){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}



m <- fisheries_matrices(states, actions, obs, reward_fn, f, sigma_g, sigma_m, noise = "lognormal")
alpha <- sarsop(m$transition, m$observation, m$reward, discount, precision = 10)
df <- compute_policy(alpha, m$transition, m$observation, m$reward)
#ggplot(df, aes(states[state], states[state] - actions[policy])) + geom_line() + geom_point()

#m <- fisheries_matrices(states, actions, obs, reward_fn, f, sigma_g, sigma_m, noise = "uniform")
#alpha <- sarsop(m$transition, m$observation, m$reward, discount, precision = .05)
#df <- compute_policy(alpha, m$transition, m$observation, m$reward)
#ggplot(df, aes(states[state], states[state] - actions[policy])) + geom_line() + geom_point()


sim <- sim_pomdp(m$transition, m$observation, m$reward, discount = discount,
                      x0 = 15, Tmax = 20, alpha = alpha)


## Check logging works
log <- tempdir()

id <- "uuid"
id

log_data <- data.frame(id = id, model = "ricker", r = r, K = K, C = NA, sigma_g = sigma_g, sigma_m = sigma_m)
alpha <- sarsop(m$transition, m$observation, m$reward, discount, precision = .1,
                log_dir = log, log_data = log_data)

## Query by id, making sure we get the model results we just ran
meta <- meta_from_log(parameters = data.frame(id = id), log_dir = log)[1,]

## Query by parameter values, getting all results from library that match the desired conditions
meta <- meta_from_log(parameters = data.frame(model = "ricker", r = r), log_dir = log)[1,]

## Note, these return a list since meta may have multiple models
stored_alpha <- alphas_from_log(meta, log_dir = log)
stored_model <- models_from_log(meta)


testthat::expect_identical(alpha, stored_alpha[[1]])


testthat::expect_equivalent(m, stored_model[[1]])

stored_fs <- f_from_log(meta)
testthat::expect_equivalent(stored_fs[[1]], f)

unlink(paste(log, list.files(log), sep="/"))
