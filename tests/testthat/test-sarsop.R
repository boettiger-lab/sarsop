

source(system.file("examples/fisheries-ex.R", package = "appl"))

alpha <- sarsop(transition, observation, reward, discount, precision = 10)

compute_policy(alpha, transition, observation, reward)

sim <- sim_pomdp(transition, observation, reward, discount,
                      x0 = 5, Tmax = 20, alpha = alpha)


## Check logging works
log <- tempdir()
log_data <- data.frame(model = "ricker", r = 0.1, K = 20, C = NA, sigma_g = 0.1, sigma_m = 0.1)
alpha <- sarsop(transition, observation, reward, discount, precision = 10,
                log_dir = log, log_data = log_data)
