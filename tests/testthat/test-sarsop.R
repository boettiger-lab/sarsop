

source(system.file("examples/fisheries-ex.R", package = "appl"))

alpha <- sarsop(transition, observation, reward, discount, precision = 10)

compute_policy(alpha, transition, observation, reward)

sim <- sim_pomdp(transition, observation, reward, discount,
                      x0 = 5, Tmax = 20, alpha = alpha)
