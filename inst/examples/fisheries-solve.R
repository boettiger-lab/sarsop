#library("MDPtoolbox")
#library("sarsop")

source(system.file("examples/fisheries-ex.R", package = "sarsop"))


## Deterministic Solution
out <- optimize(function(x) -f(x,0) + x / discount, c(min(states),max(states)))
S_star <- round(out$minimum)
exact_policy <- sapply(states, function(x) if(x < S_star) 0 else x - S_star)


## MDP Solution, requires MDPtoolbox
mdp <- MDPtoolbox::mdp_policy_iteration(transition, reward, discount)


## POMDP Solution
system.time(soln <- pomdp(transition, observation, reward, discount, precision = 10))

## Note: parallel doesn't error intelligably and cannot be interrupted gracefully either. Debug by running:
#system.time( soln <- pomdp(transition, observation, reward, discount, mc.cores = parallel::detectCores(), precision = 5, memory = 2000) )

policies <- data.frame(states = states,
                       exact = states - exact_policy,
                       mdp = states - actions[mdp$policy],
                       pomdp = states - soln$policy)

plot(policies$states, policies$pomdp)
lines(policies$states, policies$exact)
points(policies$states, policies$mdp, pch = 4)
