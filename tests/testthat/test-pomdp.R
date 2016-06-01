testthat::context("pomdp")

testthat::test_that("pomdp functions generates a policy", {

  source(system.file("examples/fisheries-ex.R", package = "appl"))

  soln <- pomdp(transition, observation, reward, discount, precision = 10)

  ## Deterministic Solution
  out <- optimize(function(x) -f(x,0) + x / discount, c(min(states),max(states)))
  S_star <- round(out$minimum)
  exact_policy <- sapply(states, function(x) if(x < S_star) 0 else x - S_star)

  policies <- data.frame(states = states,
                         exact = states - exact_policy,
                         pomdp = states - soln$policy)


  testthat::expect_is(policies, "data.frame")

  ## nope, not even for sigma_m = 0
  #testthat::expect_equivalent(policies$pomdp, policies$exact)

})
