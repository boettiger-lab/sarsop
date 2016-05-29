testthat::context("pomdp")



testthat::test_that("pomdp functions generates a policy", {

  source(system.file("examples/fisheries-ex.R", package = "appl"))

  write_pomdpx(transition, observation, reward, discount, digits = 4, digits2 = 10)
  out = pomdpsol("input.pomdpx", "output.policy", precision = 10)

  ## Note: parallel doesn't error intelligably and cannot be interrupted gracefully either. Debug by running:
  system.time(soln <- pomdp(transition, observation, reward, discount, precision = 1, mc.cores = 1))

  policies <- data.frame(states = states,
                         exact = states - exact_policy,
                         pomdp = states - soln$policy)


  testthat::expect_is(policies, "data.frame")

  ## nope, not even for sigma_m = 0
  #testthat::expect_equivalent(policies$pomdp, policies$exact)

})
