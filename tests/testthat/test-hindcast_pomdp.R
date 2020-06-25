context("hindcast_pomdp")

testthat::test_that("we can compute hindcasts to compare pomdp to historical values", {

  assert_testing()

  source(system.file("examples/fisheries-ex.R", package = "sarsop"))
  alpha <- sarsop(transition, observation, reward, discount, precision = 10)
  sim <- hindcast_pomdp(transition, observation, reward, discount,
                       obs = rnorm(21, 15, .1), action = rep(1, 21),
                       alpha = alpha)
  testthat::expect_is(sim, "list")
  testthat::expect_length(sim, 2)
  testthat::expect_is(sim[[1]], "data.frame")
  testthat::expect_equal(dim(sim[[1]]), c(21,4))
})
