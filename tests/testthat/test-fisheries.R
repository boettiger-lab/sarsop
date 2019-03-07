context("fisheries")


  test_that("test generation of fisheries matrices", {


  states <- 0:20
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

  m <- fisheries_matrices(states, actions, obs,
                          reward_fn, f, sigma_g, sigma_m,
                          noise = "lognormal")

  expect_is(m, "list")
  m <- fisheries_matrices(states, actions, obs,
                          reward_fn, f, sigma_g, sigma_m,
                          noise = "normal")
  expect_is(m, "list")

  m <- fisheries_matrices(states, actions, obs,
                          reward_fn, f, sigma_g, sigma_m,
                          noise = "uniform")
  expect_is(m, "list")

  })
