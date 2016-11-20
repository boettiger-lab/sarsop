testthat::context("appl")


testthat::test_that("appl base functions generates a policy", {


  model <- system.file("models/example.pomdp",  package = "appl")
  f <- tempfile()
  res <- pomdpsol(model, output = f, precision = 10)

  expect_true(file.exists(f))
  expect_lt(as.numeric(res["final_precision"]), 10)
  expect_true(grepl("target precision reached", res["end_condition"]))

})

testthat::test_that("pomdpeval, polgraph and pomdpsim run without error", {

  model <- system.file("models/example.pomdp",  package = "appl")
  policy <- tempfile()
  res <- pomdpsol(model, output = policy, precision = 10)

  evaluation <- pomdpeval(model, policy, stdout = FALSE, steps = 10)
  graph <- polgraph(model, policy, stdout = FALSE)
  simulations <- pomdpsim(model, policy, stdout = FALSE, steps = 10)

})
