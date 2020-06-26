context("sarsop")



test_that("appl base functions generates a policy", {

  assert_testing()

  model <- system.file("models/example.pomdp",  package = "sarsop")
  f <- tempfile()
  res <- pomdpsol(model, output = f, precision = 10)

  expect_true(file.exists(f))
  expect_lt(as.numeric(res["final_precision"]), 10)
  expect_true(grepl("target precision reached", res["end_condition"]))

})

test_that("pomdpeval, polgraph and pomdpsim run without error", {

  assert_testing()

  model <- system.file("models/example.pomdp",  package = "sarsop")
  policy <- tempfile()
  res <- pomdpsol(model, output = policy, precision = 10, stdout = FALSE)
  expect_true(file.exists(policy))

  evaluation <- pomdpeval(model, policy, stdout = FALSE, steps = 10)
  graph <- polgraph(model, policy, stdout = FALSE)
  simulations <- pomdpsim(model, policy, stdout = FALSE, steps = 10)

  expect_true(file.exists(evaluation))
  expect_true(file.exists(graph))
  expect_true(file.exists(simulations))
})
