testthat::context("appl")


testthat::test_that("appl base functions generates a policy", {


  model <- system.file("models/example.pomdp",  package = "appl")
  f <- tempfile()
  policy <- pomdpsol(model, output = f, timeout = 2, precision = 10)

  expect_true(file.exists(f))

  #expected <- system.file("models/output.policy",  package = "appl")
  #testthat::expect_output_file(print(readLines(policy)), expected, update = TRUE)


  # Other tools
  evaluation <- pomdpeval(model, policy, stdout = FALSE)
  graph <- polgraph(model, policy, stdout = FALSE)
  simulations <- pomdpsim(model, policy, stdout = FALSE)

})
