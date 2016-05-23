testthat::context("appl")


testthat::test_that("appl base functions generates a policy", {


  setwd(tempdir())
  model <- system.file("models/example.pomdp",  package = "appl")
  policy <- pomdpsol(model, timeout = 2, stdout = FALSE)

  expect_true(file.exists(policy))

  #expected <- system.file("models/output.policy",  package = "appl")
  #testthat::expect_output_file(print(readLines(policy)), expected, update = TRUE)


  # Other tools
  evaluation <- pomdpeval(model, policy, stdout = FALSE)
  graph <- polgraph(model, policy, stdout = FALSE)
  simulations <- pomdpsim(model, policy, stdout = FALSE)

})
