context("es matrices")

test_that("we can make ecosystem services matrices", {

  m <- es_matrices()
  expect_is(m, "list")

})

