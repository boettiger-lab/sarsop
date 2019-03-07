testthat::context("write pomdpx and read policyx")

testthat::test_that("write_pomdpx writes an xml problem file", {

  m <- fisheries_matrices()
  f <- tempfile()
  write_pomdpx(m$transition, m$observation, m$reward, 0.95,
               file = f)
  x <- xml2::read_xml(f)
  testthat::expect_is(x, "xml_document")

  ## FIXME check for validity conditions, check precision behaves as expected
})



testthat::test_that("we can read policy / policyx files", {
  f <- system.file("extdata", "out.policy", package="sarsop", mustWork = TRUE)
  policy <- read_policyx(f)
  expect_is(policy, "list")
  expect_equal(names(policy), c("vectors", "action"))
  expect_is(policy$vectors, "matrix")
})


