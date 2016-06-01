testthat::context("write_pomdpx")

testthat::test_that("write_pomdpx writes an xml problem file", {
  source(system.file("examples/fisheries-ex.R", package = "appl"))
  f <- tempfile()
  write_pomdpx(transition, observation, reward, discount, file = f, digits = 4, digits2 = 10)
  x <- xml2::read_xml(f)
  testthat::expect_is(x, "xml_document")

  ## FIXME check for validity conditions, check precision behaves as expected
})
