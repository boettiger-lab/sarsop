testthat::context("write_momdpx")

testthat::test_that("write_momdpx writes an xml problem file", {

  testthat::skip("momdpx not supported")

  source(system.file("examples/fisheries_ex_NS.R", package = "sarsop"))
  f <- tempfile()
  sarsop:::write_momdpx(transition_full, transition_par, emission, reward,
               discount, b_full, file = f, digits = 4, digits2 = 10)
  x <- xml2::read_xml(f)
  testthat::expect_is(x, "xml_document")

})
