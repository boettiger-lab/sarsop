#' test the APPL binaries
#'
#' Asserts that the C++ binaries for appl have been compiled successfully
#'
#' @return Will return TRUE if binaries are installed and can be located
#'  and executed, and FALSE otherwise.
#' @export
#' @examples
#' assert_has_appl()
#'
assert_has_appl <- function(){
  model <- system.file("models", "example.pomdp", package = "sarsop")
  path <- exec_path("pomdpsol")
  args <- paste(model, "--timeout 0.01 --fast", "--output",
                tempfile(fileext = ".policy"))
  res <- processx::run(path, strsplit(args, " ")[[1]],
                       spinner = FALSE, error_on_status = FALSE)
  res$status == 0
}


assert_testing <- function()
{
  ## Always test on systems set to NOT CRAN
  ## Otherwise, skip test if assert_working() fails
  always_test <- identical(Sys.getenv("NOT_CRAN"), "true")
  condition <- assert_has_appl() || always_test
  if (!isTRUE( condition )) {
    testthat::skip("Detected problem with binary installation, skipping test")
  }
}
