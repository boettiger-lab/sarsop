#' APPL wrappers
#'
#' Wrappers for the APPL executables. The \code{pomdpsol} function solves a model
#' file and returns the path to the output policy file.
#'
#' @export
#' @rdname appl
#' @aliases appl SARSOP
#' @examples
#' file.copy(system.file("models/example.pomdp", package = "appl"), "example.pomdp")
#' policy <- pomdpsol("example.pomdp")
#' readLines(policy)
pomdpsol <- function(model, output = tempfile(), precision = 25, timeout = 10){
  model <- normalizePath(model, mustWork = TRUE)
  args <- paste(model, "--output", output, "--precision", precision, "--timeout", timeout)
  exec_program("pomdpsol", args)
  return(output)
}

#' @export
#' @rdname appl
pomdpeval <- function(args = "--help"){
  exec_program("pomdpeval", args)
}

#' @export
#' @rdname appl
pomdpsim <- function(args = "--help"){
  exec_program("pomdpsim", args)
}

#' @export
#' @rdname appl
pomdpconvert <- function(args = "--help"){
  exec_program("pomdpconvert", args)
}

#' @export
#' @rdname appl
polgraph <- function(args = "--help"){
  exec_program("polgraph", args)
}

exec_program <- function(program, args) {
  postfix <- ifelse(identical(.Platform$OS.type, "windows"), ".exe", "")
  binpath <- system.file("bin", package = "appl")
  path <- normalizePath(file.path(binpath, paste0(program, postfix)), mustWork = TRUE)
  system2(path, args)
}
