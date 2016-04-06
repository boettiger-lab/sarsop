#' APPL wrappers
#'
#' Wrappers for the APPL executables
#'
#' @export
#' @rdname appl
#' @example setwd(tempdir())
#' file.copy(system.file("models/example.pomdp"), "example.pomdp")
#' pomdpsol("pomdpsol")
pomdpsol <- function(model, output = "outpub.policy", precision = 25, timeout = 10){
  model <- normalizePath(model, mustWork = TRUE)
  args <- paste(model, " --output=", output, " --precision=", precision, " --timeout=", timeout)
  exec_program("pomdpsol", args)
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
