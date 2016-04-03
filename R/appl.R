#' APPL wrappers
#'
#' Wrappers for the APPL executables
#'
#' @export
#' @rdname appl
pomdpsol <- function(args = "--help"){
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
