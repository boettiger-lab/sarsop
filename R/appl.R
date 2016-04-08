#' APPL wrappers
#'
#' Wrappers for the APPL executables. The \code{pomdpsol} function solves a model
#' file and returns the path to the output policy file.
#'
#' @export
#' @rdname appl
#' @aliases appl SARSOP
#' @param model file/path to the \code{pomdp} model file
#' @param output file/path of the output policy file. This is also returned by the function.
#' @param precision precision value
#' @param timeout timeout in seconds
#' @examples
#' setwd(tempdir())
#' model <- system.file("models/example.pomdp", package = "appl")
#' policy <- pomdpsol(model)
#' readLines(policy)
#'
#' # Other tools
#' evaluation <- pomdpeval(model, policy)
#' graph <- polgraph(model, policy)
#' simulations <- pomdpsim(model, policy)
pomdpsol <- function(model, output = tempfile(), precision = 25, timeout = 10){
  model <- normalizePath(model, mustWork = TRUE)
  args <- paste(model, "--output", output, "--precision", precision, "--timeout", timeout)
  exec_program("pomdpsol", args)
  return(output)
}

#' @export
#' @rdname appl
#' @param policy file/path to the policy file
#' @param max_depth the maximum horizon of the generated policy graph
#' @param max_branches maximum number of branches to show in the policy graph
#' @param min_prob the minimum probability threshold for a branch to be shown in the policy graph
polgraph <- function(model, policy, output = tempfile(), max_depth = 3, max_branches = 10, min_prob = 0.001){
  model <- normalizePath(model, mustWork = TRUE)
  policy <- normalizePath(policy, mustWork = TRUE)
  args <- paste(model, "--policy-file", policy, "--policy-graph", output, "--graph-max-depth", max_depth,
    "--graph-max-branch", max_branches, "--graph-min-prob", min_prob)
  exec_program("polgraph", args)
  return(output)
}

#' @export
#' @rdname appl
#' @param steps number of steps for each simulation run
#' @param simulations as the number of simulation runs
pomdpsim <- function(model, policy, output = tempfile(), steps = 100, simulations = 3){
  model <- normalizePath(model, mustWork = TRUE)
  policy <- normalizePath(policy, mustWork = TRUE)
  args <- paste(model, "--policy-file", policy, "--output-file", output, "--simLen",
    steps, "--simNum", simulations)
  exec_program("pomdpsim", args)
  return(output)
}

#' @export
#' @rdname appl
pomdpeval <- function(model, policy, output = tempfile(), steps = 100, simulations = 3){
  model <- normalizePath(model, mustWork = TRUE)
  policy <- normalizePath(policy, mustWork = TRUE)
  args <- paste(model, "--policy-file", policy, "--output-file", output, "--simLen",
                steps, "--simNum", simulations)
  exec_program("pomdpeval", args)
  return(output)
}

#' @export
#' @rdname appl
pomdpconvert <- function(model){
  model <- normalizePath(model, mustWork = TRUE)
  exec_program("pomdpconvert", model)
  return(model)
}

exec_program <- function(program, args) {
  if(identical(.Platform$OS.type, "windows")){
    program <- paste0(.Platform$r_arch, "/", program, ".exe")
  }
  binpath <- system.file("bin", package = "appl")
  path <- normalizePath(file.path(binpath, program), mustWork = TRUE)
  res <- system2(path, args)
  if(res != 0) stop("Call to ", program, " failed with error: ", res)
  return(NULL)
}
