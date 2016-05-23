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
#' @param fast logical, default FALSE. use fast (but very picky) alternate parser for .pomdp files.
#' @param precision targetPrecision. Set targetPrecision as the target precision in solution quality; run ends when target precision is reached. The target precision is 1 by default.
#' @param randomization logical, default FALSE. Turn on randomization for the sampling algorithm.
#' @param timeout Use timeLimit as the timeout in seconds.  If running time exceeds the specified value, pomdpsol writes out a policy and terminates. There is no time limit by default.
#' @param memory Use memoryLimit as the memory limit in MB. No memory limit by default.  If memory usage exceeds the specified value, pomdpsol writes out a policy and terminates. Set the value to be less than physical memory to avoid swapping.
#' @param improvementConstant Use improvementConstant as the trial improvement factor in the sampling algorithm. At the default of 0.5, a trial terminates at a belief when the gap between its upper and lower bound is 0.5 of the current precision at the initial belief.
#' @param timeInterval Use timeInterval as the time interval between two consecutive write-out of policy files. If this is not specified, pomdpsol only writes out a policy file upon termination.
#' @param stdout where output to 'stdout', see \code{\link{system2}}. Use \code{FALSE}
#' to suppress output.
#' @examples
#' setwd(tempdir())
#' model <- system.file("models/example.pomdp", package = "appl")
#' policy <- pomdpsol(model, timeout = 2, stdout = FALSE)
#' readLines(policy)
#'
#' # Other tools
#' evaluation <- pomdpeval(model, policy, stdout = FALSE)
#' graph <- polgraph(model, policy, stdout = FALSE)
#' simulations <- pomdpsim(model, policy, stdout = FALSE)
pomdpsol <- function(model, output = tempfile(), precision = 1, timeout = NULL,
                     fast = FALSE, randomization = FALSE, memory = NULL,
                     improvementConstant = NULL, timeInterval = NULL, stdout = ""){
  model <- normalizePath(model, mustWork = TRUE)
  args <- paste(model, "--output", output, "--precision", precision)

  if(!is.null(timeout)) args <- paste(args, "--timeout", timeout)
  if(!is.null(memory)) args <- paste(args, "--memory", memory)
  if(!is.null(timeInterval)) args <- paste(args, "--policy-interval", timeInterval)
  if(!is.null(improvementConstant)) paste(args, "--trial-improvement-factor", improvementConstant)
  if(randomization) args <- paste(args, "--randomization")
  if(fast) args <- paste(args, "--fast")
  exec_program("pomdpsol", args, stdout = stdout)
  return(output)
}

#' @export
#' @rdname appl
#' @param policy file/path to the policy file
#' @param max_depth the maximum horizon of the generated policy graph
#' @param max_branches maximum number of branches to show in the policy graph
#' @param min_prob the minimum probability threshold for a branch to be shown in the policy graph
polgraph <- function(model, policy, output = tempfile(), max_depth = 3, max_branches = 10,
                     min_prob = 0.001, stdout = ""){
  model <- normalizePath(model, mustWork = TRUE)
  policy <- normalizePath(policy, mustWork = TRUE)
  args <- paste(model, "--policy-file", policy, "--policy-graph", output, "--graph-max-depth", max_depth,
    "--graph-max-branch", max_branches, "--graph-min-prob", min_prob)
  exec_program("polgraph", args, stdout = stdout)
  return(output)
}

#' @export
#' @rdname appl
#' @param steps number of steps for each simulation run
#' @param simulations as the number of simulation runs
pomdpsim <- function(model, policy, output = tempfile(), steps = 100, simulations = 3, stdout = ""){
  model <- normalizePath(model, mustWork = TRUE)
  policy <- normalizePath(policy, mustWork = TRUE)
  args <- paste(model, "--policy-file", policy, "--output-file", output, "--simLen",
    steps, "--simNum", simulations)
  exec_program("pomdpsim", args, stdout = stdout)
  return(output)
}

#' @export
#' @rdname appl
pomdpeval <- function(model, policy, output = tempfile(), steps = 100, simulations = 3, stdout = ""){
  model <- normalizePath(model, mustWork = TRUE)
  policy <- normalizePath(policy, mustWork = TRUE)
  args <- paste(model, "--policy-file", policy, "--output-file", output, "--simLen",
                steps, "--simNum", simulations)
  exec_program("pomdpeval", args, stdout = stdout)
  return(output)
}

#' @export
#' @rdname appl
pomdpconvert <- function(model, stdout = ""){
  model <- normalizePath(model, mustWork = TRUE)
  exec_program("pomdpconvert", model)
  return(model)
}

exec_program <- function(program, args, stdout) {
  if(identical(.Platform$OS.type, "windows")){
    program <- paste0(.Platform$r_arch, "/", program, ".exe")
  }
  binpath <- system.file("bin", package = "appl")
  path <- normalizePath(file.path(binpath, program), mustWork = TRUE)
  res <- system2(path, args, stdout = stdout)
  if(res != 0) stop("Call to ", program, " failed with error: ", res)
  return(NULL)
}
