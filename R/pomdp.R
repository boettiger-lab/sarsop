#' pomdp
#'
#' pomdp
#' @param T Transition matrix, dimension n_s x n_s x n_a
#' @param O Observation matrix, dimension n_s x n_z x n_a
#' @param R Reward matrix, dimension n_s x n_a
#' @param GAMMA the discount factor
#' @param initial initial belief state, optional, defaults to uniform over states
#' @param mc.cores number of cores needed for parallel runs.
#' @param ... additional arguments to appl SARSOP algorithm, see \code{\link{appl}}.
#' @return optimal value and corresponding policy
#' @details Dimensions are given as number of states (n_s), number of observed states n_z, number of actions n_a
#' @importFrom parallel mclapply
#' @export
#' @examples
#' \dontrun{
#' ## May take > 5s
#' ## Use example code to generate matrices for pomdp problem:
#' source(system.file("examples/fisheries-ex.R", package = "appl"))
#' ## Run the function:
#' soln <- pomdp(transition, observation, reward, discount, precision = 10)

#' }
#'
pomdp <- function(T, O, R, GAMMA, initial = NULL, mc.cores = getOption("mc.cores", 1L), ...){

  Num_s <- dim(O)[1]
  Num_z <- dim(O)[2]
  Num_a <- dim(O)[3]

  if(is.null(initial))
    initial <- rep(1/Num_s, Num_s)

  out = vector("numeric", length = Num_z)
  value = vector("numeric", length = Num_z)
  policy = vector("numeric", length = Num_z)


  output <- parallel::mclapply(1:Num_z, function(i){   #for (i in 1:Num_z) {
    belief = initial * t(O[, i, 1])

    ## Consider more robust normalization.  Check write-out precision in write_pomdp
    belief = normalize(belief)

    if(any(is.nan(belief)) || sum(belief) == 0){
      # Belief has already converged
      list(value = 0, policy = 0, alpha = list(), alpha_action = list())
    } else {

      infile <- tempfile("input", fileext = ".pomdp")
      outfile <- tempfile("output", fileext = ".policy")

      ## function is basically just these three lines.  Consider arguments to pomdpsol being top-level arguments
      write_pomdpx(T, O, R, GAMMA, belief, file = infile)
      pomdpsol(infile, outfile, ...)
      out = read_policy(belief, file = outfile)

      list(value = out[[1]], policy = out[[2]], alpha = out[[3]], alpha_action = out[[4]])
    }


  }, mc.cores = mc.cores)


  ## Re-arrange results
  list(value  = sapply(output, `[[`, "value"),
       policy = sapply(output, `[[`, "policy"),
       alpha  = lapply(output, `[[`, "alpha"),
       alpha_action = lapply(output, `[[`, "alpha_action"))
}




#  Remarkably SARSOP seems to have a terrible concept of floating point precision. We have to normalize to 4 digits
# and then print more than 4 (to remain normalized) in floating point notation (see formatC calls in write_pomdpx)
normalize <- function(A, digits = 4){
  if(!is.null(digits)){
    A <- as.numeric(formatC(A, digits = digits, format="f"))

  }
  z = sum(A)
  s = z + (z==0)
  A / s
}



