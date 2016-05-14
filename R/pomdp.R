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
pomdp <- function(T, O, R, GAMMA, initial = NULL, mc.cores = getOption("mc.cores", 1L),
                  stdout = FALSE, ...){

  Num_s <- dim(O)[1]
  Num_z <- dim(O)[2]
  Num_a <- dim(O)[3]

  if(is.null(initial))
    initial <- rep(1/Num_s, Num_s)

  out = vector("numeric", length = Num_z)
  value = vector("numeric", length = Num_z)
  policy = vector("numeric", length = Num_z)


  ## PARALLELIZE THIS
  output <- parallel::mclapply(1:Num_z, function(i){   #for (i in 1:Num_z) {
    belief = initial * t(O[, i, 1])

    ## Consider more robust normalization.  Check write-out precision in write_pomdp
    belief = normalize(belief)
    belief = round(belief,4) / sum(round(belief,4))

    ## Why test this?  Better syntax would be: if(any(is.nan(beleif)))
    if(is.nan(sum(belief))){
      c(value = 0, policy = 0)
    } else {

      infile <- tempfile("input", fileext = ".pomdp")
      outfile <- tempfile("output", fileext = ".policy")

      ## function is basically just these three lines.  Consider arguments to pomdpsol being top-level arguments
      write_pomdp(T, O, R, GAMMA, belief, Num_s, Num_a, Num_z, file = infile)
      pomdpsol(infile, outfile, stdout = stdout, ...)
      out = read_policy(belief, file = outfile)


      c(value = out[[1]], policy = out[[2]])
    }


  }, mc.cores = mc.cores)

  list(value = sapply(output, `[[`, "value"),
       policy = sapply(output, `[[`, "policy"))
}





normalize <- function(A){
  z = sum(A)
  s = z + (z==0)
  A / s
}
