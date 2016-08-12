#' sarsop
#'
#' sarsop wraps the tasks of writing the pomdpx file defining the problem, running the pomdsol (SARSOP) algorithm in C++,
#' and then reading the resulting policy file back into R.  The returned alpha vectors and alpha_action information is then
#' transformed into a more generic, user-friendly repesentation as a matrix whose columns correspond to actions and rows to states.
#' This function can thus be used at the heart of most pomdp applications.
#' @param transition Transition matrix, dimension n_s x n_s x n_a
#' @param observation Observation matrix, dimension n_s x n_z x n_a
#' @param utility Utility/reward matrix, dimension n_s x n_a
#' @param discount the discount factor
#' @param initial initial belief state, optional, defaults to uniform over states
#' @param verbose logical, should the function include a message with pomdp diagnostics (timings, final precision, end condition)
#' @param ... additional arguments to \code{\link{appl}}.
#' @return a matrix of alpha vectors. Column index indicates action associated with the alpha vector, (1:n_actions),
#'  rows indicate system state, x. Actions for which no alpha vector was found are included as all -Inf, since such actions are
#'  not optimal regardless of belief, and thus have no corresponding alpha vectors in alpha_action list.
#' @export
#' @examples
#' \dontrun{ ## Takes > 5s
#' ## Use example code to generate matrices for pomdp problem:
#' source(system.file("examples/fisheries-ex.R", package = "appl"))
#' alpha <- sarsop(transition, observation, reward, discount, precision = 10)
#' compute_policy(alpha, transition, observation, reward)
#' }
#'
sarsop <- function(transition, observation, utility, discount, state_prior = rep(1, dim(observation)[[1]]) / dim(observation)[[1]], verbose = TRUE, ...){

  ## Consider more robust normalization.  Check write-out precision in write_pomdp
  initial = normalize(state_prior)

  ## Consider checks to initial and to matrices to make sure they meet fundamental assumptions.

  ## Compute alpha-vectors using SARSOP pomdp algorithm from APPL
  infile <- tempfile("input", fileext = ".pomdp")
  outfile <- tempfile("output", fileext = ".policy")
  write_pomdpx(transition, observation, utility, discount, initial, file = infile)
  status <- pomdpsol(infile, outfile, ...)

  if(verbose){
    message(paste("load time:", status[["load_time_sec"]],
                  "sec, init time:", status[["init_time_sec"]],
                  "sec, run time:", status[["run_time_sec"]],
                  "sec, final precision:", status[["final_precision"]],
                  "end_condition:", status[["end_condition"]]))
  }

  results <- read_policyx(file = outfile)
  regularize_alpha(results$alpha, results$alpha_action, n_a = dim(observation)[[3]])

}


read_policyx = function(file = 'output.policy'){

  xml <- xml2::read_xml(file)
  vectors <- xml2::xml_find_all(xml, "//Vector")
  get_vector <- function(v) as.numeric(strsplit(as.character(xml2::xml_contents(v)), " ")[[1]])

  ## Return alpha vectors as a data.frame, n_rows = number of states, n_columns = number of alpha vectors (piecewise linear segments)
  alpha <- unname(as.data.frame(lapply(vectors, get_vector)))

  # add 1 bc C++ pomdpsol enumerates actions starting at 0
  alpha_action <- vapply(vectors, function(v) as.numeric(xml2::xml_attr(v, "action")), double(1))  + 1

  list(alpha = alpha, alpha_action = alpha_action)
}


## POMDP solver returns a data.frame whose columns are the alpha vectors.
## The action corresponding to each vector is given by alpha_action[i].
##
## Each alpha vector is of length n_states,  but there is not one vector for each
## action -- some actions are not represented, others may be repeated (depends on #
## of piecewise linear segments used to approximate value)
##
## So we create a new data.frame whose i'th column is the alpha vector for the i'th action
regularize_alpha <- function(alpha, alpha_action, n_a){
  n_x <- dim(alpha)[[1]]
  vapply(1:n_a, function(i){
    j <- which(alpha_action == i)[1]
    if(!is.na(j))
      alpha[, j]
    else
      rep(0, n_x)
  }, numeric(n_x))
}




