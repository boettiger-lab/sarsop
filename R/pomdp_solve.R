
#' pomdp_solve
#'
#' pomdp_solve
#' @param transition Transition matrix, dimension n_s x n_s x n_a
#' @param observation Observation matrix, dimension n_s x n_z x n_a
#' @param utility Utility/reward matrix, dimension n_s x n_a
#' @param discount the discount factor
#' @param initial initial belief state, optional, defaults to uniform over states
#' @param verbose logical, should the function include a message with pomdp diagnostics (timings, final precision, end condition)
#' @param ... additional arguments to appl SARSOP algorithm, see \code{\link{appl}}.
#' @return optimal value and corresponding policy
#' @details Dimensions are given as number of states (n_s), number of observed states n_z, number of actions n_a
#' @export
#' @examples
#' \dontrun{
#' ## May take > 5s
#' ## Use example code to generate matrices for pomdp problem:
#' source(system.file("examples/fisheries-ex.R", package = "appl"))
#' ## Run the function:
#' soln <- pomdp_solve(transition, observation, reward, discount, precision = 10)

#' }
#'
pomdp_solve <- function(transition, observation, utility, discount, initial = rep(1, dim(observation)[[1]]) / dim(observation)[[1]], verbose = TRUE, ...){

  ## Consider more robust normalization.  Check write-out precision in write_pomdp
  initial = normalize(initial)

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

    ## Compute optimal policy based on alpha vectors, V(b) = max_i \sum_x b(x) alpha_i(x)
    V <- t(results$alpha) %*% observation[,,1]
    value <- apply(V, 2, max)
    policy <- apply(V, 2, function(x) results$alpha_action[which.max(x)])

    ## Note that policy is given as index numbers to the action vector, and state as index numbers to the states vector
    state <- 1:dim(observation)[[1]]
    data.frame(policy, value, state)



}




read_policyx = function(file = 'output.policy'){

  xml <- xml2::read_xml(file)
  vectors <- xml2::xml_find_all(xml, "//Vector")
  get_vector <- function(v) as.numeric(strsplit(as.character(xml2::xml_contents(v)), " ")[[1]])

  ## Return alpha vectors as a data.frame, n_rows = number of states, n_columns = number of alpha vectors (piecewise linear segments)
  alpha <- unname(as.data.frame(lapply(vectors, get_vector)))

  # add 1 bc C++ pomdpsol enumerates actions starting at 0
  alpha_action <- vapply(vectors, function(v) as.numeric(xml2::xml_attr(v, "action")), double(1))  + 1

  output = list(alpha,alpha_action)

}

