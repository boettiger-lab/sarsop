#' sarsop
#'
#' sarsop wraps the tasks of writing the pomdpx file defining the problem,
#' running the pomdsol (SARSOP) algorithm in C++, and then reading the
#' resulting policy file back into R.  The returned alpha vectors and
#' alpha_action information is then transformed into a more generic,
#' user-friendly representation as a matrix whose columns correspond
#' to actions and rows to states.  This function can thus be used at
#' the heart of most pomdp applications.
#'
#' @param transition Transition matrix, dimension n_s x n_s x n_a
#' @param observation Observation matrix, dimension n_s x n_z x n_a
#' @param reward reward matrix, dimension n_s x n_a
#' @param discount the discount factor
#' @param state_prior initial belief state, optional, defaults to uniform
#'  over states
#' @param verbose logical, should the function include a message with pomdp
#'  diagnostics (timings, final precision, end condition)
#' @param ... additional arguments to \code{\link{appl}}.
#' @param log_dir pomdpx and policyx files will be saved here, along with
#'  a metadata file
#' @param log_data a data.frame of additional columns to include in the log,
#'  such as model parameters. A unique id value for each run can be provided
#'   as one of the columns, otherwise, a globally unique id will be generated.
#' @param cache should results from the log directory be cached? Default TRUE.
#'  Identical functional calls will quickly return previously cached alpha
#'  vectors from file rather than re-running.
#' @return a matrix of alpha vectors. Column index indicates action associated
#'  with the alpha vector, (1:n_actions), rows indicate system state, x.
#'  Actions for which no alpha vector was found are included as all -Inf,
#'  since such actions are not optimal regardless of belief, and thus have no
#'  corresponding alpha vectors in alpha_action list.
#' @export
#' @importFrom digest digest
#' @examples
#' \donttest{ ## Takes > 5s
#' ## Use example code to generate matrices for pomdp problem:
#' source(system.file("examples/fisheries-ex.R", package = "sarsop"))
#' alpha <- sarsop(transition, observation, reward, discount, precision = 10)
#' compute_policy(alpha, transition, observation, reward)
#' }
#'
sarsop <- function(transition,
                   observation,
                   reward,
                   discount,
                   state_prior = rep(1, dim(observation)[[1]]) / dim(observation)[[1]],
                   verbose = TRUE,
                   log_dir = tempdir(),
                   log_data = NULL,
                   cache = TRUE,
                   ...){

  ## unique id based on arguments specified in function call
  id <- digest::digest(list(transition, observation, reward, discount,
                            state_prior, verbose, log_dir, log_data,
                            cache, ...))

  infile <- paste0(log_dir, "/", id,  ".pomdpx")
  outfile <-  paste0(log_dir, "/", id,  ".policyx")
  stdout <-  paste0(log_dir, "/", id,  ".log")

  ## Basically a simple version of memoise with a filesystem cache
  if (cache && file.exists(outfile)) {
    return(read_policyx(file = outfile))
  }

  if(!dir.exists(log_dir)){
    dir.create(log_dir, FALSE, TRUE)
  }

  ## Consider more robust normalization.  Check the write-out precision in write_pomdp
  initial = normalize(state_prior)
  ## Compute alpha-vectors using SARSOP pomdp algorithm from APPL
  write_pomdpx(transition, observation, reward, discount, initial, file = infile)
  status <- pomdpsol(infile, outfile, stdout = stdout, ...)

  if (verbose && is.list(status)) {
    message(paste("load time:", status[["load_time_sec"]],
                  "sec, init time:", status[["init_time_sec"]],
                  "sec, run time:", status[["run_time_sec"]],
                  "sec, final precision:", status[["final_precision"]],
                  "end_condition:", status[["end_condition"]]))
  }

  alpha <- read_policyx(file = outfile)

  if (!is.null(log_data))
    solutions_log(id,
                  metafile = paste0(log_dir, "/meta.csv"),
                  status = status,
                  n_states = dim(observation)[[1]],
                  n_obs = dim(observation)[[2]],
                  n_actions = dim(observation)[[3]],
                  discount = discount,
                  log_data = log_data)

  alpha
}
