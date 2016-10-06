#' sarsop
#'
#' sarsop wraps the tasks of writing the pomdpx file defining the problem, running the pomdsol (SARSOP) algorithm in C++,
#' and then reading the resulting policy file back into R.  The returned alpha vectors and alpha_action information is then
#' transformed into a more generic, user-friendly repesentation as a matrix whose columns correspond to actions and rows to states.
#' This function can thus be used at the heart of most pomdp applications.
#' @param transition Transition matrix, dimension n_s x n_s x n_a
#' @param observation Observation matrix, dimension n_s x n_z x n_a
#' @param reward reward matrix, dimension n_s x n_a
#' @param discount the discount factor
#' @param state_prior initial belief state, optional, defaults to uniform over states
#' @param verbose logical, should the function include a message with pomdp diagnostics (timings, final precision, end condition)
#' @param ... additional arguments to \code{\link{appl}}.
#' @param log_dir pomdpx and policyx files will be saved here, along with a metadata file
#' @param log_data a data.frame of additional columns to include in the log, such as model parameters. A unique id value for each run
#' can be provided as one of the columns, otherwise, a globally unique id will be generated.
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
sarsop <- function(transition, observation, reward, discount,
                   state_prior = rep(1, dim(observation)[[1]]) / dim(observation)[[1]],
                   verbose = TRUE, log_dir = tempdir(), log_data = NULL, ...){
  ## Consider more robust normalization.  Check write-out precision in write_pomdp
  initial = normalize(state_prior)

  ## Use ID given in log_data, if provided
  if(is.null(log_data) | is.null(log_data$id)){
    id <- gsub("/", "", tempfile("", tmpdir = ""))
  } else {
    id <- log_data$id
    log_data$id <- NULL
  }


  ## Compute alpha-vectors using SARSOP pomdp algorithm from APPL

  infile <- paste0(log_dir, "/", id,  ".pomdpx")
  outfile <-  paste0(log_dir, "/", id,  ".policyx")
  stdout <-  paste0(log_dir, "/", id,  ".log")
  write_pomdpx(transition, observation, reward, discount, initial, file = infile)
  status <- pomdpsol(infile, outfile, stdout = stdout, ...)

  if(verbose){
    message(paste("load time:", status[["load_time_sec"]],
                  "sec, init time:", status[["init_time_sec"]],
                  "sec, run time:", status[["run_time_sec"]],
                  "sec, final precision:", status[["final_precision"]],
                  "end_condition:", status[["end_condition"]]))
  }

  alpha <- read_policyx(file = outfile)

  if(!is.null(log_data))
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


read_policyx = function(file = 'output.policy'){

  xml <- xml2::read_xml(file)
  xml_vectors <- xml2::xml_find_all(xml, "//Vector")
  get_vector <- function(v) as.numeric(strsplit(as.character(xml2::xml_contents(v)), " ")[[1]])
  get_action <- function(v) as.numeric(xml2::xml_attr(v, "action"))

  n_states <- length(get_vector(xml_vectors[[1]]))
  ## Return alpha vectors as an array, n_rows = number of states, n_columns = number of alpha vectors (piecewise linear segments)
  alpha <-vapply(xml_vectors, get_vector, numeric(n_states))

  # add 1 bc C++ pomdpsol enumerates actions starting at 0
  alpha_action <- vapply(xml_vectors, get_action, double(1))  + 1

  list(vectors = alpha, action = alpha_action)
}

