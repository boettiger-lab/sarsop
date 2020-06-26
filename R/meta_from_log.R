#' meta from log
#'
#' load metadata from a log file
#' @param parameters a data.frame with the desired parameter values as given in metafile
#' @param log_dir path to log directory
#' @param metafile path to metafile index, assumed to be meta.csv in log_dir
#'
#' @return a data.frame with the rows of the matching metadata.
#' @export
#'
#' @examples \donttest{ # takes > 5s
#'
#' source(system.file("examples/fisheries-ex.R", package = "sarsop"))
#' log = tempfile()
#' alpha <- sarsop(transition, observation, reward, discount, precision = 10,
#'                 log_dir = log)
#'
#' }
meta_from_log <- function(parameters, log_dir = ".", metafile = paste0(log_dir, "/meta.csv")){
  meta <- utils::read.csv(metafile)
  for(p in names(parameters)){
    meta <- meta[meta[[p]] %in% parameters[[p]], ]
    #meta <- meta[ grepl(parameters[[p]],meta[[p]]), ] ## Partial matching
  }
  meta
}


#' alphas_from_log
#'
#' Read alpha vectors from a log file.
#' @inheritParams meta_from_log
#' @param meta a data frame containing the log metadata
#'  for each set of alpha vectors desired, see
#'  \code{\link{meta_from_log}}
#' @return a list with a matrix of alpha vectors for each
#'  entry in the provided metadata (as returned by \code{\link{sarsop}}).
#' @export
#'
#' @examples \donttest{ # takes > 5s
#'
#' source(system.file("examples/fisheries-ex.R", package = "sarsop"))
#' log = tempfile()
#' alpha <- sarsop(transition, observation, reward, discount, precision = 10,
#'                 log_dir = log)
#'
#' }
alphas_from_log <- function(meta, log_dir = "."){
  lapply(1:dim(meta)[[1]], function(i){
    id <- meta[i,"id"]
    read_policyx(file = paste0(log_dir, "/", id, ".policyx"))
  })
}


############# NOTE: the above two log functions are general to POMDP, but the latter
############# two are specific to the fisheries example and included for convenience only

#' model from log
#'
#' Read model details from log file
#' @inheritParams alphas_from_log
#' @param reward_fn a function f(x,a) giving the reward for taking action
#'  a given a system in state x.
#' @return a list with an element for each row in the requested meta data frame,
#' which itself is a list of the three matrices: transition, observation, and
#' reward, defining the pomdp problem.
#' @details assumes transition can be determined by the f_from_log function,
#'  which is specific to the fisheries example
#' @export
#'
#' @examples \donttest{ # takes > 5s
#'
#' source(system.file("examples/fisheries-ex.R", package = "sarsop"))
#' log = tempfile()
#' alpha <- sarsop(transition, observation, reward, discount, precision = 10,
#'                 log_dir = log)
#'
#' }

models_from_log <- function(meta, reward_fn = function(x,h) pmin(x,h)){
  lapply(1:dim(meta)[[1]], function(i){

    n_states <- meta[i,"n_states"]
    min_state <- meta[i, "min_state"]
    max_state <- meta[i, "max_state"]
    n_actions <- meta[i,"n_actions"]
    min_action <- meta[i, "min_action"]
    max_action <- meta[i, "max_action"]
    n_obs <- meta[i,"n_obs"]
    min_obs <- meta[i, "min_obs"]
    max_obs <- meta[i, "max_obs"]

    if(is.null(min_state)) min_state <- 0
    if(is.null(max_state)) max_state <- n_states - 1
    if(is.null(min_action)) min_action <- 0
    if(is.null(max_action)) max_action <- n_actions - 1
    if(is.null(min_obs)) min_obs <- 0
    if(is.null(max_obs)) max_obs <- n_obs - 1

    fisheries_matrices(states = seq(min_state, max_state, length.out = n_states),
                       actions = seq(min_action, max_action, length.out = n_actions),
                       observed_states = seq(min_obs, max_obs, length.out = n_obs),
                       reward_fn = reward_fn,
                       f = f_from_log(meta)[[i]],
                       sigma_g = meta[i,"sigma_g"],
                       sigma_m = meta[i,"sigma_m"],
                       noise = as.character(meta[i, "noise"]))
  })
}

## function closures
ricker <- function(r, K)
  function(x, h){
    s <- pmax(x - h, 0)
    s * exp(r * (1 - s / K) )
  }

allen <- function(r, K, C)
  function(x, h){
    s <- pmax(x - h, 0)
    s * exp(r * (1 - s / K) * (s - C) / K)
  }


bh <- function(r, K)
  function(x, h){
    s <- pmax(x - h, 0)
    (1 + r) * s / (1 + r * s / K)
  }




#' f from log
#'
#' Read transition function from log
#' @inheritParams alphas_from_log
#' @return the growth function associated with the model indicated.
#' @details note this function is unique to the fisheries example problem and assumes that
#' sarsop call is run with logging specifying a column "model" that contains either the string
#' "ricker" (corresponding to a Ricker-type growth function) or "allen" (corresponding to an Allen-type.)
#'
#' @export
#'
#' @examples \donttest{ # takes > 5s
#'
#' source(system.file("examples/fisheries-ex.R", package = "sarsop"))
#' log = tempfile()
#' alpha <- sarsop(transition, observation, reward, discount, precision = 10,
#'                 log_dir = log)
#'
#' }
f_from_log <- function(meta){
  lapply(1:dim(meta)[[1]], function(i){
    switch(as.character(meta[i,"model"]),
           ricker = ricker(as.numeric(meta[i,"r"]), as.numeric(meta[i,"K"])),
           allen = allen(as.numeric(meta[i,"r"]),
                         as.numeric(meta[i,"K"]), as.numeric(meta[i, "C"])),
           bh = bh(as.numeric(meta[i,"r"]), as.numeric(meta[i,"K"]))
    )
  })
}

## load alpha vectors from the intermediate policy files for
## testing convergence (if timeInterval is specified in sarsop)
intermediates_from_log <- function(meta, log_dir = "."){
  lapply(1:dim(meta)[[1]], function(i){
    id <- meta[i,"id"]
    lapply(list.files(log_dir, paste0(id, ".*\\.policy")), function(file){
      read_policyx(paste0(log_dir, "/", file))
    })
  })
}
