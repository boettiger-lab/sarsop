#' meta from log
#'
#' @param parameters a data.frame with the desired parameter values as given in metafile
#' @param log_dir path to log directory
#' @param metafile path to metafile index, assumed to be meta.csv in log_dir
#'
#' @return a data.frame with the rows of the matching metadata.
#' @export
#'
#' @examples \dontrun{
#'
#' source(system.file("examples/fisheries-ex.R", package = "appl"))
#' log = tempfile()
#' alpha <- sarsop(transition, observation, reward, discount, precision = 10,
#'                 log_dir = log, log_data = log_data)
#'
#' ## Get metadata for all logged solutions matching the desired query
#' meta <- meta_from_log(parameters = data.frame(model = "ricker", r = 0.1), log_dir = log)
#' alphas <- alphas_from_log(meta, log_dir = log)
#' fs <- f_from_log(meta)
#' models <- models_from_log(meta, log_dir = log)
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
#' @inheritParams meta_from_log
#' @param meta a data frame containing the log metadata for each set of alpha vectors desired, see \code{\link{meta_from_log}}
#' @return a list with a matrix of alpha vectors for each entry in the provided metadata (as returned by \code{\link{sarsop}}).
#' @export
#'
#' @examples \dontrun{
#'
#' source(system.file("examples/fisheries-ex.R", package = "appl"))
#' log = tempfile()
#' alpha <- sarsop(transition, observation, reward, discount, precision = 10,
#'                 log_dir = log, log_data = log_data)
#'
#' ## Get metadata for all logged solutions matching the desired query
#' meta <- meta_from_log(parameters = data.frame(model = "ricker", r = 0.1), log_dir = log)
#' alphas <- alphas_from_log(meta, log_dir = log)
#' }
alphas_from_log <- function(meta, log_dir = "."){
  lapply(1:dim(meta)[[1]], function(i){
    id <- meta[i,"id"]
    n_a <- meta[i,"n_actions"]
    results <- read_policyx(file = paste0(log_dir, "/", id, ".policyx"))
    regularize_alpha(results$alpha, results$alpha_action, n_a)
  })
}


############# NOTE: the above two log functions are general to POMDP, but the latter
############# two are specific to the fisheries example and included for convenience only

#' model from log
#'
#' @inheritParams alphas_from_log
#' @param reward_fn a function f(x,a) giving the reward for taking action a given a system in state x.
#' @return a list with an element for each row in the requested meta data frame,
#' which itself is a list of the three matrices: transition, observation, and
#' reward, defining the pomdp problem.
#' @details assumes transition can be determined by the f_from_log function, which is specific to the fisheries example
#' @export
#'
#' @examples \dontrun{
#'
#' source(system.file("examples/fisheries-ex.R", package = "appl"))
#' log = tempfile()
#' alpha <- sarsop(transition, observation, reward, discount, precision = 10,
#'                 log_dir = log, log_data = log_data)
#'
#' ## Get metadata for all logged solutions matching the desired query
#' meta <- meta_from_log(parameters = data.frame(model = "ricker", r = 0.1), log_dir = log)
#' alphas <- alphas_from_log(meta, log_dir = log)
#' fs <- f_from_log(meta)
#' models <- models_from_log(meta)
#' }

models_from_log <- function(meta, reward_fn = function(x,h) pmin(x,h)){
  lapply(1:dim(meta)[[1]], function(i){
    fisheries_matrices(states = 1:meta[i,"n_states"],
                       actions = 1:meta[i,"n_actions"],
                       observed_states = 1:meta[i,"n_obs"],
                       reward_fn = reward_fn,
                       f = f_from_log(meta)[[1]],
                       sigma_g = meta[i,"sigma_g"],
                       sigma_m = meta[i,"sigma_m"])
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


#' f from log
#'
#' @inheritParams alphas_from_log
#' @return the growth function associated with the model indicated.
#' @details note this function is unique to the fisheries example problem and assumes that
#' sarsop call is run with logging specifying a column "model" that contains either the string
#' "ricker" (corresponding to a Ricker-type growth function) or "allen" (corresponding to an Allen-type.)
#'
#' @export
#'
#' @examples \dontrun{
#'
#' source(system.file("examples/fisheries-ex.R", package = "appl"))
#' log = tempfile()
#' alpha <- sarsop(transition, observation, reward, discount, precision = 10,
#'                 log_dir = log, log_data = log_data)
#'
#' ## Get metadata for all logged solutions matching the desired query
#' meta <- meta_from_log(parameters = data.frame(model = "ricker", r = 0.1), log_dir = log)
#' alphas <- alphas_from_log(meta, log_dir = log)
#' fs <- f_from_log(meta)
#' models <- models_from_log(meta)
#' }
f_from_log <- function(meta){
  lapply(1:dim(meta)[[1]], function(i){
    switch(meta[i,"model"],
           ricker = ricker(meta[i,"r"], meta[i,"K"]),
           allen = allen(meta[i,"r"], meta[i,"K"], meta[i, "C"])
    )
  })
}
