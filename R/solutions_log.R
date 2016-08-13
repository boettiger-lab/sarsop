
#' @importFrom uuid UUIDgenerate
#' @importFrom utils read.csv write.table
# @importFrom readr read_csv write_csv
solutions_log <- function(policyx, pomdpx = NULL, log_dir = ".",
                          metafile = paste0(log_dir, "/meta.csv"),
                          id = uuid::UUIDgenerate(), status,
                          n_states, n_obs, n_actions, discount,
                          log_data = NULL){

  file.copy(policyx, paste0(log_dir, "/", id, ".policyx"))
  if(!is.null(pomdpx))
    file.copy(pomdpx, paste0(log_dir, "/", id, ".pomdpx"))

  append <- file.exists(metafile)
  log <- data.frame(id, status, n_states, n_obs, n_actions, discount, date = Sys.time(), log_data)
  #readr::write_csv(log, metafile, append = append)
  utils::write.table(log, metafile, append = append, col.names = !append, sep=",", quote = FALSE, row.names = FALSE)
}

# parameters = data.frame(model = NA, r = NA, K = NA, C = NA)


#' alphas_from_log
#'
#' @param parameters a data.frame with the desired parameter values as given in metafile
#' @param log_dir path to log directory
#' @param metafile path to metafile index, assumed to be meta.csv in log_dir
#'
#' @return a list of alpha vectors matching the query, and a data.frame with the rows of the matching metadata.
#' @export
#'
#' @examples \dontrun{
#'
#' source(system.file("examples/fisheries-ex.R", package = "appl"))
#' log = tempfile()
#' alpha <- sarsop(transition, observation, reward, discount, precision = 10,
#'                 log_dir = log, log_data = log_data)
#' alphas_from_log(parameters = data.frame(model = "ricker", r = 0.1), log_dir = log)
#'
#' }
alphas_from_log <- function(parameters, log_dir = ".", metafile = paste0(log_dir, "/meta.csv")){
  #meta <- readr::read_csv(metafile)
  meta <- utils::read.csv(metafile)
  for(p in names(parameters)){
    meta <- meta[meta[[p]] %in% parameters[[p]], ]
    #meta <- meta[ grepl(parameters[[p]],meta[[p]]), ]
  }

  alphas <- lapply(1:dim(meta)[[1]], function(i){
    id <- meta[i,"id"]
    n_a <- meta[i,"n_actions"]
    results <- read_policyx(file = paste0(log_dir, "/", id, ".policyx"))
    alpha <- regularize_alpha(results$alpha, results$alpha_action, n_a)
    alpha
  })

  list(alphas = alphas, meta = meta)
}

#
