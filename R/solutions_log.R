
#' @importFrom uuid UUIDgenerate
solutions_log <- function(policyx, pomdpx = NULL, log_dir = ".",
                          metafile = paste0(log_dir, "/meta.csv"),
                          id = uuid::UUIDgenerate(), status,
                          n_states, n_obs, n_actions, discount,
                          log_data = NULL){

  file.copy(policyx, paste0(log_dir, "/", id, ".policyx"))
  if(!is.null(pomdpx))
    file.copy(pomdpx, paste0(log_dir, "/", id, ".pomdpx"))

  append <- file.exists(metafile)
  write.table(data.frame(id, status, n_states, n_obs, n_actions, discount, log_data),
              file = metafile,
              append = append, sep = ", ", row.names = FALSE, col.names = !append)

}

# parameters = data.frame(model = NA, r = NA, K = NA, C = NA)

read_log <- function(parameters, log_dir = ".", metafile = paste0(log_dir, "/meta.csv")){
  meta <- read.csv(metafile)
}
