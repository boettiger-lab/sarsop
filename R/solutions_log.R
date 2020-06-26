#' @importFrom utils read.csv write.table
# @importFrom readr read_csv write_csv
#' @noRd
solutions_log <- function(id,
                          metafile = "meta.csv",
                          status,
                          n_states, n_obs, n_actions, discount,
                          log_data = NULL){

  append <- file.exists(metafile)
  log <- data.frame(id, status, n_states, n_obs,
                    n_actions, discount, date = Sys.time(), log_data)
  utils::write.table(log, metafile, append = append,
                     col.names = !append, sep=",", quote = FALSE, row.names = FALSE)
}

