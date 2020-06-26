
#' read_policyx
#'
#' read a .policyx file created by SARSOP and return alpha vectors and associated actions.
#' @param file name of the policyx file to be read.
#' @return a list, first element "vectors" is an n_states x n_vectors array of alpha vectors,
#'   second element is a numeric vector "action" of length n_vectors whose i'th element indicates
#'   the action corresponding to the i'th alpha vector (column) in the vectors array.
#' @importFrom xml2 read_xml xml_find_all xml_contents xml_attr
#' @export
#' @examples
#' f <- system.file("extdata", "out.policy", package="sarsop", mustWork = TRUE)
#' policy <- read_policyx(f)
#'
read_policyx = function(file = 'output.policyx'){

  xml <- xml2::read_xml(file)
  xml_vectors <- xml2::xml_find_all(xml, "//Vector")
  get_vector <- function(v) as.numeric(
    strsplit(as.character(xml2::xml_contents(v)), " ")[[1]])
  get_action <- function(v) as.numeric(xml2::xml_attr(v, "action"))

  n_states <- length(get_vector(xml_vectors[[1]]))
  ## Return alpha vectors as an array, n_rows = number of states,
  #  n_columns = number of alpha vectors (piecewise linear segments)
  alpha <-vapply(xml_vectors, get_vector, numeric(n_states))

  # add 1 bc C++ pomdpsol enumerates actions starting at 0
  alpha_action <- vapply(xml_vectors, get_action, double(1))  + 1

  list(vectors = alpha, action = alpha_action)
}

