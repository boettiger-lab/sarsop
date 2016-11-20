
#' read_policyx
#'
#' read a .policyx file created by SARSOP and return alpha vectors and associated actions.
#' @param file name of the policyx file to be read.
#' @return a list, first element "vectors" is an n_states x n_vectors array of alpha vectors,
#'   second element is a numeric vector "action" of length n_vectors whose i'th element indicates
#'   the action corresponding to the i'th alpha vector (column) in the vectors array.
#' @importFrom xml2 read_xml xml_find_all xml_contents xml_attr
#' @export
read_policyx = function(file = 'output.policyx'){

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



#' read_policy
#'
#' @param initial belief to compute dot product with alpha vectors.
#' @param fs the initial fully observable state; Only for the case MOMDP
#' @param Num_fs dimension of the fully observable state; Only for the case MOMDP
#' @param file path to file to read in policy
#' @return a list of the max alpha and associated alpha action, as well as all alpha vectors and associated actions.
read_policy = function(initial, file = 'output.policy', fs, Num_fs){

  ## Extract vectors from XML
  xml <- xml2::read_xml(file)
  vectors <- xml2::xml_find_all(xml, "//Vector")
  get_vector <- function(v) as.numeric(strsplit(as.character(xml2::xml_contents(v)), " ")[[1]])

  alpha <- lapply(vectors, get_vector)
  #alpha <- vectors %>% purrr::map(get_vector)

  alpha_action <- vapply(vectors, function(v) as.numeric(xml2::xml_attr(v, "action")), double(1))
  #alpha_action <- vectors %>% purrr::map_dbl(function(v) as.numeric(xml2::xml_attr(v, "action")))

  if(missing(fs) && missing(Num_fs)){
    a <- vapply(alpha, function(x) initial %*% matrix(x, ncol=1), double(1))
    opt_value = max(a); opt_act = alpha_action[which.max(a)]
    alpha_v = alpha; alpha_a = alpha_action
  } else{
    alpha_fs <- vapply(vectors, function(v) as.numeric(xml2::xml_attr(v, "obsValue")), double(1))
    alpha_v = vector('list',Num_fs)
    alpha_a = vector('list',Num_fs)
    for (j in 1:Num_fs){
      k = 1
      for(i in 1:length(alpha_fs)){
        if(alpha_fs[i] == j-1){
          alpha_v[[j]][[k]] = alpha[[i]]
          alpha_a[[j]][k] = alpha_action[i]
          k = k+1
        }
      }
    }
    ## Compute dot product with initial
    a <- vapply(alpha_v[[fs]], function(x) initial %*% matrix(x,ncol=1), double(1))
    opt_value = max(a); opt_act = alpha_a[[fs]][which.max(a)]
  }

 output = list(opt_value,opt_act,alpha_v,alpha_a)

}
