#' read_policyn
#'
#' read a .policyx file created by SARSOP and return alpha vectors and associated actions for non-stationary problems
#' @param file name of the policyx file to be read.
#' @param initial initial beliefo f regular states
#' @param fs the initial uncontrolled variable observation
#' @param Num_fs all possible uncontrolled variables
#' @return a list, first element "vectors" is an n_states x n_vectors array of alpha vectors,
#'   second element is a numeric vector "action" of length n_vectors whose i'th element indicates
#'   the action corresponding to the i'th alpha vector (column) in the vectors array.
#' @importFrom xml2 read_xml xml_find_all xml_contents xml_attr
#' @export
read_policyn = function(initial, fs, Num_fs, file = 'output.policy'){
  ## Extract vectors from XML
  xml <- xml2::read_xml(file)
  vectors <- xml2::xml_find_all(xml, "//Vector")
  get_vector <- function(v) as.numeric(strsplit(as.character(xml2::xml_contents(v)), " ")[[1]]) 
  alpha <- vectors %>% purrr::map(get_vector)
  alpha_action <- vectors %>% purrr::map_dbl(function(v) as.numeric(xml2::xml_attr(v, "action")))
  alpha_fs <- vectors %>% purrr::map_dbl(function(v) as.numeric(xml2::xml_attr(v, "obsValue")))
  
  ## generating the vectors for each observed state
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
  a <- alpha_v[[fs]] %>% map_dbl(function(x) initial %*% matrix(x,ncol=1))
  
  
  ## Return policy of the vector which has the biggest inner product
  #alpha_action[which.max(a)]
  
  output = list(max(a),alpha_a[[fs]][which.max(a)],alpha_v,alpha_a)
}