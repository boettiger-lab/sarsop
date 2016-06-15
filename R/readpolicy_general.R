#' readpolicy_general
#'
#' @param initial belief to compute dot product with alpha vectors.
#' @param fs the initial fully observable state
#' @param Num_fs dimension of the fully observable state
#' @param file path to file to read in policy
#' @return a list of the max alpha and associated alpha action, as well as all alpha vectors and associated actions.
#' @importFrom xml2 read_xml xml_find_all xml_contents xml_attr
#' @export

readpolicy_general = function(initial, file = 'output.policy', fs, Num_fs){
  
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
    list(max(a), alpha_action[which.max(a)], alpha, alpha_action)
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
    output = list(max(a),alpha_a[[fs]][which.max(a)],alpha_v,alpha_a)
  }
 
}
