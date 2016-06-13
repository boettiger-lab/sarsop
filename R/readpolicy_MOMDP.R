findpolicy_MOMDP = function(initial, fs, Num_fs, file = 'output.policy'){

  ## Extract vectors from XML
  xml <- xml2::read_xml(file)
  vectors <- xml2::xml_find_all(xml, "//Vector")
  get_vector <- function(v) as.numeric(strsplit(as.character(xml2::xml_contents(v)), " ")[[1]])

  alpha <- lapply(vectors, get_vector)
  #alpha <- vectors %>% purrr::map(get_vector)

  alpha_action <- vapply(vectors, function(v) as.numeric(xml2::xml_attr(v, "action")), double(1))
  #alpha_action <- vectors %>% purrr::map_dbl(function(v) as.numeric(xml2::xml_attr(v, "action")))

  alpha_fs <- vapply(vectors, function(v) as.numeric(xml2::xml_attr(v, "obsValue")), double(1))
  #alpha_fs <- vectors %>% purrr::map_dbl(function(v) as.numeric(xml2::xml_attr(v, "obsValue")))

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
  a <- vapply(alpha_v[[fs]], function(x) initial %*% matrix(x,ncol=1), double(1))
  #a <- alpha_v[[fs]] %>% map_dbl(function(x) initial %*% matrix(x,ncol=1))

  ## Return policy of the vector which has the biggest inner product
  #alpha_action[which.max(a)]

  output = list(max(a),alpha_a[[fs]][which.max(a)],alpha_v,alpha_a)
}
